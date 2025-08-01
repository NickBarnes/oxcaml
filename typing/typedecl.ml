(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(**** Typing of type definitions ****)

open Misc
open Asttypes
open Parsetree
open Primitive
open Types
open Typetexp

module String = Misc.Stdlib.String

type native_repr_kind = Unboxed | Untagged

type jkind_sort_loc =
  | Cstr_tuple of { unboxed : bool }
  | Record of { unboxed : bool }
  | Record_unboxed_product
  | Inlined_record of { unboxed : bool }
  | Mixed_product
  | External
  | External_with_layout_poly

(* Our static analyses explore the set of type expressions "reachable"
   from a type declaration, by expansion of definitions or by the
   subterm relation (a type expression is syntactically contained
   in another). *)
type reaching_type_path = reaching_type_step list
and reaching_type_step =
  | Expands_to of type_expr * type_expr
  | Contains of type_expr * type_expr

module Mixed_product_kind = struct
  type t =
    | Record
    | Cstr_tuple
    | Cstr_record

  let to_plural_string = function
    | Record -> "records"
    | Cstr_tuple -> "constructors"
    | Cstr_record -> "inline record arguments to constructors"
end

type mixed_product_violation =
  | Runtime_support_not_enabled of Mixed_product_kind.t
  | Extension_constructor
  | Value_prefix_too_long of
      { value_prefix_len : int;
        max_value_prefix_len : int;
        mixed_product_kind : Mixed_product_kind.t;
      }
  | Insufficient_level of
      { required_layouts_level : Language_extension.maturity;
        mixed_product_kind : Mixed_product_kind.t;
      }

type bad_jkind_inference_location =
  | Check_constraints
  | Delayed_checks

type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Unboxed_mutable_label
  | Recursive_abbrev of string * Env.t * reaching_type_path
  | Cycle_in_def of string * Env.t * reaching_type_path
  | Unboxed_recursion of string * Env.t * reaching_type_path
  | Definition_mismatch of type_expr * Env.t * Includecore.type_mismatch option
  | Constraint_failed of Env.t * Errortrace.unification_error
  | Inconsistent_constraint of Env.t * Errortrace.unification_error
  | Type_clash of Env.t * Errortrace.unification_error
  | Non_regular of {
      definition: Path.t;
      used_as: type_expr;
      defined_as: type_expr;
      reaching_path: reaching_type_path;
    }
  | Null_arity_external
  | Missing_native_external
  | Unbound_type_var of type_expr * type_declaration
  | Cannot_extend_private_type of Path.t
  | Not_extensible_type of Path.t
  | Extension_mismatch of Path.t * Env.t * Includecore.type_mismatch
  | Rebind_wrong_type of
      Longident.t * Env.t * Errortrace.unification_error
  | Rebind_mismatch of Longident.t * Path.t * Path.t
  | Rebind_private of Longident.t
  | Variance of Typedecl_variance.error
  | Unavailable_type_constructor of Path.t
  | Unbound_type_var_ext of type_expr * extension_constructor
  | Val_in_structure
  | Multiple_native_repr_attributes
  | Cannot_unbox_or_untag_type of native_repr_kind
  | Deep_unbox_or_untag_attribute of native_repr_kind
  | Jkind_mismatch_of_type of type_expr * Jkind.Violation.t
  | Jkind_mismatch_of_path of Path.t * Jkind.Violation.t
  | Jkind_mismatch_due_to_bad_inference of
      type_expr * Jkind.Violation.t * bad_jkind_inference_location
  | Jkind_sort of
      { kloc : jkind_sort_loc
      ; typ : type_expr
      ; err : Jkind.Violation.t
      }
  | Jkind_empty_record
  | Non_value_in_sig of Jkind.Violation.t * string * type_expr
  | Invalid_jkind_in_block of type_expr * Jkind.Sort.Const.t * jkind_sort_loc
  | Illegal_mixed_product of mixed_product_violation
  | Separability of Typedecl_separability.error
  | Bad_unboxed_attribute of string
  | Boxed_and_unboxed
  | Nonrec_gadt
  | Invalid_private_row_declaration of type_expr
  | Local_not_enabled
  | Unexpected_layout_any_in_primitive of string
  | Useless_layout_poly
  | Zero_alloc_attr_unsupported of Builtin_attributes.zero_alloc_attribute
  | Zero_alloc_attr_non_function
  | Zero_alloc_attr_bad_user_arity
  | Invalid_reexport of
      { definition: Path.t
      ; expected: Path.t
      }
  | Non_abstract_reexport of Path.t
  | Unsafe_mode_crossing_on_invalid_type_kind
  | Illegal_baggage of jkind_l
  | No_unboxed_version of Path.t

open Typedtree

exception Error of Location.t * error

let get_unboxed_from_attributes sdecl =
  let unboxed = Builtin_attributes.has_unboxed sdecl.ptype_attributes in
  let boxed = Builtin_attributes.has_boxed sdecl.ptype_attributes in
  match boxed, unboxed with
  | true, true -> raise (Error(sdecl.ptype_loc, Boxed_and_unboxed))
  | true, false -> Some false
  | false, true -> Some true
  | false, false -> None

(* [make_params] creates sort variables - these can be defaulted away (as in
   transl_type_decl) or unified with existing sort-variable-free types (as in
   transl_with_constraint). *)
let make_params env path params =
  TyVarEnv.reset (); (* [transl_type_param] binds type variables *)
  let make_param (sty, v) =
    (* Our choice for now is that if you want a parameter of jkind any, you have
       to ask for it with an annotation.  Some restriction here seems necessary
       for backwards compatibility (e.g., we wouldn't want [type 'a id = 'a] to
       have jkind any).  But it might be possible to infer [any] in some
       cases. *)
    let jkind =
      Jkind.of_new_legacy_sort ~why:(Unannotated_type_parameter path)
    in
    try
      (transl_type_param env path jkind sty, v)
    with Already_bound ->
      raise(Error(sty.ptyp_loc, Repeated_parameter))
  in
    List.map make_param params

(* Enter all declared types in the environment as abstract types *)

let add_type ~check ?shape id decl env =
  Builtin_attributes.warning_scope ~ppwarning:false decl.type_attributes
    (fun () -> Env.add_type ~check ?shape id decl env)

(* Add a dummy type declaration to the environment, with the given arity.
   The [type_kind] is [Type_abstract], but there is a generic [type_manifest]
   for abbreviations, to allow polymorphic expansion, except if
   [abstract_abbrevs] is given along with a reason for not allowing expansion.
   This function is only used in [transl_type_decl]. *)
let enter_type ?abstract_abbrevs rec_flag env sdecl (id, uid) =
  let needed =
    match rec_flag with
    | Asttypes.Nonrecursive ->
        begin match sdecl.ptype_kind with
        | Ptype_variant scds ->
            List.iter (fun cd ->
              if cd.pcd_res <> None then raise (Error(cd.pcd_loc, Nonrec_gadt)))
              scds
        | _ -> ()
        end;
        Btype.is_row_name (Ident.name id)
    | Asttypes.Recursive -> true
  in
  if not needed then env else
  let arity = List.length sdecl.ptype_params in
  let path = Path.Pident id in
  let any = Jkind.Builtin.any ~why:Initial_typedecl_env in

  (* There is some trickiness going on here with the jkind.  It expands on an
     old trick used in the manifest of [decl] below.

     Consider a declaration like:

        type t = foo list_of_values
        and foo = Bar

     When [enter_type] is called, we haven't yet analyzed anything about the
     manifests and kinds of the declarations, so it's natural to give [t] and
     [foo] jkind [Any].  But, while translating [t]'s manifest, we'll need to
     know [foo] has jkind [value], because it is used as the argument to
     [list_of_values]. And this check will occur before we've looked at [foo] at
     all.

     One can imagine solutions, like estimating the jkind based on the kind
     (tricky for unboxed) or parameterizing the type_expr translation with an
     option to not do full jkind checking in some cases and fix it up later
     (ugly).

     Instead, we build on an old trick that is used to handle constraints.
     Consider declarations like:

       type 'a t = 'a constraint 'a = ('b * 'c)

       type s = r t
       and r = int * string

     Here we face a similar problem in the context of constraints.  While
     translating [s]'s manifest (which is [r t]), we'll need to know that [t]'s
     constraint is satisfied (i.e., that [r] is a tuple).  But we don't know
     anything about [r] yet!

     The solution, in three parts:
     1) [enter_type], here, is used to construct [temp_env], an environment
        where we set the manifest of recursively defined things like [s]
        and [t] to just be a fresh type variable.
     2) [transl_declaration] checks constraints in [temp_env].  This succeeds,
        because [r]'s manifest is a variable and therefore unifies with
        ['b * 'c].
     3) After we've built the real environment with the actual manifests
        ([new_env] in [transl_type_decl]), the function [update_type] checks
        that the manifests from the old environment (here containing the
        information that [r] must be some pair to satisfy the constraint) are
        unified with the manifests from the new environment, ensuring the actual
        definitions satisfy those constraints.

     If [r] were, e.g., defined to be [int list], step 3 would fail.

     To handle the original jkind example, we piggyback off that approach - the
     jkind of the variable put in manifests here is updated when constraints
     are checked and then unified with the real manifest and checked against the
     kind. *)
  let type_jkind =
    Jkind.of_type_decl_default
      ~context:(Type_declaration path)
      (* CR layouts v2.8: This next line is truly terrible. But I think it's OK
         for now: it will mean that any [with] constraints get interpreted to
         mean that the thing does not cross that mode. That's OK: the jkind
         produced here can be an overapproximation of the correct jkind (note
         that [any] is the default).  Indeed the only reason (I think) we need a
         non-[any] jkind here is to produce better error messages.

         Doing better here will be annoying, because a type is in scope in its
         own jkind... and yet we don't have an env that we can use at this
         point. I think probably the solution will be to have
         [Jkind.of_type_decl_default] just return [max] every time it sees a
         [with]-kind... which basically just does this [type_exn] trick but much
         more sanely. *)
      ~transl_type:(fun _ -> Predef.type_exn)
      ~default:(Jkind.disallow_right any)
      sdecl
  in
  let abstract_source, type_manifest, unboxed_type_manifest =
    match sdecl.ptype_manifest, abstract_abbrevs with
    (* Make a manifest with an unrestricted type variable. This type variable
       essentially collects constraints that arise from the usage of the
       type being constructed. Nothing is gained by using the jkind from
       an annotation here, and doing so with separated left- and right-jkinds
       is hard to do. *)
    | None, _ | Some _, None ->
      Definition, Some (Ctype.newvar any), Some (Ctype.newvar any)
    | Some _, Some reason -> reason, None, None
in
  let type_params =
    List.map (fun (param, _) ->
        let name = get_type_param_name param in
        let jkind = get_type_param_jkind path param in
        Btype.newgenvar ?name jkind)
      sdecl.ptype_params
  in
  (* In the temporary environment, all types get an unboxed version.
     See Note [Typechecking unboxed versions of types]. *)
  let type_unboxed_version =
    Some { type_params;
      type_arity = arity;
      type_kind = Type_abstract abstract_source;
      type_jkind;
      type_private = sdecl.ptype_private;
      type_manifest = unboxed_type_manifest;
      type_variance = Variance.unknown_signature ~injective:false ~arity;
      type_separability = Types.Separability.default_signature ~arity;
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = sdecl.ptype_loc;
      type_attributes = sdecl.ptype_attributes;
      type_unboxed_default = false;
      type_uid = Uid.unboxed_version uid;
      type_unboxed_version = None;
    }
  in
  let decl =
    { type_params;
      type_arity = arity;
      type_kind = Type_abstract abstract_source;
      type_jkind;
      type_private = sdecl.ptype_private;
      type_manifest;
      type_variance = Variance.unknown_signature ~injective:false ~arity;
      type_separability = Types.Separability.default_signature ~arity;
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = sdecl.ptype_loc;
      type_attributes = sdecl.ptype_attributes;
      type_unboxed_default = false;
      type_uid = uid;
      type_unboxed_version;
    }
  in
  add_type ~check:true id decl env

(* nroberts: The below [update_type] is deleted upstream in
   https://github.com/ocaml/ocaml/pull/12180 to stop ocamlc from looping on some
   type constraints. Our internal version records jkind constraints, and so we
   cannot delete it. We haven't separately implemented a fix for ocamlc looping,
   so we probably have the same issue described in that PR, but users haven't
   reported it.
*)
(* [update_type] performs step 3 of the process described in the comment in
   [enter_type]: We unify the manifest of each type with the definition of that
   variable in [temp_env], which contains any requirements on the type implied
   by its use in other mutually defined types.

   In particular, we want to ensure that the manifest of this type has a jkind
   compatible with its uses in mutually defined types.  One subtlety is that we
   don't actually perform those jkind checks here - we use
   [Ctype.unify_delaying_jkind_checks] to record any needed jkind checks, but
   don't perform them until slightly later in [transl_type_decl].

   The reason for this delay is ill-formed, circular types.  These haven't been
   ruled out yet, and as a result jkind checking can fall into an infinite loop
   where jkind checking expands types, and these type expansions in subst
   trigger jkind checks that trigger type expansions that trigger jkind checks
   that...  These circular types are ruled out just after [update_type] in
   [transl_type_decl], and then we perform the delayed checks.
*)
let update_type temp_env env id loc =
  let path = Path.Pident id in
  let decl = Env.find_type path temp_env in
  try
    let checks =
      match decl.type_manifest with
      | Some ty ->
        Ctype.unify_delaying_jkind_checks
          env (Ctype.newconstr path decl.type_params) ty
      | None -> Misc.fatal_error "Typedecl.update_type"
    in
    match decl.type_unboxed_version with
    | None ->
      checks
    | Some { type_manifest = Some ty; type_params; _ } ->
      let checks_from_unboxed_version =
        Ctype.unify_delaying_jkind_checks env
          (Ctype.newconstr (Path.unboxed_version path) type_params) ty
      in
      checks @ checks_from_unboxed_version
    | Some { type_manifest = None; _ } ->
      Misc.fatal_error "Typedecl.update_type"
  with Ctype.Unify err ->
    raise (Error(loc, Type_clash (env, err)))

(* Determine if a type's values are represented by floats at run-time. *)
(* CR layouts v2.5: Should we check for unboxed float here? Is a record with all
   unboxed floats the same as a float record?

   reisenberg: Yes. And actually a record mixing floats and unboxed floats is
   also a float-record, and should be made to work. We'll have to make sure to
   add the boxing operations in the right spot at projections, but that should
   be possible.
*)
let is_float env ty =
  match get_desc (Ctype.get_unboxed_type_approximation env ty).ty with
    Tconstr(p, _, _) -> Path.same p Predef.path_float
  | _ -> false

(* Determine if a type definition defines a fixed type. (PW) *)
let is_fixed_type sd =
  let rec has_row_var sty =
    match sty.ptyp_desc with
      (* CR layouts upstreaming: The Ptyp_alias case also covers the case for a
         jkind annotation, conveniently. When upstreaming jkinds, this
         function will need a case for jkind-annotation aliases. *)
      Ptyp_alias (sty, _jkind, _) -> has_row_var sty
    | Ptyp_class _
    | Ptyp_object (_, Open)
    | Ptyp_variant (_, Open, _)
    | Ptyp_variant (_, Closed, Some _) -> true
    | _ -> false
  in
  match sd.ptype_manifest with
    None -> false
  | Some sty ->
      sd.ptype_kind = Ptype_abstract &&
      sd.ptype_private = Private &&
      has_row_var sty

(* Set the row variable to a fixed type in a private row type declaration.
   (e.g. [ type t = private [< `A | `B ] ] or [type u = private < .. > ])
   Require [is_fixed_type decl] as a precondition
*)
let set_private_row env loc p decl =
  let tm =
    match decl.type_manifest with
      None -> assert false
    | Some t -> Ctype.expand_head env t
  in
  let rv =
    match get_desc tm with
      Tvariant row ->
        let Row {fields; more; closed; name} = row_repr row in
        set_type_desc tm
          (Tvariant (create_row ~fields ~more ~closed ~name
                       ~fixed:(Some Fixed_private)));
        if Btype.static_row row then
          (* the syntax hinted at the existence of a row variable,
             but there is in fact no row variable to make private, e.g.
             [ type t = private [< `A > `A] ] *)
          raise (Error(loc, Invalid_private_row_declaration tm))
        else more
    | Tobject (ty, _) ->
        let r = snd (Ctype.flatten_fields ty) in
        if not (Btype.is_Tvar r) then
          (* a syntactically open object was closed by a constraint *)
          raise (Error(loc, Invalid_private_row_declaration tm));
        r
    | _ -> assert false
  in
  set_type_desc rv (Tconstr (p, decl.type_params, ref Mnil))

(* Makes sure a type is representable. When called with a type variable, will
   lower [any] to a sort variable if [allow_unboxed = true], and to [value]
   if [allow_unboxed = false]. *)
(* CR layouts: Many places where [check_representable] is called in this file
   should be replaced with checks at the places where values of those types are
   constructed.  We've been conservative here in the first version. This is the
   same issue as with arrows. *)
let check_representable ~why env loc kloc typ =
  match Ctype.type_sort ~why ~fixed:false env typ with
  | Ok _ -> ()
  | Error err -> raise (Error (loc,Jkind_sort {kloc; typ; err}))

let transl_labels (type rep) ~(record_form : rep record_form) ~new_var_jkind
      env univars closed lbls kloc =
  assert (lbls <> []);
  let all_labels = ref String.Set.empty in
  List.iter
    (fun {pld_name = {txt=name; loc}} ->
       if String.Set.mem name !all_labels then
         raise(Error(loc, Duplicate_label name));
       all_labels := String.Set.add name !all_labels)
    lbls;
  let mk {pld_name=name;pld_mutable=mut;pld_modalities=modalities;
          pld_type=arg;pld_loc=loc;pld_attributes=attrs} =
    Builtin_attributes.warning_scope attrs
      (fun () ->
         let mut : mutability =
          match mut with
          | Immutable -> Immutable
          | Mutable ->
              match record_form with
              | Legacy -> Mutable Mode.Value.Comonadic.legacy
              | Unboxed_product -> raise(Error(loc, Unboxed_mutable_label))
         in
         let modalities =
          Typemode.transl_modalities ~maturity:Stable mut modalities
         in
         let arg = Ast_helper.Typ.force_poly arg in
         let cty = transl_simple_type ~new_var_jkind env ?univars ~closed Mode.Alloc.Const.legacy arg in
         {ld_id = Ident.create_local name.txt;
          ld_name = name;
          ld_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
          ld_mutable = mut;
          ld_modalities = modalities;
          ld_type = cty; ld_loc = loc; ld_attributes = attrs}
      )
  in
  let lbls = List.map mk lbls in
  let lbls' =
    List.map
      (fun ld ->
         let ty = ld.ld_type.ctyp_type in
         let ty = match get_desc ty with Tpoly(t,[]) -> t | _ -> ty in
         check_representable ~why:(Label_declaration ld.ld_id)
           env ld.ld_loc kloc ty;
         {Types.ld_id = ld.ld_id;
          ld_mutable = ld.ld_mutable;
          ld_modalities = ld.ld_modalities;
          ld_sort = Jkind.Sort.Const.void;
            (* Updated by [update_label_sorts] *)
          ld_type = ty;
          ld_loc = ld.ld_loc;
          ld_attributes = ld.ld_attributes;
          ld_uid = ld.ld_uid;
         }
      )
      lbls in
  lbls, lbls'

let transl_types_gf ~new_var_jkind env loc univars closed cal kloc =
  let mk arg =
    let cty =
      transl_simple_type ~new_var_jkind env ?univars ~closed
        Mode.Alloc.Const.legacy arg.pca_type
    in
    let gf =
      Typemode.transl_modalities ~maturity:Stable Immutable arg.pca_modalities
    in
    {ca_modalities = gf; ca_type = cty; ca_loc = arg.pca_loc}
  in
  let tyl_gfl = List.map mk cal in
  let tyl_gfl' = List.mapi (fun idx (ca : Typedtree.constructor_argument) ->
    check_representable ~why:(Constructor_declaration idx)
      env loc kloc ca.ca_type.ctyp_type;
    {
      Types.ca_modalities = ca.ca_modalities;
      ca_loc = ca.ca_loc;
      ca_type = ca.ca_type.ctyp_type;
      ca_sort = Jkind.Sort.Const.void;
            (* Updated by [update_constructor_arguments_sorts] *)
    }) tyl_gfl
  in
  tyl_gfl, tyl_gfl'

let transl_constructor_arguments ~new_var_jkind ~unboxed
  env loc univars closed = function
  | Pcstr_tuple l ->
      let flds, flds' =
        transl_types_gf ~new_var_jkind
          env loc univars closed l (Cstr_tuple { unboxed })
      in
      Types.Cstr_tuple flds', Cstr_tuple flds
  | Pcstr_record l ->
      let lbls, lbls' =
        transl_labels ~record_form:Legacy ~new_var_jkind
          env univars closed l (Inlined_record { unboxed })
      in
      Types.Cstr_record lbls',
      Cstr_record lbls

(* Note that [make_constructor] does not fill in the [ld_jkind] field of any
   computed record types, because it's called too early in the translation of a
   type declaration to compute accurate jkinds in the presence of recursively
   defined types. It is updated later by [update_constructor_arguments_sorts]
*)
let make_constructor
      env loc ~cstr_path ~type_path ~unboxed type_params svars
      sargs sret_type =
  let tvars = List.map (fun (v, l) -> v.txt, l) svars in
  match sret_type with
  | None ->
      let args, targs =
        transl_constructor_arguments ~new_var_jkind:Any ~unboxed
          env loc None true sargs
      in
        tvars, targs, None, args, None
  | Some sret_type ->
      (* if it's a generalized constructor we must first narrow and
         then widen so as to not introduce any new constraints *)
      (* narrow and widen are now invoked through with_local_scope *)
      TyVarEnv.with_local_scope begin fun () ->
      let closed =
        match svars with
        | [] -> false
        | _ -> true
      in
      let targs, tret_type, args, ret_type, _univars =
        Ctype.with_local_level_if closed begin fun () ->
          TyVarEnv.reset ();
          let univar_list =
            TyVarEnv.make_poly_univars_jkinds
              ~context:(fun v -> Constructor_type_parameter (cstr_path, v))
              svars
          in
          let univars = if closed then Some univar_list else None in
          let args, targs =
            transl_constructor_arguments ~new_var_jkind:Sort ~unboxed
              env loc univars closed sargs
          in
          let tret_type =
            transl_simple_type ~new_var_jkind:Sort env ?univars ~closed Mode.Alloc.Const.legacy
              sret_type
          in
          let ret_type = tret_type.ctyp_type in
          (* TODO add back type_path as a parameter ? *)
          begin match get_desc ret_type with
          | Tconstr (p', _, _) when Path.same type_path p' -> ()
          | _ ->
              let trace =
                (* Expansion is not helpful here -- the restriction on GADT
                   return types is purely syntactic.  (In the worst case,
                   expansion produces gibberish.) *)
                [Ctype.unexpanded_diff
                   ~got:ret_type
                   ~expected:(Ctype.newconstr type_path type_params)]
              in
              raise (Error(sret_type.ptyp_loc,
                           Constraint_failed(
                           env, Errortrace.unification_error ~trace)))
          end;
          (targs, tret_type, args, ret_type, univar_list)
        end
        ~post: begin fun (_, _, args, ret_type, univars) ->
          Btype.iter_type_expr_cstr_args Ctype.generalize args;
          Ctype.generalize ret_type;
          let _vars = TyVarEnv.instance_poly_univars env loc univars in
          let set_level t = Ctype.enforce_current_level env t in
          Btype.iter_type_expr_cstr_args set_level args;
          set_level ret_type;
        end
      in
      tvars, targs, Some tret_type, args, Some ret_type
      end

let verify_unboxed_attr unboxed_attr sdecl =
  begin match unboxed_attr with
  | (None | Some false) -> ()
  | Some true ->
    let bad msg = raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute msg)) in
    match sdecl.ptype_kind with
    | Ptype_abstract    -> bad "it is abstract"
    | Ptype_open        -> bad "extensible variant types cannot be unboxed"
    | Ptype_record fields -> begin match fields with
        | [] -> bad "it has no fields"
        | _::_::_ -> bad "it has more than one field"
        | [{pld_mutable = Mutable}] -> bad "it is mutable"
        | [{pld_mutable = Immutable}] -> ()
      end
    | Ptype_record_unboxed_product _ ->
        bad "[@@unboxed] may not be used on unboxed records"
    | Ptype_variant constructors -> begin match constructors with
        | [] -> bad "it has no constructor"
        | (_::_::_) -> bad "it has more than one constructor"
        | [c] -> begin match c.pcd_args with
            | Pcstr_tuple [] ->
                bad "its constructor has no argument"
            | Pcstr_tuple (_::_::_) ->
                bad "its constructor has more than one argument"
            | Pcstr_tuple [_]  ->
                ()
            | Pcstr_record [] ->
                bad "its constructor has no fields"
            | Pcstr_record (_::_::_) ->
                bad "its constructor has more than one field"
            | Pcstr_record [{pld_mutable = Mutable}] ->
                bad "it is mutable"
            | Pcstr_record [{pld_mutable = Immutable}] ->
                ()
          end
      end
  end

(* Note [Default jkinds in transl_declaration]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   For every type declaration we create in transl_declaration, we must
   choose the jkind to use in the [type_jkind] field. Note that choices
   2 and 3 below consult the jkinds of other types. In the case that these
   types are declared in the same mutually recursive group, those jkinds
   will be approximations; see the comments on [enter_type].

   1. If there is a jkind annotation, use that. We might later compute a more
      precise jkind for the type (e.g. [type t : value = int] or [type t :
      value = A | B | C]); this will be updated in [update_decl_jkind] (updates
      from the kind) or [narrow_to_manifest_jkind] (updates from the manifest),
      which also ensures that the updated jkind is a subjkind of the annotated
      jkind.

   2. If there is no annotation but there is a manifest, use the jkind
      of the manifest. This gets improved in [narrow_to_manifest_jkind], after
      the manifest jkind might be more accurate.

   3. If there is no annotation and no manifest, the default jkind
      depends on the kind:

      - Abstract types: In this case, we have a fully abstract type declaration,
        like [type t]. We wish to default these to have jkind [value] for
        backward compatibility.

      - [@@unboxed] records and variants: We use [any] as the default.
        This default gets updated in [update_decl_jkind], when we can
        safely look up the jkind of the field. Recursive uses
        of the unboxed type are OK, because [update_decl_jkind] uses
        [Ctype.type_jkind], which looks through unboxed types (and thus
        the choice of [any] is not observed on recursive occurrences).

      - Other records and variants: The jkind of these depends on the jkinds
        of their fields: an enumeration variant is an [immediate], and someday
        (* CR layouts v5: today is the someday! *) we will allow records
        comprising only [void]s, which will also be [immediate].

        So we choose a default of [value], which gets updated in
        [update_decl_jkind]. This default choice does get used when updating
        the jkinds of other types that (recursively) mention the current type,
        but that's OK: the update in [update_decl_jkind] can only change a
        [value] to become [immediate], and yet that change can never affect
        the decision of whether an outer record/variant is a [value] or
        [immediate] (only choices of [void] can do that).

        (Again, any unboxed records/variants are looked through by
        [type_jkind], so a void one of those is OK.)

        It is tempting to use [any] as the default here, but that causes
        trouble around recursive occurrences in [update_decl_jkind].

      - Extensible variants: These really are [value]s, so we just use
        that as the default.

   The jkinds in type declarations are always just upper bounds, as
   we see in this example:

   {[
     type t7 = A | B | C | D of t7_void
     and t7_2 = { x : t7 } [@@unboxed]
     and t7_void [@@void]

     type t7_3 = t7_2 [@@immediate]
   ]}

   The proper jkind of [t7] is [immediate], but that's hard to know. Because
   [t7] has no jkind annotation and no manifest, it gets a default jkind of
   [value]. [t7_2] gets a default of [any]. We update [t7]'s jkind to be
   [immediate] in [update_decl_jkind]. But when updating [t7_2]'s jkind, we use
   the *original, default* jkind for [t7]: [value]. This means that the jkind
   recorded for [t7_2] is actually [value]. The program above is still accepted,
   because the jkind check in [narrow_to_manifest_jkind] uses [type_jkind],
   which looks through unboxed types. So it's all OK for users, but it's
   unfortunate that the stored jkind on [t7_2] is imprecise.

   The way this interacts with checking of with-bounds is somewhat subtle and complex.
   With-bounds for mutually recursive type declarations need to be normalized and checked
   in a pass /after/ computing the the proper (best) jkinds for all the types and storing
   them in the environment, so that they can be queried during normalization. But it's
   important that we call [Jkind.Layout.sub] on each type /eagerly/, so that its sort
   variables (which might be referenced from the jkinds of other types) get filled in with
   the right sort. So we do that early, in [update_decl_jkind], then check the full jkind
   against the dummy jkind later, after normalizing in [transl_type_decl].

   (* CR layouts: see if we can do better here. *)
*)


let shape_map_labels =
  List.fold_left (fun map { Types.ld_id; ld_uid; _} ->
    Shape.Map.add_label map ld_id ld_uid)
    Shape.Map.empty

let shape_map_unboxed_labels =
  List.fold_left (fun map { Types.ld_id; ld_uid; _} ->
    Shape.Map.add_unboxed_label map ld_id ld_uid)
    Shape.Map.empty

let shape_map_cstrs =
  List.fold_left (fun map { Types.cd_id; cd_uid; cd_args; _ } ->
    let cstr_shape_map =
      let label_decls =
        match cd_args with
        | Cstr_tuple _ -> []
        | Cstr_record ldecls -> ldecls
      in
      shape_map_labels label_decls
    in
    Shape.Map.add_constr map cd_id
      @@ Shape.str ~uid:cd_uid cstr_shape_map)
    (Shape.Map.empty)

let transl_declaration env sdecl (id, uid) =
  (* Bind type parameters *)
  Ctype.with_local_level begin fun () ->
  TyVarEnv.reset();
  let path = Path.Pident id in
  let tparams = make_params env path sdecl.ptype_params in
  let params = List.map (fun (cty, _) -> cty.ctyp_type) tparams in
  let cstrs = List.map
    (fun (sty, sty', loc) ->
      transl_simple_type ~new_var_jkind:Any env ~closed:false Mode.Alloc.Const.legacy sty,
      transl_simple_type ~new_var_jkind:Sort env ~closed:false Mode.Alloc.Const.legacy sty', loc)
    sdecl.ptype_cstrs
  in
  let unboxed_attr = get_unboxed_from_attributes sdecl in
  let unbox, unboxed_default =
    match sdecl.ptype_kind with
    | Ptype_variant [{pcd_args = Pcstr_tuple [_]; _}]
    | Ptype_variant [{pcd_args = Pcstr_record [{pld_mutable=Immutable; _}]; _}]
    | Ptype_record [{pld_mutable=Immutable; _}] ->
      Option.value unboxed_attr ~default:!Clflags.unboxed_types,
      Option.is_none unboxed_attr
    | Ptype_record_unboxed_product _ -> false, false
    | _ -> false, false (* Not unboxable, mark as boxed *)
  in
  verify_unboxed_attr unboxed_attr sdecl;
  (* CR layouts v2.8: This next call to [transl_simple_type] probably can loop
     because it will do perhaps-circular jkind checks. But actually I think the
     same problem exists in e.g. record fields. We should probably look into this. *)
  let transl_type sty =
    (* CR layouts v2.8: The [~new_var_jkind:Any] is weird. The type is closed,
       and so there shouldn't be any new vars. Investigate. *)
    let cty =
      Ctype.with_local_level begin fun () ->
        Typetexp.transl_simple_type env ~new_var_jkind:Any
          ~closed:true Mode.Alloc.Const.legacy sty
      end
      (* This call to [generalize_structure] is necessary so that copying
         during instantiation traverses inside of any type constructors in the
         [with]-bound. It's also necessary because the variables here are at
         generic level, and so any containers of them should be, too! *)
      ~post:(fun cty -> Ctype.generalize_structure cty.ctyp_type)
    in
    cty.ctyp_type  (* CR layouts v2.8: Do this more efficiently. Or probably
                      add with-kinds to Typedtree. *)
  in
  let jkind_from_annotation, jkind_annotation =
    match Jkind.of_type_decl ~context:(Type_declaration path) ~transl_type sdecl with
    | Some (jkind, annot) ->
        Some jkind, annot
    | None -> None, None
  in
  let (tman, man) = match sdecl.ptype_manifest with
      None -> None, None
    | Some sty ->
      let no_row = not (is_fixed_type sdecl) in
      let cty = transl_simple_type ~new_var_jkind:Any env ~closed:no_row Mode.Alloc.Const.legacy sty in
      Some cty, Some cty.ctyp_type
  in
  (* jkind_default is the jkind to use for now as the type_jkind when there
     is no annotation and no manifest.
     See Note [Default jkinds in transl_declaration].
  *)
  let (tkind, kind, jkind_default) =
    match sdecl.ptype_kind with
      (* CR layouts v3.5: this is a hack to allow re-exporting the definition
         of ['a or_null], including constructors, even if one can't define
         ['a or_null] "honestly", with tools available to users.

         Remove when we allow users to define their own null constructors.
      *)
      | Ptype_abstract when
        Builtin_attributes.has_or_null_reexport sdecl.ptype_attributes ->
          let param =
            (* We require users to define ['a t = 'a or_null]. Manifest
               must be set to [or_null] so typechecking stays correct. *)
            let ty = Option.map (Ctype.expand_head env) man in
            match Option.map get_desc ty with
            | Some (Tconstr(path, [param], _))
              when Path.same path Predef.path_or_null -> param
            | Some _ | None -> raise (Error (sdecl.ptype_loc, Invalid_reexport
              { definition = path; expected = Predef.path_or_null }))
          in
          let type_kind = Predef.or_null_kind param in
          let jkind = Predef.or_null_jkind param in
          Ttype_abstract, type_kind, jkind
      | (Ptype_variant _ | Ptype_record _ | Ptype_record_unboxed_product _
        | Ptype_open)
        when Builtin_attributes.has_or_null_reexport sdecl.ptype_attributes ->
        raise (Error (sdecl.ptype_loc, Non_abstract_reexport path))
      | Ptype_abstract ->
        Ttype_abstract, Type_abstract Definition,
        Jkind.Builtin.value ~why:Default_type_jkind
      | Ptype_variant scstrs ->
        if List.exists (fun cstr -> cstr.pcd_res <> None) scstrs then begin
          match cstrs with
            [] -> ()
          | (_,_,loc)::_ ->
              Location.prerr_warning loc Warnings.Constraint_on_gadt
        end;
        let all_constrs = ref String.Set.empty in
        List.iter
          (fun {pcd_name = {txt = name}} ->
            if String.Set.mem name !all_constrs then
              raise(Error(sdecl.ptype_loc, Duplicate_constructor name));
            all_constrs := String.Set.add name !all_constrs)
          scstrs;
        if List.length
            (List.filter (fun cd -> cd.pcd_args <> Pcstr_tuple []) scstrs)
           > (Config.max_tag + 1) then
          raise(Error(sdecl.ptype_loc, Too_many_constructors));
        let make_cstr scstr =
          let name = Ident.create_local scstr.pcd_name.txt in
          let attributes = scstr.pcd_attributes in
          let tvars, targs, tret_type, args, ret_type =
            make_constructor ~unboxed:unbox env scstr.pcd_loc
              ~cstr_path:(Path.Pident name) ~type_path:path params
              scstr.pcd_vars scstr.pcd_args scstr.pcd_res
          in
          let tcstr =
            { cd_id = name;
              cd_name = scstr.pcd_name;
              cd_vars = tvars;
              cd_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
              cd_args = targs;
              cd_res = tret_type;
              cd_loc = scstr.pcd_loc;
              cd_attributes = attributes }
          in
          let cstr =
            { Types.cd_id = name;
              cd_args = args;
              cd_res = ret_type;
              cd_loc = scstr.pcd_loc;
              cd_attributes = attributes;
              cd_uid = tcstr.cd_uid }
          in
            tcstr, cstr
        in
        let make_cstr scstr =
          Builtin_attributes.warning_scope scstr.pcd_attributes
            (fun () -> make_cstr scstr)
        in
        let tcstrs, cstrs = List.split (List.map make_cstr scstrs) in
        let rep, jkind =
          if unbox then
            Variant_unboxed, Jkind.of_new_sort ~why:Old_style_unboxed_type
          else
            (* We mark all arg sorts "void" here.  They are updated later,
               after the circular type checks make it safe to check sorts.
               Likewise, [Constructor_uniform_value] is potentially wrong
               and will be updated later.
            *)
            Variant_boxed (
              Array.map
                (fun cstr ->
                   let sorts =
                     match Types.(cstr.cd_args) with
                     | Cstr_tuple args ->
                       Array.make (List.length args) Jkind.Sort.Const.void
                     | Cstr_record _ -> [| Jkind.Sort.Const.value |]
                   in
                   Constructor_uniform_value, sorts)
                (Array.of_list cstrs)
            ),
          Jkind.for_non_float ~why:Boxed_variant
        in
          Ttype_variant tcstrs, Type_variant (cstrs, rep, None), jkind
      | Ptype_record lbls ->
          let lbls, lbls' =
            transl_labels ~record_form:Legacy ~new_var_jkind:Any
              env None true lbls (Record { unboxed = unbox })
          in
          let rep, jkind =
            if unbox then
              Record_unboxed,
              Jkind.of_new_sort ~why:Old_style_unboxed_type
            else
            (* Note this is inaccurate, using `Record_boxed` in cases where the
               correct representation is [Record_float], [Record_ufloat], or
               [Record_mixed].  Those cases are fixed up after we can get
               accurate sorts for the fields, in [update_decl_jkind]. *)
              Record_boxed (Array.make (List.length lbls) Jkind.Sort.Const.void),
              Jkind.for_non_float ~why:Boxed_record
          in
          Ttype_record lbls, Type_record(lbls', rep, None), jkind
      | Ptype_record_unboxed_product lbls ->
          Language_extension.assert_enabled ~loc:sdecl.ptype_loc Layouts
            Language_extension.Stable;
          let lbls, lbls' =
            transl_labels ~record_form:Unboxed_product ~new_var_jkind:Any
              env None true lbls Record_unboxed_product
          in
          (* The jkinds below, and the ones in [lbls], are dummy jkinds which
             are replaced and made to correspond to each other in
             [update_decl_jkind]. *)
          let jkind =
            Jkind.Builtin.product_of_sorts ~why:Unboxed_record
              (List.length lbls)
          in
          Ttype_record_unboxed_product lbls,
          Type_record_unboxed_product(lbls', Record_unboxed_product, None), jkind
      | Ptype_open ->
        Ttype_open, Type_open,
        Jkind.for_non_float ~why:Extensible_variant
      in
    let jkind =
    (* - If there's an annotation, we use that. It's checked against a kind in
         [update_decl_jkind] and the manifest in [narrow_to_manifest_jkind].
         Both of those functions update the [type_jkind] field in the
         [type_declaration] as appropriate.
       - If there's no annotation but there is a manifest, just use [any].
         This will get updated to the manifest's jkind in
         [narrow_to_manifest_jkind].
       - If there's no annotation and no manifest, we fill in with the
         default calculated above here. It will get updated in
         [update_decl_jkind]. See Note [Default jkinds in transl_declaration].
    *)
      match jkind_from_annotation, man with
      | Some annot, _ -> annot
      | None, Some _ -> Jkind.Builtin.any ~why:Initial_typedecl_env
      | None, None -> jkind_default
    in
    let jkind =
      (* Hack: unboxed records are given a product-of-[any]s layout
         when they would otherwise be given [any].

         This allows [estimate_type_jkind] to give an estimate that's
         just barely good enough, such that [constain_type_jkind] can always
         decompose the product of [any]s and recurse on the labels.
         See https://github.com/oxcaml/oxcaml/pull/3399. *)
      match kind with
      | Type_record_unboxed_product _ ->
        begin match Jkind.get_layout jkind with
        | Some Any ->
          (* [jkind_default] has just what we need here *)
          Jkind.set_layout jkind (Jkind.extract_layout jkind_default)
        | _ -> jkind
        end
      | Type_abstract _ | Type_variant _ | Type_record _
      | Type_open -> jkind
    in
    let arity = List.length params in
    let decl =
      { type_params = params;
        type_arity = arity;
        type_kind = kind;
        type_jkind = jkind;
        type_private = sdecl.ptype_private;
        type_manifest = man;
        type_variance = Variance.unknown_signature ~injective:false ~arity;
        type_separability = Types.Separability.default_signature ~arity;
        type_is_newtype = false;
        type_expansion_scope = Btype.lowest_level;
        type_loc = sdecl.ptype_loc;
        type_attributes = sdecl.ptype_attributes;
        type_unboxed_default = unboxed_default;
        type_uid = uid;
        type_unboxed_version = None;
        (* Unboxed versions are computed after all declarations have been
           translated, in [derive_unboxed_versions] *)
      } in
  (* Check constraints *)
    List.iter
      (fun (cty, cty', loc) ->
        let ty = cty.ctyp_type in
        let ty' = cty'.ctyp_type in
        try Ctype.unify env ty ty' with Ctype.Unify err ->
          raise(Error(loc, Inconsistent_constraint (env, err))))
      cstrs;
  (* Add abstract row *)
    if is_fixed_type sdecl then begin
      let p, _ =
        try Env.find_type_by_name
              (Longident.Lident(Ident.name id ^ "#row")) env
        with Not_found -> assert false
      in
      set_private_row env sdecl.ptype_loc p decl
    end;
    let decl =
      {
        typ_id = id;
        typ_name = sdecl.ptype_name;
        typ_params = tparams;
        typ_type = decl;
        typ_cstrs = cstrs;
        typ_loc = sdecl.ptype_loc;
        typ_manifest = tman;
        typ_kind = tkind;
        typ_private = sdecl.ptype_private;
        typ_attributes = sdecl.ptype_attributes;
        typ_jkind_annotation = jkind_annotation
      }
    in
    let typ_shape =
      let uid = decl.typ_type.type_uid in
      match decl.typ_type.type_kind with
      | Type_variant (cstrs, _, _) -> Shape.str ~uid (shape_map_cstrs cstrs)
      | Type_record (labels, _, _) ->
        Shape.str ~uid (shape_map_labels labels)
      | Type_record_unboxed_product (labels, _, _) ->
        Shape.str ~uid (shape_map_unboxed_labels labels)
      | Type_abstract _ | Type_open -> Shape.leaf uid
    in
    decl, typ_shape
  end

(* Note [Typechecking unboxed versions of types]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Unboxed versions are computed in three steps:

   1. In the temporary environment computed by [enter_type], all types get an
      unboxed version.

   2. After translating declarations, [derive_unboxed_versions] gives the
      [Record_boxed] records unboxed versions.

   3. But some of these [Record_boxed]s are a lie, and become
      [Record_float]/[Record_ufloat]/[Record_mixed] after [update_decls_jkind].
      As float records should not end up with unboxed versions, we then remove
      theirs in [remove_unboxed_versions].

   After steps 2 and 3, the set of unboxed versions decreases, so we check for
   newly-unbound unboxed paths with [check_unboxed_paths].
*)

(* Record declarations with representation [Record_boxed] get an implicit
   unboxed record stored in [type_unboxed_version]. If that record is also an
   alias, so is its stored unboxed version. E.g. [type t = r = { i : int }]'s
   unboxed version gets kind [#{ i : int}] and manifest [r#].

   Note that all aliases of types with unboxed versions, with an abstract kind,
   also have unboxed versions, but these aren't stored in
   [type_unboxed_version].
*)
let record_gets_unboxed_version = function
  | Record_unboxed | Record_inlined _ | Record_float | Record_ufloat -> false
  | Record_boxed _ -> true
  | Record_mixed shape ->
    let rec shape_has_float_boxed shape =
      Array.exists
        (fun (kind : mixed_block_element) ->
          match kind with
          | Value | Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64
          | Vec128 | Vec256 | Vec512 | Word | Void -> false
          | Float_boxed -> true
          | Product shape -> shape_has_float_boxed shape)
        shape
    in
    not (shape_has_float_boxed shape)
let gets_unboxed_version decl =
  (* This must be kept in sync with the match in [derive_unboxed_version] *)
  match decl.type_kind with
  | Type_abstract _ | Type_open | Type_record_unboxed_product _
  | Type_variant _ -> false
  | Type_record (_, repr, _) -> record_gets_unboxed_version repr
let derive_unboxed_version env path_in_group_has_unboxed_version decl =
  (* This must be kept in sync with the match in [gets_unboxed_version] *)
  match decl.type_kind with
  | Type_abstract _ | Type_open | Type_record_unboxed_product _
  | Type_variant _ ->
    None
  | Type_record (_, repr, _) when not (record_gets_unboxed_version repr) ->
    None
  | Type_record (lbls, _, umc) ->
    let keep_attribute a =
      (* If we keep [@deprecated_mutable], then a record that aliases
         a record with a [@deprecated_mutable] label will cause two alerts,
         if both have unboxed versions (because the unboxed version is a second
         alias). *)
      not (Builtin_attributes.attr_equals_builtin a "deprecated_mutable")
    in
    let lbls_unboxed =
      List.map
        (fun (ld : Types.label_declaration) ->
            { Types.ld_id = Ident.create_local (Ident.name ld.ld_id);
            ld_mutable = Immutable;
            ld_modalities = ld.ld_modalities;
              (* Inherit modalities from the boxed version. Note that these
                  are affected by the mutability of the boxed label, even
                  though the unboxed version is always immutable. *)
            ld_sort = Jkind.Sort.Const.void;
            ld_type = ld.ld_type;
            ld_loc = ld.ld_loc;
            ld_attributes = List.filter keep_attribute ld.ld_attributes;
              (* Copy label attributes to the unboxed version *)
            ld_uid = Uid.unboxed_version ld.ld_uid;
          })
        lbls
    in
    (* CR layouts v11: update type_jkind once we have [layout_of] layouts *)
    let jkind =
      Jkind.Builtin.product_of_sorts ~why:Unboxed_record (List.length lbls) in
    let kind =
      Type_record_unboxed_product(lbls_unboxed, Record_unboxed_product, umc)
    in
    let type_manifest =
      let has_unboxed_version path =
        match Path.Map.find_opt path path_in_group_has_unboxed_version with
        | Some b -> b
        | None ->
          try Option.is_some (Env.find_type path env).type_unboxed_version with
          | Not_found -> Misc.fatal_error "Typedecl.derive_unboxed_versions"
      in
      match decl.type_manifest with
      | None -> None
      | Some ty ->
        match get_desc ty with
        | Tconstr (path, args, _) when has_unboxed_version path ->
          Some (Ctype.newconstr (Path.unboxed_version path) args)
        | _ ->
          (* We're in one of two scenarios:

             1. The manifest is a Tconstr to a type without an unboxed version.
             2. The manifest is not a Tconstr, and [check_coherence] will reject
                this declaration later.

             In both cases, we could just not give this type an unboxed version,
             but it's fine to do so, as we already give unboxed versions to
             types that don't have one (float and [@@unboxed] records), and this
             simplifies things. *)
          None
    in
    Some
      {
        type_params = decl.type_params;
        type_arity = decl.type_arity;
        type_kind = kind;
        type_jkind = jkind;
        type_private = decl.type_private;
        type_manifest;
        type_variance =
          Variance.unknown_signature ~injective:false ~arity:decl.type_arity;
        type_separability =
          Types.Separability.default_signature ~arity:decl.type_arity;
        type_is_newtype = false;
        type_expansion_scope = Btype.lowest_level;
        type_loc = decl.type_loc;
        type_attributes = decl.type_attributes;
        type_unboxed_default = false;
        type_uid = Uid.unboxed_version decl.type_uid;
        type_unboxed_version = None;
      }

let derive_unboxed_versions decls env =
  let path_in_group_has_unboxed_version =
    Path.Map.of_seq
      (List.to_seq decls |>
       Seq.map (fun (id, d) -> Path.Pident id, gets_unboxed_version d))
  in
  List.map
    (fun (id, d) ->
       let type_unboxed_version =
         derive_unboxed_version env path_in_group_has_unboxed_version d
       in
       id, { d with type_unboxed_version })
    decls

(* Removes unboxed versions from type declarations not satisfying
   [gets_unboxed_version]. In practice, it is float records that lose their
   unboxed versions. See Note [Typechecking unboxed versions of types].

   Returns new decls and paths whose unboxed versions got removed. *)
let remove_unboxed_versions decls =
  List.fold_left_map
    (fun removed (id, d) ->
      match Option.is_some d.type_unboxed_version, gets_unboxed_version d with
      | false, false | true, true -> removed, (id, d)
      | true, false ->
        Path.Set.add (Pident id) removed,
        (id, { d with type_unboxed_version = None })
      | false, true -> Misc.fatal_error "Typedecl.remove_unboxed_versions")
    Path.Set.empty decls

(* Generalize a type declaration *)

let rec generalize_decl decl =
  Option.iter generalize_decl (decl.type_unboxed_version);
  List.iter Ctype.generalize decl.type_params;
  Btype.iter_type_expr_kind Ctype.generalize decl.type_kind;
  begin match decl.type_manifest with
  | None    -> ()
  | Some ty -> Ctype.generalize ty
  end

(* Check that all constraints are enforced *)

module TypeSet = Btype.TypeSet
module TypeMap = Btype.TypeMap

let rec check_constraints_rec env loc visited ty =
  if TypeSet.mem ty !visited then () else begin
  visited := TypeSet.add ty !visited;
  match get_desc ty with
  | Tconstr (path, args, _) ->
      let decl =
        try Env.find_type path env
        with Not_found ->
          raise (Error(loc, Unavailable_type_constructor path)) in
      let ty' = Ctype.newconstr path (Ctype.instance_list decl.type_params) in
      begin
        (* We don't expand the error trace because that produces types that
           *already* violate the constraints -- we need to report a problem with
           the unexpanded types, or we get errors that talk about the same type
           twice.  This is generally true for constraint errors. *)
        match Ctype.matches ~expand_error_trace:false env ty ty' with
        | Unification_failure err ->
          raise (Error(loc, Constraint_failed (env, err)))
        | Jkind_mismatch { original_jkind; inferred_jkind; ty } ->
          let jkind_of_type ty = Some (Ctype.type_jkind_purely env ty) in
          let violation =
            Jkind.Violation.of_ ~jkind_of_type
              (Not_a_subjkind (Jkind.disallow_right original_jkind,
                               Jkind.disallow_left inferred_jkind,
                               []))
          in
          raise (Error(loc, Jkind_mismatch_due_to_bad_inference
                            (ty, violation, Check_constraints)))
        | All_good -> ()
      end;
      List.iter (check_constraints_rec env loc visited) args
  | Tpoly (ty, tl) ->
      let ty = Ctype.instance_poly tl ty in
      check_constraints_rec env loc visited ty
  | _ ->
      Btype.iter_type_expr (check_constraints_rec env loc visited) ty
  end

let check_constraints_labels env visited l pl =
  let rec get_loc name = function
      [] -> assert false
    | pld :: tl ->
        if name = pld.pld_name.txt then pld.pld_type.ptyp_loc
        else get_loc name tl
  in
  List.iter
    (fun {Types.ld_id=name; ld_type=ty} ->
       check_constraints_rec env (get_loc (Ident.name name) pl) visited ty)
    l

let check_constraints env sdecl (_, decl) =
  let visited = ref TypeSet.empty in
  List.iter2
    (fun (sty, _) ty -> check_constraints_rec env sty.ptyp_loc visited ty)
    sdecl.ptype_params decl.type_params;
  begin match decl.type_kind with
  | Type_abstract _ -> ()
  (* We skip this check because with [or_null_reexport] the [type_kind]
     does not match [sdecl.ptype_kind]. This is sound, since re-exporting
     can't introduce new variables in the kind. *)
  | Type_variant _ when
    Builtin_attributes.has_or_null_reexport decl.type_attributes -> ()
  | Type_variant (l, _rep, _umc) ->
      let find_pl = function
          Ptype_variant pl -> pl
      | Ptype_record _ | Ptype_record_unboxed_product _ | Ptype_abstract
      | Ptype_open ->
          assert false
      in
      let pl = find_pl sdecl.ptype_kind in
      let pl_index =
        let foldf acc x =
          String.Map.add x.pcd_name.txt x acc
        in
        List.fold_left foldf String.Map.empty pl
      in
      (* CR layouts v5: when we add the "mixed block restriction", we'll
         probably want to check it here. *)
      List.iter
        (fun {Types.cd_id=name; cd_args; cd_res} ->
          let {pcd_args; pcd_res; _} =
            try String.Map.find (Ident.name name) pl_index
            with Not_found -> assert false in
          begin match cd_args, pcd_args with
          | Cstr_tuple tyl, Pcstr_tuple styl ->
              List.iter2
                (fun arg {Types.ca_type=ty; _} ->
                   check_constraints_rec env arg.pca_type.ptyp_loc visited ty)
                styl tyl
          | Cstr_record tyl, Pcstr_record styl ->
              check_constraints_labels env visited tyl styl
          | _ -> assert false
          end;
          match pcd_res, cd_res with
          | Some sr, Some r ->
              check_constraints_rec env sr.ptyp_loc visited r
          | _ ->
              () )
        l
  | Type_record (l, _, _) ->
      let find_pl = function
        | Ptype_record pl -> pl
        | Ptype_record_unboxed_product _ | Ptype_variant _ | Ptype_abstract
        | Ptype_open ->
          assert false
      in
      let pl = find_pl sdecl.ptype_kind in
      check_constraints_labels env visited l pl
  | Type_record_unboxed_product (l, _, _) ->
      let find_pl = function
        | Ptype_record_unboxed_product pl -> pl
        | Ptype_record _ | Ptype_variant _ | Ptype_abstract | Ptype_open ->
          assert false
      in
      let pl = find_pl sdecl.ptype_kind in
      check_constraints_labels env visited l pl
  | Type_open -> ()
  end;
  begin match decl.type_manifest with
  | None -> ()
  | Some ty ->
      let sty =
        match sdecl.ptype_manifest with Some sty -> sty | _ -> assert false
      in
      check_constraints_rec env sty.ptyp_loc visited ty
  end

(* Check that [type_jkind] (where we've stored the jkind annotation, if any)
   corresponds to the manifest (e.g., in the case where [type_jkind] is
   immediate, we should check the manifest is immediate). Also, update the
   resulting jkind to match the manifest. *)
let narrow_to_manifest_jkind env loc decl =
  match decl.type_manifest, decl.type_kind with
  | None, _ -> decl
  | Some _, (Type_record _ | Type_record_unboxed_product _ | Type_variant _ | Type_open)
    when not (Builtin_attributes.has_or_null_reexport decl.type_attributes)
    ->
    (* If there's both a manifest and a non-abstract kind, there's no reason to check that
       the jkind of the manifest matches the annotation ([decl.type_jkind]). This is
       because the manifest's jkind is exactly the kind's jkind, and we've already checked
       and updated [type_jkind] based on the kind. *)
    decl
  | Some ty, _ ->
    (* CR layouts v2.8: Remove this use of [type_jkind_purely], which is slow
       and effectful. But we cannot do so easily, sadly. I tried using
       [estimate_type_jkind] here instead, but this runs aground with mutually
       recursive declarations with manifests. Example:

       {[
         type s1 = float#
         and s2 = s1
       ]}

       We assign jkind [any] to both [s1] and [s2] in [transl_declaration],
       because we really can't do better at that point. But then when we get
       here, [estimate_type_jkind] on [s1] returns [any] -- it won't expand
       the manifest to get [float64]. That's not unsound, because a jkind is
       always just an approximation. But it does mean that we need to load more
       cmi files in practice to get the "real" jkind. This was observed when
       compiling Jane Street's codebase, requiring lots more dependencies to be
       added. Boo. [type_jkind_purely] avoids this problem, by looking deeply
       to find the [float#] and thus the [float64] jkind.

       The solution I have in mind here is to change the jkind assigned in
       [transl_declaration] to be [kind_of 'a], where ['a] is the type variable
       invented in [enter_type]; that variable can be thought of an empty vessel
       that accumulates information about jkinds from usages. It also,
       critically, gets unified with the actual type being defined, in
       [update_type], which happens before [narrow_to_manifest_jkind]. If we use
       [kind_of 'a] in [transl_declaration], then [decl.type_jkind] will be
       [kind_of 'a] here, and that will force the [constrain_type_jkind] below
       to work just hard enough to find a jkind less than [kind_of 'a] --
       exactly what we want to record in the final [type_jkind] of the decl.
       (This will require changing [constrain_type_jkind] to return its best
       jkind, but that is easy to do, and indeed used to be the case until the
       result was never used anywhere.)

       Do not try this (that is, removing the use of [type_jkind_purely]) before
       removing the "horrible hack" just below, as that horrible hack sometimes
       avoids calling [constrain_type_jkind], which is necessary for the plan
       above to work.  *)
    let manifest_jkind = Ctype.type_jkind_purely env ty in
    (* CR layouts v2.8: Remove this horrible hack. In practice, this
       [try_allow_r] fails in the case of a record re-export, because the jkind
       from the record has been calculated and put in decl.type_jkind at this
       point.  So we need to use the deeply broken [type_jkind_purely] and then
       [sub_jkind_l] here. The right way forward is to parameterize
       [constrain_type_jkind] over the [l]-ness of its bound. But probably not
       until we have proper subsumption working, as this hack will likely hold
       up for a little while. *)
    begin match Jkind.try_allow_r decl.type_jkind with
    | None -> begin
        let type_equal = Ctype.type_equal env in
        let jkind_of_type ty = Some (Ctype.type_jkind_purely env ty) in
        match
          Jkind.sub_jkind_l ~type_equal ~jkind_of_type
            manifest_jkind decl.type_jkind
        with
        | Ok () -> ()
        | Error v -> raise (Error (loc, Jkind_mismatch_of_type (ty,v)))
      end
    | Some type_jkind -> begin
        match Ctype.constrain_type_jkind env ty type_jkind with
        | Ok () -> ()
        | Error v -> raise (Error (loc, Jkind_mismatch_of_type (ty,v)))
      end
    end;
    { decl with type_jkind = manifest_jkind }

(* Check that the type expression (if present) is compatible with the kind.
   If both a variant/record definition and a type equation are given,
   need to check that the equation refers to a type of the same kind
   with the same constructors and labels. *)
let check_kind_coherence env loc dpath decl =
  match decl.type_kind, decl.type_manifest with
  | (Type_variant _ | Type_record _ | Type_record_unboxed_product _
    | Type_open),
    Some ty ->
    begin match get_desc ty with
    | Tconstr(path, args, _) ->
      begin
      try
        let decl' = Env.find_type path env in
        let err =
          if List.length args <> List.length decl.type_params
          then Some Includecore.Arity
          else begin
            match Ctype.equal env false args decl.type_params with
            | exception Ctype.Equality err ->
                Some (Includecore.Constraint err)
            | () ->
              let subst =
                Subst.Unsafe.add_type_path dpath path Subst.identity in
              let decl =
                match Subst.Unsafe.type_declaration subst decl with
                | Ok decl -> decl
                | Error (Fcm_type_substituted_away _) ->
                      (* no module type substitution in [subst] *)
                    assert false
              in
              Includecore.type_declarations ~loc ~equality:true env
                ~mark:true
                (Path.last path)
                decl'
                dpath
                decl
          end
        in
        if err <> None then
          raise (Error(loc, Definition_mismatch (ty, env, err)))
      with Not_found ->
        raise(Error(loc, Unavailable_type_constructor path))
      end
    | _ -> raise (Error(loc, Definition_mismatch (ty, env, None)))
    end
  | _ -> ()

let check_coherence env loc dpath decl =
  check_kind_coherence env loc dpath decl;
  match decl.type_unboxed_version with
  | Some decl' ->
    check_kind_coherence env loc (Path.unboxed_version dpath) decl'
  | None -> ()

let check_abbrev env sdecl (id, decl) =
  check_coherence env sdecl.ptype_loc (Path.Pident id) decl;
  (id, narrow_to_manifest_jkind env sdecl.ptype_loc decl)

(* The [update_x_sorts] functions infer more precise jkinds in the type kind,
   including which fields of a record are void.  This would be hard to do during
   [transl_declaration] due to mutually recursive types.
*)
(* [update_label_sorts] additionally returns whether all the jkinds
   were void, and the jkinds of the labels *)
(* CR reisenberg: remove all_void return *)
let update_label_sorts env loc lbls named =
  (* [named] is [Some sorts] for top-level records (we will update the
     sorts) and [None] for inlined records. *)
  (* CR layouts v5: it wouldn't be too hard to support records that are all
     void.  just needs a bit of refactoring in translcore *)
  let update =
    match named with
    | None -> fun _ _ -> ()
    | Some sorts -> fun idx sort -> sorts.(idx) <- sort
  in
  let lbls_and_jkinds =
    List.mapi (fun idx (Types.{ld_type} as lbl) ->
      let jkind = Ctype.type_jkind env ld_type in
      (* Next line guaranteed to be safe because of [check_representable] *)
      let sort = Jkind.sort_of_jkind jkind in
      let ld_sort = Jkind.Sort.default_to_value_and_get sort in
      update idx ld_sort;
      {lbl with ld_sort}, jkind
    ) lbls
  in
  let lbls, jkinds = List.split lbls_and_jkinds in
  if List.for_all (fun l -> Jkind.Sort.Const.all_void l.ld_sort) lbls then
    raise (Error (loc, Jkind_empty_record))
  else lbls, false, jkinds
(* CR layouts v5: return true for a record with all voids *)

(* In addition to updated constructor arguments, returns whether
   all arguments are void, useful for detecting enumerations that
   can be [immediate]. *)
let update_constructor_arguments_sorts env loc cd_args sorts =
  let update =
    match sorts with
    | None -> fun _ _ -> ()
    | Some sorts -> fun idx sort -> sorts.(idx) <- sort
  in
  match cd_args with
  | Types.Cstr_tuple args ->
    let args_and_jkinds =
      List.mapi (fun idx ({Types.ca_type; _} as arg) ->
          let jkind = Ctype.type_jkind env ca_type in
          (* Next line guaranteed to be safe because of [check_representable] *)
          let sort = Jkind.sort_of_jkind jkind in
          let ca_sort = Jkind.Sort.default_to_value_and_get sort in
          update idx ca_sort;
          {arg with ca_sort}, jkind)
        args
    in
    let args, jkinds = List.split args_and_jkinds in
    Types.Cstr_tuple args,
    List.for_all
      (fun { ca_sort } -> Jkind_types.Sort.Const.(all_void ca_sort)) args,
    jkinds
  | Types.Cstr_record lbls ->
    let lbls, all_void, jkinds =
      update_label_sorts env loc lbls None
    in
    update 0 Jkind.Sort.Const.value;
    Types.Cstr_record lbls, all_void, jkinds

let assert_mixed_product_support =
  let required_reserved_header_bits = 8 in
  (* Why 2? We'd subtract 1 if the mixed block encoding could use all 8 bits of
     the prefix. But the all-0 prefix means "not a mixed block", so we can't use
     the all-0 pattern, and we must subtract 2 instead. *)
  let max_value_prefix_len = (1 lsl required_reserved_header_bits) - 2 in
  fun loc mixed_product_kind ~value_prefix_len ->
    let required_layouts_level = Language_extension.Stable in
    if not (Language_extension.is_at_least Layouts required_layouts_level) then
      raise (Error (loc, Illegal_mixed_product
                      (Insufficient_level { required_layouts_level;
                                            mixed_product_kind;
                                          })));
    if Config.reserved_header_bits < required_reserved_header_bits then
      raise (Error (loc, Illegal_mixed_product
                      (Runtime_support_not_enabled
                        mixed_product_kind)));
    if value_prefix_len > max_value_prefix_len then
      raise
        (Error (loc,
                Illegal_mixed_product
                  (Value_prefix_too_long
                     { value_prefix_len; max_value_prefix_len;
                       mixed_product_kind })))

(* [Element_repr] is used to classify whether something is a "mixed product"
   (a mixed record or mixed variant constructor), meaning that some of the
   fields are unboxed in a way that isnt encoded in the usual short numeric tag.
   "Element" refers to a constructor argument or record field.
*)
module Element_repr = struct
  type unboxed_element =
    | Float64
    | Float32
    | Bits8
    | Bits16
    | Bits32
    | Bits64
    | Vec128
    | Vec256
    | Vec512
    | Word
    | Product of t array

  and t =
    | Unboxed_element of unboxed_element
    | Float_element
    | Value_element
    | Void
    (* This type technically permits [Float_element] to appear in an unboxed
       product, but we never generate that and make no attempt to apply the
       float record optimization to records of unboxed products of floats. Kinds
       don't give us enough information to do this reliably, and you could just
       use unboxed floats instead. *)

  let classify env ty jkind =
    if is_float env ty then Float_element
    else
      let layout = Jkind.get_layout_defaulting_to_value jkind in
      let sort =
        match Jkind.Layout.Const.get_sort layout with
        | None ->
          Misc.fatal_error "Element_repr.classify: unexpected abstract layout"
        | Some s -> s
      in
      let rec sort_to_t : Jkind_types.Sort.Const.t -> t = function
      | Base Value -> Value_element
      | Base Float64 -> Unboxed_element Float64
      | Base Float32 -> Unboxed_element Float32
      | Base Word -> Unboxed_element Word
      | Base Bits8 -> Unboxed_element Bits8
      | Base Bits16 -> Unboxed_element Bits16
      | Base Bits32 -> Unboxed_element Bits32
      | Base Bits64 -> Unboxed_element Bits64
      | Base Vec128 -> Unboxed_element Vec128
      | Base Vec256 -> Unboxed_element Vec256
      | Base Vec512 -> Unboxed_element Vec512
      | Base Void -> Void
      | Product l ->
        Unboxed_element (Product (Array.of_list (List.map sort_to_t l)))
      in
      sort_to_t sort

  and mixed_product_shape loc ts kind =
    let to_shape_element (t,_ty) : mixed_block_element =
      let rec of_t : t -> mixed_block_element = function
      | Unboxed_element unboxed -> of_unboxed_element unboxed
      | Float_element | Value_element -> Value
      | Void -> Void
      and of_unboxed_element : unboxed_element -> mixed_block_element = function
        | Float64 -> Float64
        | Float32 -> Float32
        | Bits8 -> Bits8
        | Bits16 -> Bits16
        | Bits32 -> Bits32
        | Bits64 -> Bits64
        | Vec128 -> Vec128
        | Vec256 -> Vec256
        | Vec512 -> Vec512
        | Word -> Word
        | Product l -> Product (Array.map of_t l)
      in
      of_t t
    in
    let boxed_elements =
      let rec count_boxed_in_t acc : t -> int = function
        | Unboxed_element u -> count_boxed_in_unboxed_element acc u
        | Void -> acc
        | Float_element | Value_element -> acc + 1
      and count_boxed_in_unboxed_element acc : unboxed_element -> int =
        function
        | Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64
        | Vec128 | Vec256 | Vec512 | Word -> acc
        | Product l -> Array.fold_left count_boxed_in_t acc l
      in
      List.fold_left (fun acc (t,_) -> count_boxed_in_t acc t) 0 ts
    in
    let mixed =
      List.exists
        (function ((Unboxed_element _ | Void), _) -> true | _ -> false) ts
    in
    if not mixed then None else begin
      assert_mixed_product_support loc kind ~value_prefix_len:boxed_elements;
      Some (List.map to_shape_element ts |> Array.of_list)
    end
end

let update_constructor_representation
    env (cd_args : Types.constructor_arguments) arg_jkinds ~loc
    ~is_extension_constructor
  =
  let flat_suffix =
    match cd_args with
    | Cstr_tuple arg_types_and_modes ->
        let arg_reprs =
          List.map2 (fun {Types.ca_type=arg_type; _} arg_jkind ->
            Element_repr.classify env arg_type arg_jkind, arg_type)
            arg_types_and_modes arg_jkinds
        in
        Element_repr.mixed_product_shape loc arg_reprs Cstr_tuple
    | Cstr_record fields ->
        let arg_reprs =
          List.map2 (fun ld arg_jkind ->
              Element_repr.classify env ld.Types.ld_type arg_jkind,
              ld.Types.ld_type)
            fields arg_jkinds
        in
        Element_repr.mixed_product_shape loc arg_reprs Cstr_record
  in
  match flat_suffix with
  | None -> Constructor_uniform_value
  | Some shape ->
      (* CR layouts v5.9: Enable extension constructors in the flambda2
         middle-end so that we can permit them in the source language.
      *)
      if is_extension_constructor then
        raise (Error (loc, Illegal_mixed_product Extension_constructor));
      Constructor_mixed shape


let add_types_to_env decls shapes env =
  List.fold_right2
    (fun (id, decl) shape env ->
      add_type ~check:true ~shape id decl env)
    decls shapes env

(* This function updates jkind stored in kinds with more accurate jkinds.
   It is called after the circularity checks and the delayed jkind checks
   have happened, so we can fully compute jkinds of types.

   This function does not do any /checks/ on the jkind after performing the update; the
   annotation, for example, is checked in the caller, [update_decls_jkind], so that
   mutually recursive type decls see each others' best kinds during normalization and
   subsumption
*)
let rec update_decl_jkind env dpath decl =
  let type_unboxed_version =
    Option.map
      (fun d ->
        update_decl_jkind env (Path.unboxed_version dpath) d)
      decl.type_unboxed_version
  in
  let decl = { decl with type_unboxed_version } in
  let open struct
    (* For tracking what types appear in record blocks. All product layouts
       count only as a [non_float64_unboxed_field], even if it's a
       [float64 & float64] or [void & void].
    *)
    type element_repr_summary =
      {  mutable values : bool; (* includes immediates. *)
         mutable floats: bool;
         (* For purposes of this record, [floats] tracks whether any field
            has layout value and is known to be a float.
         *)
         mutable float64s : bool;
         mutable non_float64_unboxed_fields : bool;
         (* Includes product containing void *)
         mutable voids : bool;
      }
  end in

  (* returns updated labels, updated rep, and updated jkind *)
  let update_record_kind loc lbls rep =
    match lbls, rep with
    | [Types.{ld_type} as lbl], Record_unboxed ->
      let jkind =
        Ctype.type_jkind env ld_type |>
        Jkind.apply_modality_l lbl.ld_modalities
      in
      (* This next line is guaranteed to be OK because of a call to
         [check_representable] *)
      let sort = Jkind.sort_of_jkind jkind in
      let ld_sort = Jkind.Sort.default_to_value_and_get sort in
      [{lbl with ld_sort}], Record_unboxed, jkind
    | _, Record_boxed sorts ->
      let lbls, _all_void, jkinds =
        update_label_sorts env loc lbls (Some sorts)
      in
      let jkind = Jkind.for_boxed_record lbls in
      let reprs =
        List.map2
          (fun lbl jkind ->
             Element_repr.classify env lbl.Types.ld_type jkind,
             lbl.Types.ld_type)
          lbls jkinds
      in
      let repr_summary =
        { values = false; floats = false; float64s = false;
          non_float64_unboxed_fields = false; voids = false;
        }
      in
      List.iter
        (fun ((repr : Element_repr.t), _lbl) ->
           match repr with
           | Float_element -> repr_summary.floats <- true
           | Unboxed_element Float64 -> repr_summary.float64s <- true
           | Unboxed_element ( Float32 | Bits8 | Bits16 | Bits32 | Bits64
                             | Vec128 | Vec256 | Vec512 | Word | Product _ ) ->
               repr_summary.non_float64_unboxed_fields <- true
           | Value_element -> repr_summary.values <- true
           | Void ->
               repr_summary.voids <- true)
        reprs;
      let rep =
        (* CR layouts: improve the readability of this match *)
        match repr_summary with
        (* We store floats flatly in mixed records if all fields are
           float/float64/void. *)
        | { values = false; floats = true;
            float64s = true; non_float64_unboxed_fields = false }
           ->
            let shape =
              List.map
                (fun ((repr : Element_repr.t), _lbl) ->
                  match repr with
                  | Float_element -> Float_boxed
                  | Unboxed_element Float64 -> Float64
                  | Void -> Void
                  | Unboxed_element (Float32 | Bits8 | Bits16 | Bits32 | Bits64
                                    | Vec128 | Vec256 | Vec512 | Word
                                    | Product _)
                  | Value_element ->
                      Misc.fatal_error "Expected only floats and float64s")
                reprs
              |> Array.of_list
            in
            assert_mixed_product_support loc Record ~value_prefix_len:0;
            Record_mixed shape
        (* For other mixed blocks, float fields are stored as flat
           only when they're unboxed.
        *)
        | { values = true; voids = true }
        | { floats = true; voids = true }
        | { float64s = true; voids = true }
        | { values = true; float64s = true }
        | { non_float64_unboxed_fields = true } ->
            let shape =
              Element_repr.mixed_product_shape loc reprs Record
            in
            let shape =
              match shape with
              | Some x -> x
              | None -> Misc.fatal_error "expected mixed block"
            in
            Record_mixed shape
        (* value-only records are stored as boxed records *)
        | { values = true; float64s = false; non_float64_unboxed_fields = false;
            voids = false }
          -> rep
        (* All-float and all-float64 records are stored as flat float records.
        *)
        | { values = false; floats = true ; float64s = false;
            non_float64_unboxed_fields = false; voids = false } ->
          Record_float
        | { values = false; floats = false; float64s = true;
            non_float64_unboxed_fields = false; voids = false } ->
          Record_ufloat
        | { values = false; floats = false; float64s = false;
            non_float64_unboxed_fields = false; voids = _ }  ->
          (* CR layouts v5: support all-void records *)
          Misc.fatal_error "Typedecl.update_record_kind: empty record"
      in
      lbls, rep, jkind
    | _, ( Record_inlined _ | Record_float | Record_ufloat
         | Record_mixed _)
    | ([] | (_ :: _)), Record_unboxed ->
      (* These are never created by [transl_declaration]. *)
      Misc.fatal_error
        "Typedecl.update_record_kind: unexpected record representation"
  in

  (* returns updated constructors, updated rep, and updated jkind *)
  let update_variant_kind loc cstrs rep =
    (* CR layouts: factor out duplication *)
    match cstrs, rep with
    | _, Variant_with_null ->
      (* CR layouts v3.5: this case only happens with [or_null_reexport].
         Change when we allow users to write their own null constructors. *)
      (* CR layouts v3.3: use [any_non_null]. *)
      cstrs, rep, Jkind.Builtin.value_or_null ~why:(Primitive Predef.ident_or_null)
    | [{Types.cd_args} as cstr], Variant_unboxed -> begin
        match cd_args with
        | Cstr_tuple [{ca_type=ty; _} as arg] -> begin
            let jkind = Ctype.type_jkind env ty in
            let sort = Jkind.sort_of_jkind jkind in
            let ca_sort = Jkind.Sort.default_to_value_and_get sort in
            [{ cstr with Types.cd_args =
                           Cstr_tuple [{ arg with ca_sort }] }],
            Variant_unboxed, jkind
          end
        | Cstr_record [{ld_type} as lbl] -> begin
            let jkind = Ctype.type_jkind env ld_type in
            let sort = Jkind.sort_of_jkind jkind in
            let ld_sort = Jkind.Sort.default_to_value_and_get sort in
            [{ cstr with Types.cd_args =
                           Cstr_record [{ lbl with ld_sort }] }],
            Variant_unboxed, jkind
          end
        | (Cstr_tuple ([] | _ :: _ :: _) | Cstr_record ([] | _ :: _ :: _)) ->
          assert false
      end
    | cstrs, Variant_boxed cstr_shapes ->
      let (_,cstrs) =
        List.fold_left (fun (idx,cstrs) cstr ->
          let arg_sorts =
            match cstr_shapes.(idx) with
            | Constructor_uniform_value, arg_sorts -> arg_sorts
            | Constructor_mixed _, _ ->
                fatal_error
                  "Typedecl.update_variant_kind doesn't expect mixed \
                   constructor as input"
          in
          let cd_args, _all_void, jkinds =
            update_constructor_arguments_sorts env cstr.Types.cd_loc
              cstr.Types.cd_args (Some arg_sorts)
          in
          let cstr_repr =
            update_constructor_representation env cd_args jkinds
              ~is_extension_constructor:false
              ~loc:cstr.Types.cd_loc
          in
          let () =
            match cstr_repr with
            | Constructor_uniform_value -> ()
            | Constructor_mixed _ -> cstr_shapes.(idx) <- cstr_repr, arg_sorts
          in
          let cstr = { cstr with Types.cd_args } in
          (idx+1,cstr::cstrs)
        ) (0,[]) cstrs
      in
      let jkind = Jkind.for_boxed_variant ~loc cstrs in
      List.rev cstrs, rep, jkind
    | (([] | (_ :: _)), Variant_unboxed | _, Variant_extensible) ->
      assert false
  in


  let new_decl =
    match decl.type_kind with
    | Type_abstract _ ->
      (* Abstract types should never have quality=best, but let's double check that here
         just to be safe *)
      assert (not (Jkind.is_best decl.type_jkind));
      decl
    | Type_open ->
      let type_jkind =
        Jkind.for_non_float ~why:Extensible_variant
        (* It's unlikely we'll ever be able to give better kinds than [value] to
           extensible variants, so we're not worried about backwards compatibility if we
           mark them as best here, and we want to be able to normalize them away *)
        (* See Note [Quality of jkinds during inference] for more information about when
           we mark jkinds as best *)
        |> Jkind.mark_best
      in
      { decl with type_jkind }
    | Type_record (lbls, rep, umc) ->
      let lbls, rep, type_jkind = update_record_kind decl.type_loc lbls rep in
      (* See Note [Quality of jkinds during inference] for more information about when we
         mark jkinds as best *)
      let type_jkind = Jkind.mark_best type_jkind in
      { decl with type_kind = Type_record (lbls, rep, umc); type_jkind }
    (* CR layouts v3.0: handle this case in [update_variant_jkind] when
       [Variant_with_null] introduced.

       No updating required for [or_null_reexport], and we must not
       incorrectly override the jkind to [non_null].
    *)
    | Type_record_unboxed_product (lbls, rep, umc) ->
        begin match rep with
        | Record_unboxed_product ->
          let lbls =
            List.map (fun (Types.{ld_type} as lbl) ->
              let jkind = Ctype.type_jkind env ld_type in
              (* This next line is guaranteed to be OK because of a call to
                 [check_representable] *)
              let sort = Jkind.sort_of_jkind jkind in
              let ld_sort = Jkind.Sort.default_to_value_and_get sort in
              {lbl with ld_sort}
            ) lbls
          in
          let type_jkind = Jkind.for_unboxed_record lbls in
          (* See Note [Quality of jkinds during inference] for more information about when we
             mark jkinds as best *)
          let type_jkind = Jkind.mark_best type_jkind in
          { decl with type_kind = Type_record_unboxed_product (lbls, rep, umc);
                      type_jkind }
        end
    | Type_variant _ when
      Builtin_attributes.has_or_null_reexport decl.type_attributes ->
      decl
    | Type_variant (cstrs, rep, umc) ->
      let cstrs, rep, type_jkind =
        update_variant_kind decl.type_loc cstrs rep
      in
      (* See Note [Quality of jkinds during inference] for more information
         about when we mark jkinds as best *)
      let type_jkind = Jkind.mark_best type_jkind in
      { decl with type_kind = Type_variant (cstrs, rep, umc); type_jkind }
  in

  (* Check the layout here, both to check it, but more importantly to fill in any sort
     variables in the original decl's jkind, which might be shared with the jkinds of
     other types in a (maybe mutually recursive) type declaration. See Note [Default
     jkinds in transl_declaration]) *)
  match
    Jkind.Layout.sub new_decl.type_jkind.jkind.layout decl.type_jkind.jkind.layout
  with
  | Not_le reason ->
    let jkind_of_type ty = Some (Ctype.type_jkind_purely env ty) in
    raise (Error (
      decl.type_loc,
      Jkind_mismatch_of_path (
        dpath,
        Jkind.Violation.of_ ~jkind_of_type (
          Not_a_subjkind (
            new_decl.type_jkind, decl.type_jkind, Nonempty_list.to_list reason)))))
  | Less | Equal -> new_decl

let update_decls_jkind_reason env decls =
  List.map
    (fun (id, decl) ->
       let update_generalized =
        Ctype.check_and_update_generalized_ty_jkind
          ~name:id ~loc:decl.type_loc env
       in
       List.iter update_generalized decl.type_params;
       Btype.iter_type_expr_kind update_generalized decl.type_kind;
       Option.iter update_generalized decl.type_manifest;
       let reason = Jkind.History.Generalized (Some id, decl.type_loc) in
       let new_decl = {decl with type_jkind =
                                   Jkind.History.update_reason decl.type_jkind reason} in
       (id, new_decl)
    )
    decls

let update_decls_jkind env decls =
  List.map
    (fun (id, decl) ->
       let allow_any_crossing =
         Builtin_attributes.has_unsafe_allow_any_mode_crossing
           decl.type_attributes
       in

       (* Check that the attribute is valid, if set (unconditionally, for
          consistency). *)
       if allow_any_crossing then begin
         match decl.type_kind with
         | Type_abstract _ | Type_open ->
           raise(
             Error(decl.type_loc, Unsafe_mode_crossing_on_invalid_type_kind))
         | _ -> ()
       end;

       (id, decl, allow_any_crossing, update_decl_jkind env (Pident id) decl))
    decls

(* See Note [Typechecking unboxed versions of types]. *)
let check_unboxed_paths decls ~unboxed_version_banned =
  (* We iterate on all subexpressions of the declaration to check "in depth"
     that no non-existent unboxed version is used. *)
  let open Btype in
  let checked =
    (* [checked] remembers the types that the iterator already
        checked, to avoid looping on cyclic types. *)
    ref TypeSet.empty
  in
  let check_ty loc ty =
    match get_desc ty with
    | Tconstr(Pextra_ty (path, Punboxed_ty), _, _)
      when unboxed_version_banned path ->
        raise (Error (loc, No_unboxed_version path))
    | _ -> ()
  in
  let check_decl d =
    let it =
      {type_iterators with it_type_expr =
        (fun self ty ->
          if not (TypeSet.mem ty !checked) then begin
            check_ty d.type_loc ty;
            checked := TypeSet.add ty !checked;
            self.it_do_type_expr self ty
          end)}
    in
    it.it_type_declaration it (Ctype.generic_instance_declaration d)
  in
  List.iter (fun (_, d) -> check_decl d) decls

(* Note: Well-foundedness for OCaml types

   We want to guarantee that all cycles within OCaml types are
   "guarded".

   More precisely, we consider a reachability relation
     "[t] is reachable [guarded|unguarded] from [u]"
   defined as follows:

   - [t1, t2...] are reachable guarded from object types
       [< m1 : t1; m2 : t2; ... >]
     or polymorphic variants
       [[`A of t1 | `B of t2 | ...]].

   - [t1, t2...] are reachable rectypes-guarded from
     [t1 -> t2], [t1 * t2 * ...], and all other built-in
     contractive type constructors.

     (By rectypes-guarded we mean: guarded if -rectypes is set,
      unguarded if it is not set.)

   - If [(t1, t2...) c] is a datatype (variant or record),
     then [t1, t2...] are reachable rectypes-guarded from it.

   - If [(t1, t2...) c] is an abstract type,
     then [t1, t2...] are reachable unguarded from it.

   - If [(t1, t2...) c] is an (expandable) abbreviation,
     then its expansion is reachable unguarded from it.
     Note that we do not define [t1, t2...] as reachable.

   - The relation is transitive and guardedness of a composition
     is the disjunction of each guardedness:
     if t1 is reachable from t2 and t2 is reachable from t3;
     then t1 is reachable guarded from t3 if t1 is guarded in t2
     or t2 is guarded in t3, and reachable unguarded otherwise.

   A type [t] is not well-founded if and only if [t] is reachable
   unguarded in [t].

   Notice that, in the case of datatypes, the arguments of
   a parametrized datatype are reachable (they must not contain
   recursive occurrences of the type), but the definition of the
   datatype is not defined as reachable.

      (* well-founded *)
      type t = Foo of u
      and u = t

      (* ill-founded *)
      type 'a t = Foo of 'a
      and u = u t
      > Error: The type abbreviation u is cyclic

   Indeed, in the second example [u] is reachable unguarded in [u t]
   -- its own definition.
*)

(* Note: Forms of ill-foundedness

   Several OCaml language constructs could introduce ill-founded
   types, and there are several distinct checks that forbid different
   sources of ill-foundedness.

   1. Type aliases.

      (* well-founded *)
      type t = < x : 'a > as 'a

      (* ill-founded, unless -rectypes is used *)
      type t = (int * 'a) as 'a
      > Error: This alias is bound to type int * 'a
      > but is used as an instance of type 'a
      > The type variable 'a occurs inside int * 'a

      Ill-foundedness coming from type aliases is detected by the "occur check"
      used by our type unification algorithm. See typetexp.ml.

   2. Type abbreviations.

      (* well-founded *)
      type t = < x : t >

      (* ill-founded, unless -rectypes is used *)
      type t = (int * t)
      > Error: The type abbreviation t is cyclic

      Ill-foundedness coming from type abbreviations is detected by
      [check_well_founded] below.

  3. Recursive modules.

     (* well-founded *)
     module rec M : sig type t = < x : M.t > end = M

     (* ill-founded, unless -rectypes is used *)
     module rec M : sig type t = int * M.t end = M
     > Error: The definition of M.t contains a cycle:
     >        int * M.t

     This is also checked by [check_well_founded] below,
     as called from [check_recmod_typedecl].

  4. Functor application

     A special case of (3) is that a type can be abstract
     in a functor definition, and be instantiated with
     an abbreviation in an application of the functor.
     This can introduce ill-foundedness, so functor applications
     must be checked by re-checking the type declarations of their result.

     module type T = sig type t end
     module Fix(F:(T -> T)) = struct
       (* this recursive definition is well-founded
          as F(Fixed).t contains no reachable type expression. *)
       module rec Fixed : T with type t = F(Fixed).t = F(Fixed)
     end

     (* well-founded *)
     Module M = Fix(functor (M:T) -> struct type t = < x : M.t > end)

     (* ill-founded *)
     module M = Fix(functor (M:T) -> struct type t = int * M.t end);;
     > Error: In the signature of this functor application:
     >   The definition of Fixed.t contains a cycle:
     >   F(Fixed).t
*)

(* Check that a type expression is well-founded:
   - if -rectypes is used, we must prevent non-contractive fixpoints
     ('a as 'a)
   - if -rectypes is not used, we only allow cycles in the type graph
     if they go through an object or polymorphic variant type *)

let check_well_founded ~abs_env env loc path to_check visited ty0 =
  let rec check parents trace ty =
    if TypeSet.mem ty parents then begin
      (*Format.eprintf "@[%a@]@." Printtyp.raw_type_expr ty;*)
      let err =
        let reaching_path, rec_abbrev =
          (* The reaching trace is accumulated in reverse order, we
             reverse it to get a reaching path. *)
          match trace with
          | [] -> assert false
          | Expands_to (ty1, _) :: trace when (match get_desc ty1 with
              Tconstr (p,_,_) -> Path.same p path | _ -> false) ->
                List.rev trace, true
          | trace -> List.rev trace, false
        in
        if rec_abbrev
        then Recursive_abbrev (Path.name path, abs_env, reaching_path)
        else Cycle_in_def (Path.name path, abs_env, reaching_path)
      in raise (Error (loc, err))
    end;
    let (fini, parents) =
      try
        (* Map each node to the set of its already checked parents *)
        let prev = TypeMap.find ty !visited in
        if TypeSet.subset parents prev then (true, parents) else
        let parents = TypeSet.union parents prev in
        visited := TypeMap.add ty parents !visited;
        (false, parents)
      with Not_found ->
        visited := TypeMap.add ty parents !visited;
        (false, parents)
    in
    if fini then () else
    let rec_ok =
      match get_desc ty with
      | Tconstr(p,_,_) ->
          !Clflags.recursive_types && Ctype.is_contractive env p
      | Tobject _ | Tvariant _ -> true
      | _ -> !Clflags.recursive_types
    in
    if rec_ok then () else
    let parents = TypeSet.add ty parents in
    match get_desc ty with
    | Tconstr(p, tyl, _) ->
        let to_check = to_check p in
        if to_check then List.iter (check_subtype parents trace ty) tyl;
        begin match Ctype.try_expand_once_opt env ty with
        | ty' -> check parents (Expands_to (ty, ty') :: trace) ty'
        | exception Ctype.Cannot_expand ->
            if not to_check then List.iter (check_subtype parents trace ty) tyl
        end
    | _ ->
        Btype.iter_type_expr (check_subtype parents trace ty) ty
  and check_subtype parents trace outer_ty inner_ty =
      check parents (Contains (outer_ty, inner_ty) :: trace) inner_ty
  in
  let snap = Btype.snapshot () in
  try Ctype.wrap_trace_gadt_instances env (check TypeSet.empty []) ty0
  with Ctype.Escape _ ->
    (* Will be detected by check_regularity *)
    Btype.backtrack snap

let check_well_founded_manifest ~abs_env env loc path decl =
  if decl.type_manifest = None then () else
  let args =
    (* The jkinds here shouldn't matter for the purposes of
       [check_well_founded] *)
    List.map (fun _ -> Ctype.newvar (Jkind.Builtin.any ~why:Dummy_jkind))
      decl.type_params
  in
  let visited = ref TypeMap.empty in
  check_well_founded ~abs_env env loc path (Path.same path) visited
    (Ctype.newconstr path args)

(* Given a new type declaration [type t = ...] (potentially mutually-recursive),
   we check that accepting the declaration does not introduce ill-founded types.

   Note: we check that the types at the toplevel of the declaration
   are not reachable unguarded from themselves, that is, we check that
   there is no cycle going through the "root" of the declaration. But
   we *also* check that all the type sub-expressions reachable from
   the root even those that are guarded, are themselves
   well-founded. (So we check the absence of cycles, even for cycles
   going through inner type subexpressions but not the root.

   We are not actually sure that this "deep check" is necessary
   (we don't have an example at hand where it is necessary), but we
   are doing it anyway out of caution.
*)
let check_well_founded_decl  ~abs_env env loc path decl to_check =
  let open Btype in
  (* We iterate on all subexpressions of the declaration to check
     "in depth" that no ill-founded type exists. *)
  let it =
    let checked =
      (* [checked] remembers the types that the iterator already
         checked, to avoid looping on cyclic types. *)
      ref TypeSet.empty in
    let visited =
      (* [visited] remembers the inner visits performed by
         [check_well_founded] on each type expression reachable from
         this declaration. This avoids unnecessary duplication of
         [check_well_founded] work when invoked on two parts of the
         type declaration that have common subexpressions. *)
      ref TypeMap.empty in
    {type_iterators with it_type_expr =
     (fun self ty ->
       if TypeSet.mem ty !checked then () else begin
         check_well_founded  ~abs_env env loc path to_check visited ty;
         checked := TypeSet.add ty !checked;
         self.it_do_type_expr self ty
       end)} in
  it.it_type_declaration it (Ctype.generic_instance_declaration decl)

(* We only allow recursion in unboxed product types to occur through boxes,
   otherwise the type is uninhabitable and usually also infinite-size.
   See [typing-layouts-unboxed-records/recursive.ml].

   Because [check_well_founded] already ruled out recursion through structural
   types, we just look for a cycle in nominal unboxed types ([@@unboxed] types
   and unboxed records), tracking the set of seen paths.

   For each group of mutually recursive type declarations, we define the
   following "type contains" transitive relation on type expressions:

   1. Unboxed records and variants defined in the group contain their fields.

      If [type 'a t = #{ ...; lbl : u;  ... }],
      or [type 'a t = { lbl : u } [@@unboxed]],
      or [type 'a t = U of u [@@unboxed]]
      is in the recursive group, then ['a t] contains [u].

   2. Abbreviations defined in the group contain their expansions.

      If [type 'a t = u] is in the recursive group then ['a t] contains [u].

   3. Unboxed tuples contain their components.

      [#(u_1 * ...)] contains all [u_i].

   4. Types not in the group contain the parameters indicated by their layout.

      ['a t] contains ['a] if [layout_of 'a] or [any] occurs in ['a t]'s layout.

      For example, if [('a, 'b) t] has layout [layout_of 'a], it may contain
      ['a], but not ['b]. If it has layout [any], we must conservatively
      consider it to contain both ['a] and ['b].

      Note: We don't yet have [layout_of], so currently only consider [any].

   If a path starting from the type expression on the LHS of a declaration
   contains two types with the same head type constructor, and that repeated
   type is an unboxed record or variant, then the check raises a type error.

   CR layouts v7.2: accept safe types that expand the same path multiple times,
   e.g. [type 'a t = #{ a : 'a } and x = int t t], either by using layouts
   variables or the algorithm from "Unboxed data constructors - or, how cpp
   decides a halting problem."
   See https://github.com/oxcaml/oxcaml/pull/3407.
*)
type step_result =
  | Contained of type_expr list
  | Expanded_to of type_expr
  | Is_cyclic
let check_unboxed_recursion ~abs_env env loc path0 ty0 to_check =
  let contained_parameters tyl layout =
    (* A type whose layout has [any] could contain all its parameters.
       CR layouts v11: update this function for [layout_of] layouts. *)
    let rec has_any : Jkind_types.Layout.Const.t -> bool = function
      | Any -> true
      | Base _ -> false
      | Product l -> List.exists has_any l
    in
    if has_any layout then tyl else []
  in
  let step_once parents ty =
    match get_desc ty with
    | Tconstr (path, tyl, _) ->
      if to_check path then
        if Path.Set.mem path parents then
          Is_cyclic, parents
        else
          let parents = Path.Set.add path parents in
          match Ctype.try_expand_safe_opt env ty with
          | ty' ->
            Expanded_to ty', parents
          | exception Ctype.Cannot_expand ->
            Contained (Ctype.contained_without_boxing env ty), parents
      else
        begin try
          (* Determine contained types by layout for decls outside of the
             recursive group *)
          let jkind = (Env.find_type path env).type_jkind in
          let layout = Option.get (Jkind.get_layout jkind) in
          Contained (contained_parameters tyl layout), parents
        with Not_found | Invalid_argument _ ->
          (* Because [to_check path] is false, this decl has already been
            typechecked, so it's already in [env] with a constant layout. *)
          Misc.fatal_error "Typedecl.check_unboxed_recursion"
        end
    | _ -> Contained (Ctype.contained_without_boxing env ty), parents
  in
  let rec visit parents trace ty =
    match step_once parents ty with
    | Contained tys, parents ->
      List.iter (fun ty' -> visit parents (Contains (ty, ty') :: trace) ty') tys
    | Expanded_to ty', parents ->
      visit parents (Expands_to(ty,ty') :: trace) ty'
    | Is_cyclic, _ ->
      raise (Error (loc, Unboxed_recursion (path0, abs_env, List.rev trace)))
  in
  Ctype.wrap_trace_gadt_instances env (visit Path.Set.empty []) ty0

let check_unboxed_recursion_decl ~abs_env env loc path decl to_check =
  let decl = Ctype.generic_instance_declaration decl in
  let ty = Btype.newgenty (Tconstr (path, decl.type_params, ref Mnil)) in
  check_unboxed_recursion ~abs_env env loc (Path.name path) ty to_check;
  match decl.type_unboxed_version with
  | None -> ()
  | Some decl ->
      let path = Path.unboxed_version path in
      let ty = Btype.newgenty (Tconstr (path, decl.type_params, ref Mnil)) in
      check_unboxed_recursion ~abs_env env loc (Path.name path) ty to_check

(* Check for non-regular abbreviations; an abbreviation
   [type 'a t = ...] is non-regular if the expansion of [...]
   contains instances [ty t] where [ty] is not equal to ['a].

   Note: in the case of a constrained type definition
   [type 'a t = ... constraint 'a = ...], we require
   that all instances in [...] be equal to the constrained type.
*)

let check_regularity ~abs_env env loc path decl to_check =
  (* to_check is true for potentially mutually recursive paths.
     (path, decl) is the type declaration to be checked. *)

  if decl.type_params = [] then () else

  let visited = ref TypeSet.empty in

  let rec check_regular cpath args prev_exp trace ty =
    if not (TypeSet.mem ty !visited) then begin
      visited := TypeSet.add ty !visited;
      match get_desc ty with
      | Tconstr(path', args', _) ->
          if Path.same path path' then begin
            if not (Ctype.is_equal abs_env false args args') then
              raise (Error(loc,
                     Non_regular {
                       definition=path;
                       used_as=ty;
                       defined_as=Ctype.newconstr path args;
                       reaching_path=List.rev trace;
                     }))
          end
          (* Attempt to expand a type abbreviation if:
              1- [to_check path'] holds
                 (otherwise the expansion cannot involve [path]);
              2- we haven't expanded this type constructor before
                 (otherwise we could loop if [path'] is itself
                 a non-regular abbreviation). *)
          else if to_check path' && not (List.mem path' prev_exp) then begin
            try
              (* Attempt expansion *)
              let (params0, body0, _) = Env.find_type_expansion path' env in
              let (params, body) =
                Ctype.instance_parameterized_type params0 body0 in
              begin
                try List.iter2 (Ctype.unify abs_env) args' params
                with Ctype.Unify err ->
                  raise (Error(loc, Constraint_failed (abs_env, err)));
              end;
              check_regular path' args
                (path' :: prev_exp) (Expands_to (ty,body) :: trace)
                body
            with Not_found -> ()
          end;
          List.iter (check_subtype cpath args prev_exp trace ty) args'
      | Tpoly (ty, tl) ->
          let ty = Ctype.instance_poly ~keep_names:true tl ty in
          check_regular cpath args prev_exp trace ty
      | _ ->
          Btype.iter_type_expr
            (check_subtype cpath args prev_exp trace ty) ty
    end
    and check_subtype cpath args prev_exp trace outer_ty inner_ty =
      let trace = Contains (outer_ty, inner_ty) :: trace in
      check_regular cpath args prev_exp trace inner_ty
  in

  Option.iter
    (fun body ->
      let (args, body) =
        Ctype.instance_parameterized_type
          ~keep_names:true decl.type_params body in
      List.iter (check_regular path args [] []) args;
      check_regular path args [] [] body)
    decl.type_manifest

let check_abbrev_regularity ~abs_env env id_loc_list to_check tdecl =
  let decl = tdecl.typ_type in
  let id = tdecl.typ_id in
  check_regularity ~abs_env env (List.assoc id id_loc_list) (Path.Pident id)
    decl to_check

let check_duplicates sdecl_list =
  let labels = Hashtbl.create 7 in
  let unboxed_labels = Hashtbl.create 7 in
  let constrs = Hashtbl.create 7 in
  List.iter
    (fun sdecl -> match sdecl.ptype_kind with
      Ptype_variant cl ->
        List.iter
          (fun pcd ->
            try
              let name' = Hashtbl.find constrs pcd.pcd_name.txt in
              Location.prerr_warning pcd.pcd_loc
                (Warnings.Duplicate_definitions
                   ("constructor", pcd.pcd_name.txt, name',
                    sdecl.ptype_name.txt))
            with Not_found ->
              Hashtbl.add constrs pcd.pcd_name.txt sdecl.ptype_name.txt)
          cl
    | Ptype_record fl ->
        List.iter
          (fun {pld_name=cname;pld_loc=loc} ->
            try
              let name' = Hashtbl.find labels cname.txt in
              Location.prerr_warning loc
                (Warnings.Duplicate_definitions
                   ("label", cname.txt, name', sdecl.ptype_name.txt))
            with Not_found -> Hashtbl.add labels cname.txt sdecl.ptype_name.txt)
          fl
    | Ptype_record_unboxed_product fl ->
        List.iter
          (fun {pld_name=cname;pld_loc=loc} ->
            try
              let name' = Hashtbl.find unboxed_labels cname.txt in
              Location.prerr_warning loc
                (Warnings.Duplicate_definitions
                   ("unboxed record label", cname.txt, name',
                    sdecl.ptype_name.txt))
            with Not_found ->
              Hashtbl.add unboxed_labels cname.txt sdecl.ptype_name.txt)
          fl
    | Ptype_abstract -> ()
    | Ptype_open -> ())
    sdecl_list

(* Force recursion to go through id for private types*)
let name_recursion sdecl id decl =
  match decl with
  | { type_kind = Type_abstract _;
      type_manifest = Some ty;
      type_private = Private; } when is_fixed_type sdecl ->
    let ty' = newty2 ~level:(get_level ty) (get_desc ty) in
    if Ctype.deep_occur ty ty' then
      let td = Tconstr(Path.Pident id, decl.type_params, ref Mnil) in
      link_type ty (newty2 ~level:(get_level ty) td);
      {decl with type_manifest = Some ty'}
    else decl
  | _ -> decl

let name_recursion_decls sdecls decls =
  List.map2 (fun sdecl (id, decl) -> (id, name_recursion sdecl id decl))
    sdecls decls

(* Warn on definitions of type "type foo = ()" which redefine a different unit
   type and are likely a mistake. *)
let check_redefined_unit (td: Parsetree.type_declaration) =
  let open Parsetree in
  let is_unit_constructor cd = cd.pcd_name.txt = "()" in
  match td with
  | { ptype_name = { txt = name };
      ptype_manifest = None;
      ptype_kind = Ptype_variant [ cd ] }
    when is_unit_constructor cd ->
      Location.prerr_warning td.ptype_loc (Warnings.Redefining_unit name)
  | _ ->
      ()


(* Note [Quality of jkinds during inference]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   We're careful during inference of jkinds for type declarations not to ever call
   [Jkind.mark_best] on a jkind before we've added all the various pieces of information
   to the jkind (the full layout, and all the with-bounds). Early in translation, we give
   "dummy" kinds to types, without any with bounds, and have been careful not to mark
   these as having a [Best] quality. Later on, in [update_decls_jkind], once we've learned
   everything there is to know about a type declaration, we mark the new kind as [Best].
   It's important to do this /before/ [normalize_decl_jkinds], so that mutually recursive
   type declarations can look up each others' (best, though perhaps not normalized!)
   jkind.
*)

(* Normalize the jkinds in a list of (potentially mutually recursive) type declarations *)
let normalize_decl_jkinds env shapes decls =
  let rec normalize_decl_jkind env original_decl allow_any_crossing decl path =
    let type_unboxed_version =
      Option.map (fun type_unboxed_version ->
        normalize_decl_jkind env (Option.get original_decl.type_unboxed_version)
          allow_any_crossing type_unboxed_version (Path.unboxed_version path))
      decl.type_unboxed_version
    in
    let normalized_jkind =
      Jkind.normalize
        ~mode:Require_best
        ~jkind_of_type:(fun ty -> Some (Ctype.type_jkind env ty))
        decl.type_jkind
    in
    let decl =
      { decl with type_jkind = normalized_jkind; type_unboxed_version }
    in
    if normalized_jkind != original_decl.type_jkind then begin
      (* If the jkind has changed, check that it is a subjkind of the original jkind
        that we computed, either from a user-written annotation or as a dummy jkind.

         (see Note [Default jkinds in transl_declaration]) *)
      (* CR layouts v2.8: it almost definitely has changed, but also we probably trust
         the new jkind (we really only want this check here to check against the
         user-written annotation). We might be able to do a better job here and save
         some work. *)
      let jkind_of_type ty = Some (Ctype.type_jkind_purely env ty) in
      let type_equal = Ctype.type_equal env in
      match
        (* CR layouts v2.8: Consider making a function that doesn't compute
           histories for this use-case, which doesn't need it. *)
        Jkind.sub_jkind_l
          ~type_equal
          ~jkind_of_type
          ~allow_any_crossing
          decl.type_jkind
          original_decl.type_jkind
      with
      | Ok _ ->
        if allow_any_crossing then
          (* If the user is asking us to allow any crossing, we use the mod- and
             with-bounds from the annotation rather than the modal bounds inferred from
             the type_kind. However, we /only/ take the bounds, not the layout - because
             we still want to be able to eg locally use a type declared as layout [any] as
             [value] if that's its actual layout! *)
          let type_jkind =
            Jkind.unsafely_set_bounds ~from:original_decl.type_jkind decl.type_jkind
          in
          let umc = Some (Jkind.to_unsafe_mode_crossing type_jkind) in
          let type_kind =
            match decl.type_kind with
            | Type_abstract _ | Type_open -> assert false (* Checked above *)
            | Type_record (lbls, rep, _) ->
              Type_record (lbls, rep, umc)
            | Type_record_unboxed_product (lbls, rep, _) ->
              Type_record_unboxed_product (lbls, rep, umc)
            | Type_variant (cs, rep, _) ->
              Type_variant (cs, rep, umc)
          in
          { decl with type_jkind; type_kind; }
        else decl
      | Error err ->
        raise(Error(decl.type_loc,
                    Jkind_mismatch_of_path (path, err)))
    end
    else decl
  in
  (* Add the types, with non-normalized kinds, to the environment to start, so that eg
     types can look up their own (potentially non-normalized) kinds *)
  let env =
    List.fold_right2
      (fun (id, _, _, decl) shape env ->
         add_type ~check:true ~shape id decl env)
      decls shapes env
  in
  Misc.Stdlib.List.fold_left_map2
    (fun env (id, original_decl, allow_any_crossing, decl) shape ->
       let decl =
         normalize_decl_jkind env original_decl allow_any_crossing decl
           (Pident id)
       in
       (* Add the decl with the normalized kind back to the environment, so that later
          kinds don't have to normalize this kind if they mention this type in their
          with-bounds *)
       let env = add_type ~check:false ~shape:shape id decl env in
       env, (id, decl)
    )
    env
    decls
    shapes

(* Translate a set of type declarations, mutually recursive or not *)
let transl_type_decl env rec_flag sdecl_list =
  List.iter check_redefined_unit sdecl_list;
  (* Add dummy types for fixed rows *)
  let fixed_types = List.filter is_fixed_type sdecl_list in
  let sdecl_list =
    List.map
      (fun sdecl ->
         let ptype_name =
           let loc = Location.ghostify sdecl.ptype_name.loc in
           mkloc (sdecl.ptype_name.txt ^"#row") loc
         in
         let ptype_kind = Ptype_abstract in
         let ptype_manifest = None in
         let ptype_loc = Location.ghostify sdecl.ptype_loc in
        {sdecl with
           ptype_name; ptype_kind; ptype_manifest; ptype_loc })
      fixed_types
    @ sdecl_list
  in

  (* Create identifiers. *)
  let scope = Ctype.create_scope () in
  let ids_list =
    List.map (fun sdecl ->
      Ident.create_scoped ~scope sdecl.ptype_name.txt,
      Uid.mk ~current_unit:(Env.get_unit_name ())
    ) sdecl_list
  in
  (* Translate declarations, using a temporary environment where abbreviations
     expand to a generic type variable. After that, we check the coherence of
     the translated declarations in the resulting new environment. *)
  let tdecls, decls, shapes, new_env, delayed_jkind_checks =
    Ctype.with_local_level_iter ~post:generalize_decl begin fun () ->
      (* Enter types. *)
      let temp_env =
        List.fold_left2 (enter_type rec_flag) env sdecl_list ids_list in
      (* Translate each declaration. *)
      let current_slot = ref None in
      let warn_unused =
        Warnings.is_active (Warnings.Unused_type_declaration "") in
      let ids_slots (id, _uid as ids) =
        match rec_flag with
        | Asttypes.Recursive when warn_unused ->
            (* See typecore.ml for a description of the algorithm used to
               detect unused declarations in a set of recursive definitions. *)
            let slot = ref [] in
            let td = Env.find_type (Path.Pident id) temp_env in
            Env.set_type_used_callback
              td
              (fun old_callback ->
                match !current_slot with
                | Some slot -> slot := td.type_uid :: !slot
                | None ->
                    List.iter Env.mark_type_used (get_ref slot);
                    old_callback ()
              );
            ids, Some slot
        | Asttypes.Recursive | Asttypes.Nonrecursive ->
            ids, None
      in
      let transl_declaration name_sdecl (id, slot) =
        current_slot := slot;
        Builtin_attributes.warning_scope
          name_sdecl.ptype_attributes
          (fun () -> transl_declaration temp_env name_sdecl id)
      in
      (* Translate declarations, using a temporary environment where
         abbreviations expand to a generic type variable. After that, we check
         the coherence of the translated declarations in the resulting new
         enviroment. *)
      let tdecls =
        List.map2 transl_declaration sdecl_list (List.map ids_slots ids_list) in
      let tdecls, shapes = List.split tdecls in
      let decls = List.map (fun d -> (d.typ_id, d.typ_type)) tdecls in
      let decls = derive_unboxed_versions decls env in
      let tdecls =
        List.map2
          (fun tdecl (_, decl) -> { tdecl with typ_type = decl }) tdecls decls
      in
      current_slot := None;
      (* Check for duplicates *)
      check_duplicates sdecl_list;
      (* Build the final env. *)
      let new_env = add_types_to_env decls shapes env in
      (* Update stubs *)
      let delayed_jkind_checks =
        match rec_flag with
        | Asttypes.Nonrecursive -> []
        | Asttypes.Recursive ->
          List.map2
            (fun (id, _) sdecl ->
               update_type temp_env new_env id sdecl.ptype_loc,
               sdecl.ptype_loc)
            ids_list sdecl_list
      in
      ((tdecls, decls, shapes, new_env, delayed_jkind_checks), List.map snd decls)
    end
  in
  (* Check for ill-formed abbrevs *)
  let id_loc_list =
    List.map2 (fun (id, _) sdecl -> (id, sdecl.ptype_loc))
      ids_list sdecl_list
  in
  (* [check_abbrev_regularity] and error messages cannot use the new
     environment, as this might result in non-termination. Instead we use a
     completely abstract version of the temporary environment, giving a reason
     for why abbreviations cannot be expanded (#12334, #12368) *)
  let abs_env =
    List.fold_left2
      (enter_type ~abstract_abbrevs:Rec_check_regularity rec_flag)
      env sdecl_list ids_list in
  check_unboxed_paths decls
    ~unboxed_version_banned:(fun path ->
       match Env.find_type (Path.unboxed_version path) new_env with
        | _ -> false | exception Not_found -> true);
  List.iter (fun (id, decl) ->
    check_well_founded_manifest ~abs_env new_env (List.assoc id id_loc_list)
      (Path.Pident id) decl)
    decls;
  let to_check =
    function
    | Path.Pident id | Path.Pextra_ty (Path.Pident id, Punboxed_ty) ->
      List.mem_assoc id id_loc_list
    | _ -> false
  in
  List.iter (fun (id, decl) ->
    check_well_founded_decl ~abs_env new_env (List.assoc id id_loc_list)
      (Path.Pident id)
      decl to_check)
    decls;
  List.iter
    (check_abbrev_regularity ~abs_env new_env id_loc_list to_check) tdecls;
  List.iter (fun (id, decl) ->
    check_unboxed_recursion_decl ~abs_env new_env (List.assoc id id_loc_list)
      (Path.Pident id)
      decl to_check)
    decls;
  (* Now that we've ruled out ill-formed types, we can perform the delayed
     jkind checks *)
  List.iter (fun (checks,loc) ->
    List.iter (fun (ty,jkind) ->
      (* The use [check_type_jkind] rather than [constrain_type_jkind] here is
         conservative. It ensures that the delayed checks don't succeed by
         mutating type variables from the [temp_env] in a way that won't be
         reflected in the final type decls and may be incompatible with them.
         An alternative would be to beef up [check_constraints] and really make
         sure we re-check any kind constraint that could arise from translating
         the typedecl RHSs, for example by looking at Typedtree instead of
         what's just in the type environment. See Test 41 in
         [tests/typing-layouts/basics.ml] for a subtle example. *)
      match Ctype.check_type_jkind new_env ty jkind with
      | Ok _ -> ()
      | Error err ->
        (* This inner match is just here to detect when we're rejecting this
           program because we're being conservative in the sense of the previous
           comment, and issue an error admitting to it. *)
        begin match Ctype.constrain_type_jkind new_env ty jkind with
        | Error _ ->
          let err = Errortrace.unification_error ~trace:[Bad_jkind (ty,err)] in
          raise (Error (loc, Type_clash (new_env, err)))
        | Ok _ ->
          raise (Error (loc, Jkind_mismatch_due_to_bad_inference
                               (ty, err, Delayed_checks)))
        end)
      checks)
    delayed_jkind_checks;
  (* Check that all type variables are closed; this also defaults any remaining
     sort variables. Defaulting must happen before update_decls_jkind,
     Typedecl_seperability.update_decls, and add_types_to_env, all of which need
     to check whether parts of the type are void (and currently use Jkind.equate
     to do this which would set any remaining sort variables to void). It also
     must happen before check_constraints, so that check_constraints can detect
     when a jkind is inferred incorrectly.  (The unification that
     check_constraints does is undone via backtracking, and thus forgetting to
     do the defaulting first is actually unsound: the unification in
     check_constraints will succeed via mutation, be backtracked, and then
     perhaps a sort variable gets defaulted to value. Bad bad.) *)
  List.iter2
    (fun sdecl tdecl ->
      let decl = tdecl.typ_type in
       match Ctype.closed_type_decl decl with
         Some ty -> raise(Error(sdecl.ptype_loc, Unbound_type_var(ty,decl)))
       | None   -> ())
    sdecl_list tdecls;
  (* Check that constraints are enforced *)
  List.iter2 (check_constraints new_env) sdecl_list decls;
  (* Add type properties to declarations *)
  let new_env, decls =
    try
      let new_env, decls =
        decls
        |> name_recursion_decls sdecl_list
        |> Typedecl_variance.update_decls env sdecl_list
        |> Typedecl_separability.update_decls env
        |> update_decls_jkind new_env
        |> normalize_decl_jkinds new_env shapes
      in
      let removed, decls = remove_unboxed_versions decls in
      if not (Path.Set.is_empty removed) then
        check_unboxed_paths decls
          ~unboxed_version_banned:(fun p -> Path.Set.mem p removed);
      new_env, update_decls_jkind_reason new_env decls
    with
    | Typedecl_variance.Error (loc, err) ->
        raise (Error (loc, Variance err))
    | Typedecl_separability.Error (loc, err) ->
        raise (Error (loc, Separability err))
  in
  (* Check re-exportation, updating [type_jkind] from the manifest *)
  let decls = List.map2 (check_abbrev new_env) sdecl_list decls in
  (* Compute the final environment with variance and immediacy *)
  let final_env = add_types_to_env decls shapes env in
  (* Save the shapes of the declarations in [Type_shape] for debug info. *)
  List.iter (fun (id, decl) ->
    Type_shape.add_to_type_decls
      (Pident id) decl
      (Env.find_uid_of_path final_env)
  ) decls;
  (* Keep original declaration *)
  let final_decls =
    List.map2
      (fun tdecl (_id2, decl) ->
        { tdecl with typ_type = decl }
      ) tdecls decls
  in
  (* Done *)
  (final_decls, final_env, shapes)

(* Translating type extensions *)
let transl_extension_constructor_decl
      env type_path typext_params loc id svars sargs sret_type =
  let tvars, targs, tret_type, args, ret_type =
    make_constructor env loc
      ~cstr_path:(Pident id) ~type_path ~unboxed:false typext_params
      svars sargs sret_type
  in
  let args, constant, jkinds =
    update_constructor_arguments_sorts env loc args None
  in
  let constructor_shape =
    update_constructor_representation env args jkinds ~loc
      ~is_extension_constructor:true
  in
  args, constructor_shape, constant, ret_type,
  Text_decl(tvars, targs, tret_type)

let transl_extension_constructor ~scope env type_path type_params
                                 typext_params priv sext =
  let id = Ident.create_scoped ~scope sext.pext_name.txt in
  let loc = sext.pext_loc in
  let args, shape, constant, ret_type, kind =
    match sext.pext_kind with
      Pext_decl(svars, sargs, sret_type) ->
      transl_extension_constructor_decl
        env type_path typext_params loc id svars sargs sret_type
    | Pext_rebind lid ->
        let usage : Env.constructor_usage =
          if priv = Public then Env.Exported else Env.Exported_private
        in
        let cdescr = Env.lookup_constructor ~loc:lid.loc usage lid.txt env in
        let (args, cstr_res, _ex) =
          Ctype.instance_constructor Keep_existentials_flexible cdescr
        in
        let res, ret_type =
          if cdescr.cstr_generalized then
            let params = Ctype.instance_list type_params in
            let res = Ctype.newconstr type_path params in
            let ret_type = Some (Ctype.newconstr type_path params) in
              res, ret_type
          else (Ctype.newconstr type_path typext_params), None
        in
        begin
          try
            Ctype.unify env cstr_res res
          with Ctype.Unify err ->
            raise (Error(lid.loc,
                     Rebind_wrong_type(lid.txt, env, err)))
        end;
        (* Remove "_" names from parameters used in the constructor *)
        if not cdescr.cstr_generalized then begin
          let vars =
            Ctype.free_variables
              (Btype.newgenty (Ttuple (List.map (fun {Types.ca_type=t; _} -> None, t) args)))
          in
          List.iter
            (fun ty ->
              match get_desc ty with
              | Tvar { name = Some "_"; jkind }
                when List.exists (eq_type ty) vars ->
                set_type_desc ty (Tvar { name = None; jkind })
              | _ -> ())
            typext_params
        end;
        (* Ensure that constructor's type matches the type being extended *)
        let cstr_type_path = Btype.cstr_type_path cdescr in
        let cstr_type_params = (Env.find_type cstr_type_path env).type_params in
        let cstr_types =
          (Btype.newgenty
             (Tconstr(cstr_type_path, cstr_type_params, ref Mnil)))
          :: cstr_type_params
        in
        let ext_types =
          (Btype.newgenty
             (Tconstr(type_path, type_params, ref Mnil)))
          :: type_params
        in
        if not (Ctype.is_equal env true cstr_types ext_types) then
          raise (Error(lid.loc,
                       Rebind_mismatch(lid.txt, cstr_type_path, type_path)));
        (* Disallow rebinding private constructors to non-private *)
        begin
          match cdescr.cstr_private, priv with
            Private, Public ->
              raise (Error(lid.loc, Rebind_private lid.txt))
          | _ -> ()
        end;
        let path =
          match cdescr.cstr_tag with
            Extension path -> path
          | _ -> assert false
        in
        let args =
          match cdescr.cstr_inlined with
          | None ->
              Types.Cstr_tuple args
          | Some decl ->
              let tl =
                match List.map (fun {Types.ca_type=ty; _} -> get_desc ty) args with
                | [ Tconstr(_, tl, _) ] -> tl
                | _ -> assert false
              in
              let decl = Ctype.instance_declaration decl in
              assert (List.length decl.type_params = List.length tl);
              List.iter2 (Ctype.unify env) decl.type_params tl;
              let lbls =
                match decl.type_kind with
                | Type_record (lbls, Record_inlined _, _) -> lbls
                | _ -> assert false
              in
              Types.Cstr_record lbls
        in
        args, cdescr.cstr_shape,
        cdescr.cstr_constant, ret_type,
        Text_rebind(path, lid)
  in
  let ext =
    { ext_type_path = type_path;
      ext_type_params = typext_params;
      ext_args = args;
      ext_shape = shape;
      ext_constant = constant;
      ext_ret_type = ret_type;
      ext_private = priv;
      Types.ext_loc = sext.pext_loc;
      Types.ext_attributes = sext.pext_attributes;
      ext_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
    }
  in
  let ext_cstrs =
    { ext_id = id;
      ext_name = sext.pext_name;
      ext_type = ext;
      ext_kind = kind;
      Typedtree.ext_loc = sext.pext_loc;
      Typedtree.ext_attributes = sext.pext_attributes; }
  in
  let shape =
    let map = match args with
    | Cstr_record lbls -> shape_map_labels lbls
    | _ -> Shape.Map.empty
    in
    Shape.str ~uid:ext_cstrs.ext_type.ext_uid map
 in
  ext_cstrs, shape

let transl_extension_constructor ~scope env type_path type_params
    typext_params priv sext =
  Builtin_attributes.warning_scope sext.pext_attributes
    (fun () -> transl_extension_constructor ~scope env type_path type_params
        typext_params priv sext)

let is_rebind ext =
  match ext.ext_kind with
  | Text_rebind _ -> true
  | Text_decl _ -> false

let transl_type_extension extend env loc styext =
  let type_path, type_decl =
    let lid = styext.ptyext_path in
    Env.lookup_type ~loc:lid.loc lid.txt env
  in
  begin
    match type_decl.type_kind with
    | Type_open -> begin
        match type_decl.type_private with
        | Private when extend -> begin
            match
              List.find
                (function {pext_kind = Pext_decl _} -> true
                        | {pext_kind = Pext_rebind _} -> false)
                styext.ptyext_constructors
            with
            | {pext_loc} ->
                raise (Error(pext_loc, Cannot_extend_private_type type_path))
            | exception Not_found -> ()
          end
        | _ -> ()
      end
    | _ ->
        raise (Error(loc, Not_extensible_type type_path))
  end;
  let type_variance =
    List.map (fun v ->
                let (co, cn) = Variance.get_upper v in
                  (not cn, not co, false))
             type_decl.type_variance
  in
  let err =
    if type_decl.type_arity <> List.length styext.ptyext_params then
      Some Includecore.Arity
    else
      if List.for_all2
           (fun (c1, n1, _) (c2, n2, _) -> (not c2 || c1) && (not n2 || n1))
           type_variance
           (Typedecl_variance.variance_of_params styext.ptyext_params)
      then None else Some Includecore.Variance
  in
  begin match err with
  | None -> ()
  | Some err -> raise (Error(loc, Extension_mismatch (type_path, env, err)))
  end;
  let ttype_params, _type_params, constructors =
    (* Note: it would be incorrect to call [create_scope] *after*
       [TyVarEnv.reset] or after [with_local_level] (see #10010). *)
    let scope = Ctype.create_scope () in
    Ctype.with_local_level begin fun () ->
      TyVarEnv.reset();
      let ttype_params = make_params env type_path styext.ptyext_params in
      let type_params = List.map (fun (cty, _) -> cty.ctyp_type) ttype_params in
      List.iter2 (Ctype.unify_var env)
        (Ctype.instance_list type_decl.type_params)
        type_params;
      let constructors =
        List.map (transl_extension_constructor ~scope env type_path
                    type_decl.type_params type_params styext.ptyext_private)
          styext.ptyext_constructors
      in
      (ttype_params, type_params, constructors)
    end
    ~post: begin fun (_, type_params, constructors) ->
      (* Generalize types *)
      List.iter Ctype.generalize type_params;
      List.iter
        (fun (ext, _shape) ->
          Btype.iter_type_expr_cstr_args Ctype.generalize ext.ext_type.ext_args;
          Option.iter Ctype.generalize ext.ext_type.ext_ret_type)
        constructors;
    end
  in
  (* Check that all type variables are closed *)
  List.iter
    (fun (ext, _shape) ->
       match Ctype.closed_extension_constructor ext.ext_type with
         Some ty ->
           raise(Error(ext.ext_loc, Unbound_type_var_ext(ty, ext.ext_type)))
       | None -> ())
    constructors;
  (* Check variances are correct *)
  List.iter
    (fun (ext, _shape) ->
       (* Note that [loc] here is distinct from [type_decl.type_loc], which
          makes the [loc] parameter to this function useful. [loc] is the
          location of the extension, while [type_decl] points to the original
          type declaration being extended. *)
       try Typedecl_variance.check_variance_extension
             env type_decl ext (type_variance, loc)
       with Typedecl_variance.Error (loc, err) ->
         raise (Error (loc, Variance err)))
    constructors;
  (* Add extension constructors to the environment *)
  let newenv =
    List.fold_left
      (fun env (ext, shape) ->
         let rebind = is_rebind ext in
         Env.add_extension ~check:true ~shape ~rebind
           ext.ext_id ext.ext_type env)
      env constructors
  in
  let constructors, shapes = List.split constructors in
  let tyext =
    { tyext_path = type_path;
      tyext_txt = styext.ptyext_path;
      tyext_params = ttype_params;
      tyext_constructors = constructors;
      tyext_private = styext.ptyext_private;
      tyext_loc = styext.ptyext_loc;
      tyext_attributes = styext.ptyext_attributes; }
  in
    (tyext, newenv, shapes)

let transl_type_extension extend env loc styext =
  Builtin_attributes.warning_scope styext.ptyext_attributes
    (fun () -> transl_type_extension extend env loc styext)

let transl_exception env sext =
  let ext, shape =
    let scope = Ctype.create_scope () in
    Ctype.with_local_level
      (fun () ->
        TyVarEnv.reset();
        transl_extension_constructor ~scope env
          Predef.path_exn [] [] Asttypes.Public sext)
      ~post: begin fun (ext, _shape) ->
        Btype.iter_type_expr_cstr_args Ctype.generalize ext.ext_type.ext_args;
        Option.iter Ctype.generalize ext.ext_type.ext_ret_type;
      end
  in
  (* Check that all type variables are closed *)
  begin match Ctype.closed_extension_constructor ext.ext_type with
    Some ty ->
      raise (Error(ext.ext_loc, Unbound_type_var_ext(ty, ext.ext_type)))
  | None -> ()
  end;
  let rebind = is_rebind ext in
  let newenv =
    Env.add_extension ~check:true ~shape ~rebind ext.ext_id ext.ext_type env
  in
  ext, newenv, shape

let transl_type_exception env t =
  let contructor, newenv, shape =
    Builtin_attributes.warning_scope t.ptyexn_attributes
      (fun () ->
         transl_exception env t.ptyexn_constructor
      )
  in
  {tyexn_constructor = contructor;
   tyexn_loc = t.ptyexn_loc;
   tyexn_attributes = t.ptyexn_attributes}, newenv, shape


type native_repr_attribute =
  | Native_repr_attr_absent
  | Native_repr_attr_present of native_repr_kind

let get_native_repr_attribute attrs ~global_repr =
  match
    Attr_helper.get_no_payload_attribute "unboxed"  attrs,
    Attr_helper.get_no_payload_attribute "untagged" attrs,
    global_repr
  with
  | None, None, None -> Native_repr_attr_absent
  | None, None, Some repr -> Native_repr_attr_present repr
  | Some _, None, None -> Native_repr_attr_present Unboxed
  | None, Some _, None -> Native_repr_attr_present Untagged
  | Some { Location.loc }, _, _
  | _, Some { Location.loc }, _ ->
    raise (Error (loc, Multiple_native_repr_attributes))

let is_upstream_compatible_non_value_unbox env ty =
  (* CR layouts v2.5: This needs to be updated when we support unboxed
     types with arbitrary names suffixed with "#" *)
  match get_desc (Ctype.expand_head_opt env ty) with
  | Tconstr (path, _, _) ->
    List.exists
      (Path.same path)
      [
        Predef.path_unboxed_float;
        Predef.path_unboxed_int32;
        Predef.path_unboxed_int64;
        Predef.path_unboxed_nativeint;
      ]
  | _ ->
    false

type sort_or_poly = Sort of Jkind.Sort.Const.t | Poly

let native_repr_of_type env kind ty sort_or_poly =
  match kind, get_desc (Ctype.expand_head_opt env ty) with
  | Untagged, Tconstr (_, _, _) when
         Typeopt.maybe_pointer_type env ty
         = (Lambda.Immediate, Lambda.Non_nullable)
      (* Only allow [@untagged] on immediate values. [maybe_pointer_type]
         currently returns [Immediate] on unboxed number types, which
         do not support [@untagged].
      *)
      && match sort_or_poly with
         | Poly -> false
         | Sort (Base Value) -> true
         | Sort (Base _ | Product _) -> false
    ->
    Some Untagged_immediate
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float ->
    Some (Unboxed_float Boxed_float64)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float32 ->
    Some (Unboxed_float Boxed_float32)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int8 ->
    Some (Unboxed_integer Unboxed_int8)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int16 ->
    Some (Unboxed_integer Unboxed_int16)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int32 ->
    Some (Unboxed_integer Unboxed_int32)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int64 ->
    Some (Unboxed_integer Unboxed_int64)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_nativeint ->
    Some (Unboxed_integer Unboxed_nativeint)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int8x16 ->
    Some (Unboxed_vector Boxed_vec128)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int16x8 ->
    Some (Unboxed_vector Boxed_vec128)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int32x4 ->
    Some (Unboxed_vector Boxed_vec128)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int64x2 ->
    Some (Unboxed_vector Boxed_vec128)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float32x4 ->
    Some (Unboxed_vector Boxed_vec128)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float64x2 ->
    Some (Unboxed_vector Boxed_vec128)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int8x32 ->
    Some (Unboxed_vector Boxed_vec256)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int16x16 ->
    Some (Unboxed_vector Boxed_vec256)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int32x8 ->
    Some (Unboxed_vector Boxed_vec256)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int64x4 ->
    Some (Unboxed_vector Boxed_vec256)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float32x8 ->
    Some (Unboxed_vector Boxed_vec256)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float64x4 ->
    Some (Unboxed_vector Boxed_vec256)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int8x64 ->
    Some (Unboxed_vector Boxed_vec512)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int16x32 ->
    Some (Unboxed_vector Boxed_vec512)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int32x16 ->
    Some (Unboxed_vector Boxed_vec512)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int64x8 ->
    Some (Unboxed_vector Boxed_vec512)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float32x16 ->
    Some (Unboxed_vector Boxed_vec512)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float64x8 ->
    Some (Unboxed_vector Boxed_vec512)
  | _ ->
    None

(* Raises an error when [core_type] contains an [@unboxed] or [@untagged]
   attribute in a strict sub-term. *)
let error_if_has_deep_native_repr_attributes core_type =
  let open Ast_iterator in
  let this_iterator =
    { default_iterator with typ = fun iterator core_type ->
      begin
        match
          get_native_repr_attribute core_type.ptyp_attributes ~global_repr:None
        with
        | Native_repr_attr_present kind ->
           raise (Error (core_type.ptyp_loc,
                         Deep_unbox_or_untag_attribute kind))
        | Native_repr_attr_absent -> ()
      end;
      default_iterator.typ iterator core_type }
  in
  default_iterator.typ this_iterator core_type

(* Note that [typ] is guaranteed not to contain sort variables because it was
   produced by [type_scheme], which defaults them.

   However, there can be jkind [any] present with something like:
    [external f : ('a : any). 'a -> 'a = "%identity"]
   In such cases, we raise an expection. *)
let type_sort_external ~is_layout_poly ~why env loc typ =
  match Ctype.type_sort ~why ~fixed:true env typ with
  | Ok s -> Jkind.Sort.default_to_value_and_get s
  | Error err ->
    let kloc =
      if is_layout_poly then External_with_layout_poly else External
    in
    raise(Error (loc, Jkind_sort {kloc; typ; err}))

let make_native_repr env core_type ty ~global_repr ~is_layout_poly ~why =
  error_if_has_deep_native_repr_attributes core_type;
  let sort_or_poly =
    match get_desc (Ctype.get_unboxed_type_approximation env ty).ty with
    (* This only captures tvars with layout [any] explicitly quantified within
       the declaration.

       This is sufficient since [transl_type_scheme] promises that:
       - non-explicitly quantified tvars get sort jkinds
       - this isn't a tvar from an outer scopes ([TyVarEnv] gets reset before
         transl)
    *)
    | Tvar {jkind} when is_layout_poly
                      && Jkind.has_layout_any jkind
                      && get_level ty = Btype.generic_level -> Poly
    | _ ->
      let sort =
        type_sort_external ~is_layout_poly ~why env core_type.ptyp_loc ty
      in
      Sort sort
  in
  match get_native_repr_attribute
          core_type.ptyp_attributes ~global_repr,
        sort_or_poly with
  | Native_repr_attr_absent, Poly ->
    Repr_poly
  | Native_repr_attr_absent, Sort (Base Value) ->
    Same_as_ocaml_repr (Base Value)
  | Native_repr_attr_absent, (Sort (Base sort as c)) ->
    (if Language_extension.erasable_extensions_only ()
    then
      (* Non-value sorts without [@unboxed] are not erasable. *)
      let layout = Jkind_types.Sort.to_string_base sort in
      Location.prerr_warning core_type.ptyp_loc
        (Warnings.Incompatible_with_upstream
              (Warnings.Unboxed_attribute layout)));
    Same_as_ocaml_repr c
  | Native_repr_attr_absent, (Sort ((Product _) as c)) ->
    (if Language_extension.erasable_extensions_only ()
     then
       (* CR layouts v7.1: Using an unboxed product in a C external is not
          upstream compatible and should issue this warning.  Two problems: (1)
          we can't test that yet, because products are not stable, and (2) we
          _should_ allow them with built-ins like "%identity", but the current
          mechanism doesn't allow for this (float# has the same problem). I
          think (2) hasn't arisen much in practice because for other sorts you
          can add an [@unboxed] annotation to suppress the warning, and because
          in practice people use the layout_poly versions which do work fine. *)
       let sort = Format.asprintf "%a" Jkind_types.Sort.Const.format c in
       Location.prerr_warning core_type.ptyp_loc
         (Warnings.Incompatible_with_upstream
            (Warnings.Non_value_sort sort)));
    Same_as_ocaml_repr c
  | Native_repr_attr_present kind, (Poly | Sort (Base Value))
  | Native_repr_attr_present (Untagged as kind), Sort _ ->
    begin match native_repr_of_type env kind ty sort_or_poly with
    | None ->
      raise (Error (core_type.ptyp_loc, Cannot_unbox_or_untag_type kind))
    | Some repr -> repr
    end
  | Native_repr_attr_present Unboxed, (Sort (Base sort as c)) ->
    (* We allow [@unboxed] on non-value sorts.

       This is to enable upstream-compatibility. We want the code to
       still work when all the layout annotations and unboxed types
       get erased.

       One may wonder why can't the erasure process mentioned above
       also add in the [@unboxed] attributes. This is not possible due
       to the fact that:

       1. Without type information, the erasure process can't transform:

        {|
           type t = float#
           external f : t -> t = ...
        |}

       2. We need [is_upstream_compatible_non_value_unbox] to further
          limit the cases that can work with upstream. *)
    (if Language_extension.erasable_extensions_only ()
       && not (is_upstream_compatible_non_value_unbox env ty)
    then
      (* There are additional requirements if we are operating in
         upstream compatible mode. *)
      let layout = Jkind_types.Sort.to_string_base sort in
      Location.prerr_warning core_type.ptyp_loc
        (Warnings.Incompatible_with_upstream
              (Warnings.Non_value_sort layout)));
    Same_as_ocaml_repr c
  | Native_repr_attr_present Unboxed, (Sort (Product _)) ->
    raise (Error (core_type.ptyp_loc, Cannot_unbox_or_untag_type Unboxed))

let prim_const_mode m =
  match Mode.Locality.Guts.check_const m with
  | Some Global -> Prim_global
  | Some Local -> Prim_local
  | None -> assert false

let rec parse_native_repr_attributes env core_type ty rmode
        ~global_repr ~is_layout_poly =
  match core_type.ptyp_desc, get_desc ty,
    get_native_repr_attribute core_type.ptyp_attributes ~global_repr:None
  with
  | Ptyp_arrow _, Tarrow _, Native_repr_attr_present kind  ->
    raise (Error (core_type.ptyp_loc, Cannot_unbox_or_untag_type kind))
  | Ptyp_arrow (_, ct1, ct2, _, _), Tarrow ((_,marg,mret), t1, t2, _), _
    when not (Builtin_attributes.has_curry core_type.ptyp_attributes) ->
    let t1, _ = Btype.tpoly_get_poly t1 in
    let repr_arg =
      make_native_repr
        env ct1 t1 ~global_repr
        ~is_layout_poly ~why:External_argument
    in
    let mode =
      if Builtin_attributes.has_local_opt ct1.ptyp_attributes
      then Prim_poly
      else prim_const_mode (Mode.Alloc.proj_comonadic Areality marg)
    in
    let repr_args, repr_res =
      parse_native_repr_attributes env ct2 t2
        (prim_const_mode (Mode.Alloc.proj_comonadic Areality mret))
        ~global_repr ~is_layout_poly
    in
    ((mode, repr_arg) :: repr_args, repr_res)
  | (Ptyp_poly (_, t) | Ptyp_alias (t, _, _)), _, _ ->
     parse_native_repr_attributes env t ty rmode ~global_repr ~is_layout_poly
  | _ ->
     let rmode =
       if Builtin_attributes.has_local_opt core_type.ptyp_attributes
       then Prim_poly
       else rmode
     in
     let repr_res =
       make_native_repr
        env core_type ty ~global_repr
        ~is_layout_poly ~why:External_result
     in
     ([], (rmode, repr_res))

let check_unboxable env loc ty =
  let rec check_type acc ty : Path.Set.t =
    let ty = Ctype.expand_head_opt env ty in
    try match get_desc ty with
      | Tconstr (p, _, _) ->
        let tydecl = Env.find_type p env in
        if tydecl.type_unboxed_default then
          Path.Set.add p acc
        else acc
      | Tpoly (ty, []) -> check_type acc ty
      | _ -> acc
    with Not_found -> acc
  in
  let all_unboxable_types = Btype.fold_type_expr check_type Path.Set.empty ty in
  Path.Set.fold
    (fun p () ->
       Location.prerr_warning loc
         (Warnings.Unboxable_type_in_prim_decl (Path.name p))
    )
    all_unboxable_types
    ()

let has_ty_var_with_layout_any ty =
  Ctype.exists_free_variable (fun _ jkind -> Jkind.has_layout_any jkind) ty

let unexpected_layout_any_check prim cty ty =
  if Primitive.prim_can_contain_layout_any prim ||
     prim.prim_is_layout_poly then ()
  else
  if has_ty_var_with_layout_any ty then
    raise(Error (cty.ctyp_loc,
            Unexpected_layout_any_in_primitive(prim.prim_name)))

(* Note regarding jkind checks on external declarations

   There are currently three checks in place:

   1. The argument/return types of an external can't have jkind [any]. This is
      enforced by [type_sort_external].

      The situation becomes trickier with the use of [@layout_poly]:

      1. we allow argument/return to have jkind [any] iff it's the layout
         polymorphic type variable.
      2. we use [Repr_poly] to encode it and we mark the primitive as
         [prim_is_layout_poly]
      3. all interactions with the declared primitive type have to go through
         [instance_prim], which instances the layout polymorphic type variable
         down from jkind [any] to a sort.

      The result is that we maintain the facade that all argument/return types
      are representable. The jkind [any] from [@layout_poly] doesn't leak out.

   2. [Primitive.prim_has_valid_reprs] performs an additional sanity check on
      built-in primitives regarding argument/result representations. It only
      allows a selected subset of primitives to have non-value jkinds. And for
      that subset, it checks to see the argument/return jkinds are what it
      expects.

      See comment in [prim_has_valid_reprs] about what it could miss.

   3. Built-in primitives that inspect the jkind of type parameters cannot have
      type variables with jkind [any] anywhere within their types.

      This check is here to prevent someone from writing:

      [external len : ('a : any). 'a array -> int = "%array_length"]

      If this is accepted, [len] will behave as expected most of the time until
      someone writes:

      [let f x = len x]

      [x] here will have type ['a array] where the jkind of ['a] is [any]. The
      array kind function in [typeopt] will look at ['a] expecting it to be
      representable and fail. This produces a bad error message that doesn't
      point to the source of the mistake which is, in fact, the external
      declaration.

      For this reason, we have [unexpected_layout_any_check].  It's here to
      point out this type of mistake early and suggest the use of
      [@layout_poly].

      An exception is raised if any of these checks fails. *)
(* CR layouts v7.1: additionally, we do not allow externals to have unboxed
   product args/returns. Right now this restriction is in place for all
   externals, but we should be able to relax it for some primitives that are
   implemented by the compiler, like %identity. Enforcement for [@layout_poly]
   primitives is tricky, because it is legal to, e.g., apply such a primitive to
   a sort variable, and that sort variable could subsequently be filled in by a
   product. So we rule out some things here, but others must be caught much
   later, in translprim.
*)
let error_if_containing_unexpected_jkind prim cty ty =
  Primitive.prim_has_valid_reprs ~loc:cty.ctyp_loc prim;
  unexpected_layout_any_check prim cty ty

(* [@@@zero_alloc assert all] in signatures uses the apparent arity of each
   declaration just by looking at the number of arrows in the type.  If the type
   is an alias to an arrow type, the apparent arity is zero, and the item won't
   get any zero alloc checking.  This is probably not the user's intent, so we
   give a warning in that case.  *)
let check_for_hidden_arrow env loc ty =
  match !Clflags.zero_alloc_assert with
  | Assert_all | Assert_all_opt ->
    let check () =
      begin match get_desc (Ctype.expand_head env ty) with
      | Tarrow _ ->
        let attr =
          match !Clflags.zero_alloc_assert with
          | Assert_all -> "all"
          | Assert_all_opt -> "all_opt"
          | Assert_default -> assert false
        in
        Location.prerr_warning loc (Warnings.Zero_alloc_all_hidden_arrow attr)
      | _ -> ()
      end
    in
    if !Clflags.principal || Env.has_local_constraints env then
      let snap = Btype.snapshot () in
      check ();
      Btype.backtrack snap
    else
      check()
  | Assert_default -> ()

(* Translate a value declaration *)
let transl_value_decl env loc ~modalities valdecl =
  let cty = Typetexp.transl_type_scheme env valdecl.pval_type in
  (* CR layouts v5: relax this to check for representability. *)
  begin match Ctype.constrain_type_jkind env cty.ctyp_type
                (Jkind.Builtin.value_or_null ~why:Structure_element) with
  | Ok () -> ()
  | Error err ->
    raise(Error(cty.ctyp_loc,
                Non_value_in_sig(err,valdecl.pval_name.txt,cty.ctyp_type)))
  end;
  let ty = cty.ctyp_type in
  let v =
  match valdecl.pval_prim with
    [] when Env.is_in_signature env ->
      let default_arity =
        let rec count_arrows n ty =
          match get_desc ty with
          | Tarrow (_, _, t2, _) -> count_arrows (n+1) t2
          | _ -> n
        in
        count_arrows 0 ty
      in
      let zero_alloc =
        Builtin_attributes.get_zero_alloc_attribute ~in_signature:true
          ~on_application:false
          ~default_arity valdecl.pval_attributes
      in
      let zero_alloc =
        match zero_alloc with
        | Default_zero_alloc ->
          (* We fabricate a "Check" attribute if a top-level annotation
             specifies that all functions should be checked for zero alloc. *)
          if default_arity = 0 then begin
            check_for_hidden_arrow env loc ty;
            Zero_alloc.default
          end else
            let create_const ~opt =
              Zero_alloc.create_const
                (Check { strict = false;
                         arity = default_arity;
                         custom_error_msg = None;
                         loc;
                         opt })
            in
            (match !Clflags.zero_alloc_assert with
             | Assert_default -> Zero_alloc.default
             | Assert_all -> create_const ~opt:false
             | Assert_all_opt -> create_const ~opt:true)
        | Ignore_assert_all -> Zero_alloc.ignore_assert_all
        | Check za ->
          if default_arity = 0 && za.arity <= 0 then
            raise (Error(valdecl.pval_loc, Zero_alloc_attr_non_function));
          if za.arity <= 0 then
            raise (Error(valdecl.pval_loc, Zero_alloc_attr_bad_user_arity));
          Zero_alloc.create_const zero_alloc
        | Assume _ ->
          raise (Error(valdecl.pval_loc, Zero_alloc_attr_unsupported zero_alloc))
      in
      { val_type = ty; val_kind = Val_reg; Types.val_loc = loc;
        val_attributes = valdecl.pval_attributes; val_modalities = modalities;
        val_zero_alloc = zero_alloc;
        val_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
      }
  | [] ->
      raise (Error(valdecl.pval_loc, Val_in_structure))
  | _ ->
      let global_repr =
        match
          get_native_repr_attribute valdecl.pval_attributes ~global_repr:None
        with
        | Native_repr_attr_present repr -> Some repr
        | Native_repr_attr_absent -> None
      in
      let is_layout_poly =
        Builtin_attributes.has_layout_poly valdecl.pval_attributes
      in
      if is_layout_poly &&
         not (has_ty_var_with_layout_any ty) then
        raise(Error(valdecl.pval_type.ptyp_loc, Useless_layout_poly));
      let native_repr_args, native_repr_res =
        parse_native_repr_attributes
          env valdecl.pval_type ty Prim_global ~global_repr ~is_layout_poly
      in
      let prim =
        Primitive.parse_declaration valdecl
          ~native_repr_args
          ~native_repr_res
          ~is_layout_poly
      in
      error_if_containing_unexpected_jkind prim cty ty;
      if prim.prim_arity = 0 &&
         (prim.prim_name = "" || prim.prim_name.[0] <> '%') then
        raise(Error(valdecl.pval_type.ptyp_loc, Null_arity_external));
      if !Clflags.native_code
      && prim.prim_arity > 5
      && prim.prim_native_name = ""
      then raise(Error(valdecl.pval_type.ptyp_loc, Missing_native_external));
      check_unboxable env loc ty;
      { val_type = ty; val_kind = Val_prim prim; Types.val_loc = loc;
        val_attributes = valdecl.pval_attributes; val_modalities = modalities;
        val_zero_alloc = Zero_alloc.default;
        val_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
      }
  in
  let (id, newenv) =
    Env.enter_value ~mode:Mode.Value.legacy valdecl.pval_name.txt v env
      ~check:(fun s -> Warnings.Unused_value_declaration s)
  in
  Ctype.check_and_update_generalized_ty_jkind ~name:id ~loc newenv ty;
  let desc =
    {
     val_id = id;
     val_name = valdecl.pval_name;
     val_desc = cty; val_val = v;
     val_prim = valdecl.pval_prim;
     val_loc = valdecl.pval_loc;
     val_attributes = valdecl.pval_attributes;
    }
  in
  desc, newenv

let transl_value_decl env ~modalities loc valdecl =
  Builtin_attributes.warning_scope valdecl.pval_attributes
    (fun () -> transl_value_decl env ~modalities loc valdecl)

(* Translate a "with" constraint -- much simplified version of
   transl_type_decl. For a constraint [Sig with t = sdecl],
   there are two declarations of interest in two environments:
   - [sig_decl] is the declaration of [t] in [Sig],
     in the environment [sig_env] (containing the declarations
     of [Sig] before [t])
   - [sdecl] is the new syntactic declaration, to be type-checked
     in the current, outer environment [with_env].

   In particular, note that [sig_env] is an extension of
   [outer_env].
*)
let transl_with_constraint id ?fixed_row_path ~sig_env ~sig_decl ~outer_env
    sdecl =
  Env.mark_type_used sig_decl.type_uid;
  Ctype.with_local_level begin fun () ->
  TyVarEnv.reset();
  (* In the first part of this function, we typecheck the syntactic
     declaration [sdecl] in the outer environment [outer_env]. *)
  let env = outer_env in
  let loc = sdecl.ptype_loc in
  let tparams = make_params env (Pident id) sdecl.ptype_params in
  let params = List.map (fun (cty, _) -> cty.ctyp_type) tparams in
  let arity = List.length params in
  let constraints =
    List.map (fun (ty, ty', loc) ->
      let cty =
        transl_simple_type ~new_var_jkind:Any env ~closed:false Mode.Alloc.Const.legacy ty
      in
      let cty' =
        transl_simple_type ~new_var_jkind:Sort env ~closed:false Mode.Alloc.Const.legacy ty'
      in
      (* Note: We delay the unification of those constraints
         after the unification of parameters, so that clashing
         constraints report an error on the constraint location
         rather than the parameter location. *)
      (cty, cty', loc)
    ) sdecl.ptype_cstrs
  in
  let no_row = not (is_fixed_type sdecl) in
  let (tman, man) =  match sdecl.ptype_manifest with
      None -> Misc.fatal_error "Typedecl.transl_with_constraint: no manifest"
    | Some sty ->
      let cty =
        transl_simple_type ~new_var_jkind:Any env ~closed:no_row Mode.Alloc.Const.legacy sty
      in
      cty, cty.ctyp_type
  in
  (* In the second part, we check the consistency between the two
     declarations and compute a "merged" declaration; we now need to
     work in the larger signature environment [sig_env], because
     [sig_decl.type_params] and [sig_decl.type_kind] are only valid
     there. *)
  let env = sig_env in
  let sig_decl = Ctype.instance_declaration sig_decl in
  let arity_ok = arity = sig_decl.type_arity in
  if arity_ok then
    List.iter2 (fun (cty, _) tparam ->
      try Ctype.unify_var env cty.ctyp_type tparam
      with Ctype.Unify err ->
        raise(Error(cty.ctyp_loc, Inconsistent_constraint (env, err)))
    ) tparams sig_decl.type_params;
  List.iter (fun (cty, cty', loc) ->
    (* Note: constraints must also be enforced in [sig_env] because
       they may contain parameter variables from [tparams]
       that have now be unified in [sig_env]. *)
    try Ctype.unify env cty.ctyp_type cty'.ctyp_type
    with Ctype.Unify err ->
      raise(Error(loc, Inconsistent_constraint (env, err)))
  ) constraints;
  let sig_decl_abstract = Btype.type_kind_is_abstract sig_decl in
  let priv =
    if sdecl.ptype_private = Private then Private else
    if arity_ok && not sig_decl_abstract
    then sig_decl.type_private else sdecl.ptype_private
  in
  if arity_ok && not sig_decl_abstract
  && sdecl.ptype_private = Private then
    Location.deprecated loc "spurious use of private";
  let type_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) in
  let type_unboxed_version =
    match get_desc man with
    | Tconstr (path, args, _) ->
      begin match Env.find_type path sig_env with
      | { type_unboxed_version = Some decl ; _ } ->
        let man = Ctype.newconstr (Path.unboxed_version path) args in
        let type_kind =
          match sig_decl.type_unboxed_version, arity_ok with
          | Some { type_kind ; _ }, true -> type_kind
          | None, _ | _, false -> Type_abstract Definition
        in
        let type_jkind = decl.type_jkind in
        Some {
          type_params = params;
          type_arity = arity;
          type_kind;
          type_jkind;
          type_private = priv;
          type_manifest = Some man;
          type_variance = [];
          type_separability = Types.Separability.default_signature ~arity;
          type_is_newtype = false;
          type_expansion_scope = Btype.lowest_level;
          type_loc = loc;
          type_attributes = decl.type_attributes;
          type_unboxed_default = false;
          type_uid = Uid.unboxed_version type_uid;
          type_unboxed_version = None;
        }
      | { type_unboxed_version = None ; _ } ->
        None
      | exception Not_found ->
        (* If [man] was translated already, this should be unreachable *)
        Misc.fatal_error "Typedecl.transl_with_constraint"
      end
    | _ -> None
  in
  let type_kind, type_unboxed_default, type_jkind =
    if arity_ok then
      sig_decl.type_kind,
      sig_decl.type_unboxed_default,
      sig_decl.type_jkind
    else
      Type_abstract Definition, false, sig_decl.type_jkind
  in
  let new_sig_decl =
    { type_params = params;
      type_arity = arity;
      type_kind;
      type_jkind;
      type_private = priv;
      type_manifest = Some man;
      type_variance = [];
      type_separability = Types.Separability.default_signature ~arity;
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = loc;
      type_attributes = sdecl.ptype_attributes;
      type_unboxed_default;
      type_uid;
      type_unboxed_version;
    }
  in
  Option.iter (fun p -> set_private_row env sdecl.ptype_loc p new_sig_decl)
    fixed_row_path;
  begin match Ctype.closed_type_decl new_sig_decl with None -> ()
  | Some ty -> raise(Error(loc, Unbound_type_var(ty, new_sig_decl)))
  end;
  let new_sig_decl = name_recursion sdecl id new_sig_decl in
  let new_type_variance =
    let required = Typedecl_variance.variance_of_params sdecl.ptype_params in
    try
      Typedecl_variance.compute_decl env ~check:(Some (id, false)) new_sig_decl
        required
    with Typedecl_variance.Error (loc, err) ->
      raise (Error (loc, Variance err)) in
  let new_type_separability =
    try Typedecl_separability.compute_decl env new_sig_decl
    with Typedecl_separability.Error (loc, err) ->
      raise (Error (loc, Separability err)) in
  let new_sig_decl =
    (* we intentionally write this without a fragile { decl with ... }
       to ensure that people adding new fields to type declarations
       consider whether they need to recompute it here; for an example
       of bug caused by the previous approach, see #9607 *)
    {
      type_params = new_sig_decl.type_params;
      type_arity = new_sig_decl.type_arity;
      type_kind = new_sig_decl.type_kind;
      type_jkind = new_sig_decl.type_jkind;
      type_private = new_sig_decl.type_private;
      type_manifest = new_sig_decl.type_manifest;
      type_unboxed_default = new_sig_decl.type_unboxed_default;
      type_is_newtype = new_sig_decl.type_is_newtype;
      type_expansion_scope = new_sig_decl.type_expansion_scope;
      type_loc = new_sig_decl.type_loc;
      type_attributes = new_sig_decl.type_attributes;
      type_uid = new_sig_decl.type_uid;

      (* For every recomputed field added below, consider if we also must
         recompute it for the unboxed version. *)
      type_variance = new_type_variance;
      type_separability = new_type_separability;
      type_unboxed_version =
        Option.map (fun d ->
          let type_variance =
            let required =
              Typedecl_variance.variance_of_params sdecl.ptype_params in
            try
              Typedecl_variance.compute_decl env ~check:(Some (id, true))
                d required
            with Typedecl_variance.Error (loc, err) ->
              raise (Error (loc, Variance err))
          in
          let type_separability =
            try
              Typedecl_separability.compute_decl env d
            with Typedecl_separability.Error (loc, err) ->
              raise (Error (loc, Separability err))
          in
          {
            d with
            type_variance;
            type_separability;
          })
        new_sig_decl.type_unboxed_version
    } in
  {
    typ_id = id;
    typ_name = sdecl.ptype_name;
    typ_params = tparams;
    typ_type = new_sig_decl;
    typ_cstrs = constraints;
    typ_loc = loc;
    typ_manifest = Some tman;
    typ_kind = Ttype_abstract;
    typ_private = sdecl.ptype_private;
    typ_attributes = sdecl.ptype_attributes;
    typ_jkind_annotation = Jkind.get_annotation type_jkind;
  }
  end
  ~post:(fun ttyp -> generalize_decl ttyp.typ_type)

(* A simplified version of [transl_with_constraint], for the case of packages.
   Package constraints are much simpler than normal with type constraints (e.g.,
   they can not have parameters and can only update abstract types.) *)
let transl_package_constraint ~loc ty =
  { type_params = [];
    type_arity = 0;
    type_kind = Type_abstract Definition;
    type_jkind = Jkind.Builtin.any ~why:Dummy_jkind;
    (* There is no reason to calculate an accurate jkind here.  This typedecl
       will be thrown away once it is used for the package constraint inclusion
       check, and that check will expand the manifest as needed. *)
    type_private = Public;
    type_manifest = Some ty;
    type_variance = [];
    type_separability = [];
    type_is_newtype = false;
    type_expansion_scope = Btype.lowest_level;
    type_loc = loc;
    type_attributes = [];
    type_unboxed_default = false;
    type_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
    type_unboxed_version = None;
  }

(* Approximate a type declaration: just make all types abstract *)

let abstract_type_decl ~injective ~jkind ~params =
  let arity = List.length params in
  Ctype.with_local_level ~post:generalize_decl begin fun () ->
    let params = List.map Ctype.newvar params in
    { type_params = params;
      type_arity = arity;
      type_kind = Type_abstract Definition;
      type_jkind = jkind;
      type_private = Public;
      type_manifest = None;
      type_variance = Variance.unknown_signature ~injective ~arity;
      type_separability = Types.Separability.default_signature ~arity;
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = Location.none;
      type_attributes = [];
      type_unboxed_default = false;
      type_uid = Uid.internal_not_actually_unique;
      type_unboxed_version =
        Some {
          type_params = params;
          type_arity = arity;
          type_kind = Type_abstract Definition;
          type_jkind = Jkind.Builtin.any ~why:Dummy_jkind;
          type_private = Public;
          type_manifest = None;
          type_variance = Variance.unknown_signature ~injective ~arity;
          type_separability = Types.Separability.default_signature ~arity;
          type_is_newtype = false;
          type_expansion_scope = Btype.lowest_level;
          type_loc = Location.none;
          type_attributes = [];
          type_unboxed_default = false;
          type_uid = Uid.internal_not_actually_unique;
          type_unboxed_version = None;
        };
    }
  end

let approx_type_decl sdecl_list =
  let scope = Ctype.create_scope () in
  List.map
    (fun sdecl ->
       let id = Ident.create_scoped ~scope sdecl.ptype_name.txt in
       let path = Path.Pident id in
       let injective = sdecl.ptype_kind <> Ptype_abstract in
       let transl_type sty =
         Misc.fatal_errorf
           "@[I do not yet know how to deal with [with]-types (such as %a)@ in \
            recursive modules. Please contact the Jane Street OCaml Language@ \
            team for help if you see this."
           Pprintast.core_type sty
       in
       let jkind =
         Jkind.of_type_decl_default
           ~context:(Type_declaration path)
           ~transl_type
           ~default:(Jkind.Builtin.value ~why:Default_type_jkind)
           sdecl
       in
       let params =
         List.map (fun (param, _) -> get_type_param_jkind path param)
           sdecl.ptype_params
       in
       (id, abstract_type_decl ~injective ~jkind ~params))
    sdecl_list

(* Check the well-formedness conditions on type abbreviations defined
   within recursive modules. *)

let check_recmod_typedecl env loc recmod_ids path decl =
  (* recmod_ids is the list of recursively-defined module idents.
     (path, decl) is the type declaration to be checked. *)
  let to_check path = Path.exists_free recmod_ids path in
  check_well_founded_decl ~abs_env:env env loc path decl to_check;
  check_unboxed_recursion_decl ~abs_env:env env loc path decl to_check;
  check_regularity ~abs_env:env env loc path decl to_check;
  (* additional coherence check, as one might build an incoherent signature,
     and use it to build an incoherent module, cf. #7851 *)
  (* Call just [check_kind_coherence], skipping [narrow_to_manifest_jkind].
     That call sometimes spuriously fails on
     valid programs (see test 14 in testsuite/tests/typing-jkind-bounds/basics.ml). This
     isn't sound, but this check is already unsound otherwise (see issue #13765)! And not
     performing this check on the jkinds is no less sound than what already exists. So
     instead of fixing the spurious failures, we choose to just not perform the check,
     with the intention of fixing the jkind soundness issue once the other soundness issue
     is resolved. *)
  check_kind_coherence env loc path decl


(**** Error report ****)

open Format
module Style = Misc.Style

let explain_unbound_gen ppf tv tl typ kwd pr =
  try
    let ti = List.find (fun ti -> Ctype.deep_occur tv (typ ti)) tl in
    let ty0 = (* Hack to force aliasing when needed *)
      Btype.newgenty (Tobject(tv, ref None)) in
    Printtyp.prepare_for_printing [typ ti; ty0];
    fprintf ppf
      ".@ @[<hov2>In %s@ %a@;<1 -2>the variable %a is unbound@]"
      kwd (Style.as_inline_code pr) ti
      (Style.as_inline_code Printtyp.prepared_type_expr) tv
  with Not_found -> ()

let explain_unbound ppf tv tl typ kwd lab =
  explain_unbound_gen ppf tv tl typ kwd
    (fun ppf ti ->
       fprintf ppf "%s%a" (lab ti) Printtyp.prepared_type_expr (typ ti)
    )

let explain_unbound_single ppf tv ty =
  let trivial ty =
    explain_unbound ppf tv [ty] (fun t -> t) "type" (fun _ -> "") in
  match get_desc ty with
    Tobject(fi,_) ->
      let (tl, rv) = Ctype.flatten_fields fi in
      if eq_type rv tv then trivial ty else
      explain_unbound ppf tv tl (fun (_,_,t) -> t)
        "method" (fun (lab,_,_) -> lab ^ ": ")
  | Tvariant row ->
      if eq_type (row_more row) tv then trivial ty else
      explain_unbound ppf tv (row_fields row)
        (fun (_l,f) -> match row_field_repr f with
          Rpresent (Some t) -> t
        | Reither (_,[t],_) -> t
        | Reither (_,tl,_) -> Btype.newgenty (Ttuple (List.map (fun e -> None, e) tl))
        | _ -> Btype.newgenty (Ttuple[]))
        "case" (fun (lab,_) -> "`" ^ lab ^ " of ")
  | _ -> trivial ty

module Reaching_path = struct
  type t = reaching_type_path

  (* Simplify a reaching path before showing it in error messages. *)
  let simplify path =
    let is_tconstr ty = match get_desc ty with Tconstr _ -> true | _ -> false in
    let rec simplify : t -> t = function
      | Contains (ty1, _ty2) :: Contains (ty2', ty3) :: rest
        when not (is_tconstr ty2') ->
          (* If t1 contains t2 and t2 contains t3, then t1 contains t3
             and we don't need to show t2. *)
          simplify (Contains (ty1, ty3) :: rest)
      | hd :: rest -> hd :: simplify rest
      | [] -> []
    in simplify path

  (* See Printtyp.add_type_to_preparation.

     Note: it is better to call this after [simplify], otherwise some
     type variable names may be used for types that are removed
     by simplification and never actually shown to the user.
  *)
  let add_to_preparation path =
    List.iter (function
      | Contains (ty1, ty2) | Expands_to (ty1, ty2) ->
          List.iter Printtyp.add_type_to_preparation [ty1; ty2]
    ) path

  let pp ppf reaching_path =
    let pp_step ppf = function
      | Expands_to (ty, body) ->
          Format.fprintf ppf "%a = %a"
            (Style.as_inline_code Printtyp.prepared_type_expr) ty
            (Style.as_inline_code Printtyp.prepared_type_expr) body
      | Contains (outer, inner) ->
          Format.fprintf ppf "%a contains %a"
            (Style.as_inline_code Printtyp.prepared_type_expr) outer
            (Style.as_inline_code Printtyp.prepared_type_expr) inner
    in
    let comma ppf () = Format.fprintf ppf ",@ " in
    Format.(pp_print_list ~pp_sep:comma pp_step) ppf reaching_path

  let pp_colon ppf path =
  Format.fprintf ppf ":@;<1 2>@[<v>%a@]"
    pp path
end

let report_jkind_mismatch_due_to_bad_inference ppf ty violation loc =
  let loc =
    match loc with
    | Check_constraints ->
      "final type declaration consistency check"
    | Delayed_checks ->
      "checking consistency of mutually recursive groups"
  in
  fprintf ppf
    "@[<v>Layout mismatch in %s.@ \
     This is most often caused by the fact that type inference is not@ \
     clever enough to propagate layouts through variables in different@ \
     declarations. It is also not clever enough to produce a good error@ \
     message, so we'll say this instead:@;<1 2>@[%a@]@ \
     A good next step is to add a layout annotation on a parameter to@ \
     the declaration where this error is reported.@]"
    loc
    (Jkind.Violation.report_with_offender
       ~offender:(fun ppf -> Printtyp.type_expr ppf ty)) violation

let report_error ppf = function
  | Repeated_parameter ->
      fprintf ppf "A type parameter occurs several times"
  | Duplicate_constructor s ->
      fprintf ppf "Two constructors are named %a" Style.inline_code s
  | Too_many_constructors ->
      fprintf ppf
        "@[Too many non-constant constructors@ -- maximum is %i %s@]"
        (Config.max_tag + 1) "non-constant constructors"
  | Duplicate_label s ->
      fprintf ppf "Two labels are named %a" Style.inline_code s
  | Unboxed_mutable_label ->
      fprintf ppf "Unboxed record labels cannot be mutable"
  | Recursive_abbrev (s, env, reaching_path) ->
      let reaching_path = Reaching_path.simplify reaching_path in
      Printtyp.wrap_printing_env ~error:true env @@ fun () ->
      Printtyp.reset ();
      Reaching_path.add_to_preparation reaching_path;
      fprintf ppf "@[<v>The type abbreviation %a is cyclic%a@]"
        Style.inline_code s
        Reaching_path.pp_colon reaching_path
  | Cycle_in_def (s, env, reaching_path) ->
      let reaching_path = Reaching_path.simplify reaching_path in
      Printtyp.wrap_printing_env ~error:true env @@ fun () ->
      Printtyp.reset ();
      Reaching_path.add_to_preparation reaching_path;
      fprintf ppf "@[<v>The definition of %a contains a cycle%a@]"
        Style.inline_code s
        Reaching_path.pp_colon reaching_path
  | Unboxed_recursion (s, env, reaching_path) ->
      let reaching_path = Reaching_path.simplify reaching_path in
      Printtyp.wrap_printing_env ~error:true env @@ fun () ->
      Printtyp.reset ();
      Reaching_path.add_to_preparation reaching_path;
      fprintf ppf "@[<v>The definition of %a is recursive without boxing%a@]"
        Style.inline_code s
        Reaching_path.pp_colon reaching_path
  | Definition_mismatch (ty, _env, None) ->
      fprintf ppf "@[<v>@[<hov>%s@ %s@;<1 2>%a@]@]"
        "This variant or record definition" "does not match that of type"
        (Style.as_inline_code Printtyp.type_expr) ty
  | Definition_mismatch (ty, env, Some err) ->
      fprintf ppf "@[<v>@[<hov>%s@ %s@;<1 2>%a@]%a@]"
        "This variant or record definition" "does not match that of type"
        (Style.as_inline_code Printtyp.type_expr) ty
        (Includecore.report_type_mismatch
           "the original" "this" "definition" env)
        err
  | Constraint_failed (env, err) ->
      let get_jkind_error : _ Errortrace.elt -> _ = function
      | Bad_jkind (ty, violation) | Bad_jkind_sort (ty, violation) ->
        Some (ty, violation)
      | Unequal_var_jkinds _ | Unequal_tof_kind_jkinds _ | Diff _ | Variant _
      | Obj _ | Escape _ | Incompatible_fields _ | Rec_occur _ -> None
      in
      begin match List.find_map get_jkind_error err.trace with
      | Some (ty, violation) ->
        report_jkind_mismatch_due_to_bad_inference ppf ty violation
          Check_constraints
      | None ->
      fprintf ppf "@[<v>Constraints are not satisfied in this type.@ ";
      Printtyp.report_unification_error ppf env err
        (fun ppf -> fprintf ppf "Type")
        (fun ppf -> fprintf ppf "should be an instance of");
      fprintf ppf "@]"
      end
  | Jkind_mismatch_due_to_bad_inference (ty, violation, loc) ->
      report_jkind_mismatch_due_to_bad_inference ppf ty violation loc
  | Non_regular { definition; used_as; defined_as; reaching_path } ->
      let reaching_path = Reaching_path.simplify reaching_path in
      let pp_type ppf ty = Style.as_inline_code !Oprint.out_type ppf ty in
      Printtyp.prepare_for_printing [used_as; defined_as];
      Reaching_path.add_to_preparation reaching_path;
      Printtyp.Naming_context.reset ();
      fprintf ppf
        "@[<hv>This recursive type is not regular.@ \
         The type constructor %a is defined as@;<1 2>type %a@ \
         but it is used as@;<1 2>%a%t\
         All uses need to match the definition for the recursive type \
         to be regular.@]"
        Style.inline_code (Path.name definition)
        pp_type (Printtyp.tree_of_typexp Type defined_as)
        pp_type (Printtyp.tree_of_typexp Type used_as)
        (fun pp ->
           let is_expansion = function Expands_to _ -> true | _ -> false in
           if List.exists is_expansion reaching_path then
             fprintf pp "@ after the following expansion(s)%a@ "
             Reaching_path.pp_colon reaching_path
           else fprintf pp ".@ ")
  | Inconsistent_constraint (env, err) ->
      fprintf ppf "@[<v>The type constraints are not consistent.@ ";
      Printtyp.report_unification_error ppf env err
        (fun ppf -> fprintf ppf "Type")
        (fun ppf -> fprintf ppf "is not compatible with type");
      fprintf ppf "@]"
  | Type_clash (env, err) ->
      Printtyp.report_unification_error ppf env err
        (function ppf ->
           fprintf ppf "This type constructor expands to type")
        (function ppf ->
           fprintf ppf "but is used here with type")
  | Null_arity_external ->
      fprintf ppf "External identifiers must be functions"
  | Missing_native_external ->
      fprintf ppf "@[<hv>An external function with more than 5 arguments \
                   requires a second stub function@ \
                   for native-code compilation@]"
  | Unbound_type_var (ty, decl) ->
      fprintf ppf "@[A type variable is unbound in this type declaration";
      begin match decl.type_kind, decl.type_manifest with
      | Type_variant (tl, _rep, _), _ ->
          explain_unbound_gen ppf ty tl (fun c ->
              let tl = tys_of_constr_args c.Types.cd_args in
              Btype.newgenty (Ttuple (List.map (fun t -> None, t) tl))
            )
            "case" (fun ppf c ->
              fprintf ppf
                "%a of %a" Printtyp.ident c.Types.cd_id
                Printtyp.constructor_arguments c.Types.cd_args)
      | Type_record (tl, _, _), _ ->
          explain_unbound ppf ty tl (fun l -> l.Types.ld_type)
            "field" (fun l -> Ident.name l.Types.ld_id ^ ": ")
      | Type_record_unboxed_product (tl, _, _), _ ->
          explain_unbound ppf ty tl (fun l -> l.Types.ld_type)
            "unboxed record field" (fun l -> Ident.name l.Types.ld_id ^ ": ")
      | Type_abstract _, Some ty' ->
          explain_unbound_single ppf ty ty'
      | _ -> ()
      end;
      fprintf ppf "@]"
  | Unbound_type_var_ext (ty, ext) ->
      fprintf ppf "@[A type variable is unbound in this extension constructor";
      let args = tys_of_constr_args ext.ext_args in
      explain_unbound ppf ty args (fun c -> c) "type" (fun _ -> "");
      fprintf ppf "@]"
  | Cannot_extend_private_type path ->
      fprintf ppf "@[%s@ %a@]"
        "Cannot extend private type definition"
        Printtyp.path path
  | Not_extensible_type path ->
      fprintf ppf "@[%s@ %a@ %s@]"
        "Type definition"
        (Style.as_inline_code Printtyp.path) path
        "is not extensible"
  | Extension_mismatch (path, env, err) ->
      fprintf ppf "@[<v>@[<hov>%s@ %s@;<1 2>%a@]%a@]"
        "This extension" "does not match the definition of type"
        Style.inline_code (Path.name path)
        (Includecore.report_type_mismatch
           "the type" "this extension" "definition" env)
        err
  | Rebind_wrong_type (lid, env, err) ->
      Printtyp.report_unification_error ppf env err
        (function ppf ->
           fprintf ppf "The constructor %a@ has type"
             (Style.as_inline_code Printtyp.longident) lid)
        (function ppf ->
           fprintf ppf "but was expected to be of type")
  | Rebind_mismatch (lid, p, p') ->
      fprintf ppf
        "@[%s@ %a@ %s@ %a@ %s@ %s@ %a@]"
        "The constructor"
        (Style.as_inline_code Printtyp.longident) lid
        "extends type" Style.inline_code (Path.name p)
        "whose declaration does not match"
        "the declaration of type" Style.inline_code (Path.name p')
  | Rebind_private lid ->
      fprintf ppf "@[%s@ %a@ %s@]"
        "The constructor"
        (Style.as_inline_code Printtyp.longident) lid
        "is private"
  | Variance (Typedecl_variance.Bad_variance (n, v1, v2)) ->
      let variance (p,n,i) =
        let inj = if i then "injective " else "" in
        match p, n with
          true,  true  -> inj ^ "invariant"
        | true,  false -> inj ^ "covariant"
        | false, true  -> inj ^ "contravariant"
        | false, false -> if inj = "" then "unrestricted" else inj
      in
      (match n with
       | Variance_variable_error { error; variable; context } ->
           Printtyp.prepare_for_printing [ variable ];
           Printtyp.Naming_context.reset ();
           begin match context with
           | Type_declaration { id ; decl ; unboxed_version } ->
               let pre, post =
                 if unboxed_version then
                   (* Unexpected; errors in the unboxed version should have also
                      been present and reported first for the boxed version. *)
                   "In the unboxed version of the definition",
                 "@ Please report this error to the Jane Street compilers team."
                 else
                   "In the definition", ""
               in
               Printtyp.add_type_declaration_to_preparation id decl;
               fprintf ppf "@[<v>%s@;<1 2>%a@;%s"
                 pre
                 (Style.as_inline_code @@ Printtyp.prepared_type_declaration id)
                 decl
                 post
           | Gadt_constructor c ->
               Printtyp.add_constructor_to_preparation c;
               fprintf ppf "@[<v>%s@;<1 2>%a@;"
                 "In the GADT constructor"
                 (Style.as_inline_code Printtyp.prepared_constructor)
                 c
           | Extension_constructor (id, e) ->
               Printtyp.add_extension_constructor_to_preparation e;
               fprintf ppf "@[<v>%s@;<1 2>%a@;"
                 "In the extension constructor"
                 (Printtyp.prepared_extension_constructor id)
                 e
           end;
           begin match error with
           | Variance_not_reflected ->
               fprintf ppf "@[%s@ %a@ %s@ %s@ It"
                 "the type variable"
                 (Style.as_inline_code Printtyp.prepared_type_expr) variable
                 "has a variance that"
                 "is not reflected by its occurrence in type parameters."
           | No_variable ->
               fprintf ppf "@[%s@ %a@ %s@ %s@]@]"
                 "the type variable"
                 (Style.as_inline_code Printtyp.prepared_type_expr) variable
                 "cannot be deduced"
                 "from the type parameters."
           | Variance_not_deducible ->
               fprintf ppf "@[%s@ %a@ %s@ %s@ It"
                 "the type variable"
                 (Style.as_inline_code Printtyp.prepared_type_expr) variable
                 "has a variance that"
                 "cannot be deduced from the type parameters."
           end
       | Variance_not_satisfied n ->
           fprintf ppf "@[@[%s@ %s@ The %d%s type parameter"
             "In this definition, expected parameter"
             "variances are not satisfied."
             n (Misc.ordinal_suffix n));
      (match n with
       | Variance_variable_error { error = No_variable; _ } -> ()
       | _ ->
           fprintf ppf " was expected to be %s,@ but it is %s.@]@]"
             (variance v2) (variance v1))
  | Unavailable_type_constructor p ->
      fprintf ppf "The definition of type %a@ is unavailable"
        (Style.as_inline_code Printtyp.path) p
  | Variance Typedecl_variance.Varying_anonymous ->
      fprintf ppf "@[%s@ %s@ %s@]"
        "In this GADT definition," "the variance of some parameter"
        "cannot be checked"
  | Val_in_structure ->
      fprintf ppf "Value declarations are only allowed in signatures"
  | Multiple_native_repr_attributes ->
      fprintf ppf "Too many %a/%a attributes"
        Style.inline_code "[@@unboxed]"
        Style.inline_code "[@@untagged]"
  | Cannot_unbox_or_untag_type Unboxed ->
      fprintf ppf "@[Don't know how to unbox this type.@ \
                   Only %a, %a, %a, %a, vector primitives, and@ \
                   the corresponding unboxed types can be marked unboxed.@]"
        Style.inline_code "float"
        Style.inline_code "int32"
        Style.inline_code "int64"
        Style.inline_code "nativeint"
  | Cannot_unbox_or_untag_type Untagged ->
      fprintf ppf "@[Don't know how to untag this type. Only %a, %a, %a, \
                   and@ other immediate types can be untagged.@]"
        Style.inline_code "int8"
        Style.inline_code "int16"
        Style.inline_code "int"
  | Deep_unbox_or_untag_attribute kind ->
      fprintf ppf
        "@[The attribute %a should be attached to@ \
         a direct argument or result of the primitive,@ \
         it should not occur deeply into its type.@]"
        Style.inline_code
        (match kind with Unboxed -> "@unboxed" | Untagged -> "@untagged")
  | Jkind_mismatch_of_path (dpath,v) ->
    (* the type is always printed just above, so print out just the head of the
       path instead of something like [t/3] *)
    let offender ppf =
      let head_name = Ident.name (Path.head dpath) in
      let path_end =
        if Path.is_unboxed_version dpath then head_name ^ "#" else head_name
      in
      fprintf ppf "type %a" Style.inline_code path_end
    in
    Jkind.Violation.report_with_offender ~offender ppf v
  | Jkind_mismatch_of_type (ty,v) ->
    let offender ppf = fprintf ppf "type %a"
        (Style.as_inline_code Printtyp.type_expr) ty in
    Jkind.Violation.report_with_offender ~offender ppf v
  | Jkind_sort {kloc; typ; err} ->
    let s =
      match kloc with
      | Mixed_product -> "Structures with non-value elements"
      | Cstr_tuple _ -> "Constructor argument types"
      | Inlined_record { unboxed = false }
      | Record { unboxed = false } -> "Record element types"
      | Inlined_record { unboxed = true }
      | Record { unboxed = true } -> "[@@unboxed] record element types"
      | Record_unboxed_product -> "Unboxed record element types"
      | External -> "Types in an external"
      | External_with_layout_poly -> "Types in an external"
    in
    let extra =
      match kloc with
      | Mixed_product | Cstr_tuple _ | Record _ | Inlined_record _ | External
      | Record_unboxed_product -> dprintf ""
      | External_with_layout_poly -> dprintf
        "@ (locally-scoped type variables with layout 'any' are@ \
          made representable by %a)"
        Style.inline_code "[@layout_poly]"
    in
    fprintf ppf "@[%s must have a representable layout%t.@ %a@]" s
      extra
      (Jkind.Violation.report_with_offender
         ~offender:(fun ppf -> Printtyp.type_expr ppf typ)) err
  | Jkind_empty_record ->
    fprintf ppf "@[Records must contain at least one runtime value.@]"
  | Non_value_in_sig (err, val_name, ty) ->
    let offender ppf = fprintf ppf "type %a" Printtyp.type_expr ty in
    fprintf ppf "@[This type signature for %a is not a value type.@ %a@]"
      Style.inline_code val_name
      (Jkind.Violation.report_with_offender ~offender) err
  | Invalid_jkind_in_block (typ, sort_const, lloc) ->
    let struct_desc =
      match lloc with
      | Mixed_product -> "Structures with non-value elements"
      | Inlined_record { unboxed = false } -> "Inlined records"
      | Inlined_record { unboxed = true } -> "[@@unboxed] inlined records"
      | Record { unboxed = false } -> "Records"
      | Record { unboxed = true }-> "[@@unboxed] records"
      | Record_unboxed_product -> "Unboxed records"
      | Cstr_tuple { unboxed = false } -> "Variants"
      | Cstr_tuple { unboxed = true } -> "Unboxed variants"
      | External | External_with_layout_poly -> assert false
    in
    fprintf ppf
      "@[Type %a has layout %a.@ %s may not yet contain types of this layout.@]"
      (Style.as_inline_code Printtyp.type_expr) typ
      (Style.as_inline_code Jkind.Sort.Const.format) sort_const
      struct_desc
  | Illegal_mixed_product error -> begin
      match error with
      | Runtime_support_not_enabled mixed_product_kind ->
          fprintf ppf
            "@[This OCaml runtime doesn't support mixed %s.@]"
            (Mixed_product_kind.to_plural_string mixed_product_kind)
      | Extension_constructor ->
          fprintf ppf
            "@[Extensible types can't have fields of unboxed type.@ Consider \
             wrapping the unboxed fields in a record.@]"
      | Value_prefix_too_long
          { value_prefix_len; max_value_prefix_len; mixed_product_kind } ->
          fprintf ppf
            "@[Mixed %s may contain at most %d value fields prior to the\
            \ flat suffix, but this one contains %d.@]"
            (Mixed_product_kind.to_plural_string mixed_product_kind)
            max_value_prefix_len value_prefix_len
      | Insufficient_level { required_layouts_level; mixed_product_kind } -> (
        let hint ppf =
          Format.fprintf ppf "You must enable -extension %s to use this feature."
            (Language_extension.to_command_line_string Layouts
               required_layouts_level)
        in
        match Language_extension.is_enabled Layouts with
        | false ->
          fprintf ppf
            "@[<v>The appropriate layouts extension is not enabled.@;%t@]" hint
        | true ->
          fprintf ppf
            "@[<v>The enabled layouts extension does not allow for mixed %s.@;\
             %t@]"
            (Mixed_product_kind.to_plural_string mixed_product_kind)
            hint)
    end
  | Bad_unboxed_attribute msg ->
      fprintf ppf "@[This type cannot be unboxed because@ %s.@]" msg
  | Separability (Typedecl_separability.Non_separable_evar evar) ->
      let pp_evar ppf = function
        | None ->
            fprintf ppf "an unnamed existential variable"
        | Some str ->
            fprintf ppf "the existential variable %a"
              (Style.as_inline_code Pprintast.tyvar) str in
      fprintf ppf "@[This type cannot be unboxed because@ \
                   it might contain both float and non-float values,@ \
                   depending on the instantiation of %a.@ \
                   You should annotate it with %a.@]"
        pp_evar evar
        Style.inline_code "[@@ocaml.boxed]"
  | Boxed_and_unboxed ->
      fprintf ppf "@[A type cannot be boxed and unboxed at the same time.@]"
  | Nonrec_gadt ->
      fprintf ppf
        "@[GADT case syntax cannot be used in a %a block.@]"
        Style.inline_code "nonrec"
  | Invalid_private_row_declaration ty ->
      let pp_private ppf ty = fprintf ppf "private %a" Printtyp.type_expr ty in
      Format.fprintf ppf
        "@[<hv>This private row type declaration is invalid.@ \
         The type expression on the right-hand side reduces to@;<1 2>%a@ \
         which does not have a free row type variable.@]@,\
         @[<hv>@[@{<hint>Hint@}: If you intended to define a private \
         type abbreviation,@ \
        write explicitly@]@;<1 2>%a@]"
        (Style.as_inline_code Printtyp.type_expr) ty
        (Style.as_inline_code pp_private) ty
  | Local_not_enabled ->
      fprintf ppf "@[The local extension is disabled@ \
                   To enable it, pass the '-extension local' flag@]"
  | Unexpected_layout_any_in_primitive name ->
      fprintf ppf
        "@[The primitive %a doesn't work well with type variables of@ \
           layout any. Consider using %a.@]"
        Style.inline_code name
        Style.inline_code "[@layout_poly]"
  | Useless_layout_poly ->
      fprintf ppf
        "@[%a on this external declaration has no@ \
           effect. Consider removing it or adding a type@ \
           variable for it to operate on.@]"
        Style.inline_code "[@layout_poly]"
  | Zero_alloc_attr_unsupported ca ->
      let variety = match ca with
        | Default_zero_alloc  | Check _ -> assert false
        | Assume _ -> "assume"
        | Ignore_assert_all -> "ignore"
      in
      fprintf ppf
        "@[zero_alloc %a attributes are not supported in signatures@]"
        Style.inline_code variety
  | Zero_alloc_attr_non_function ->
    fprintf ppf
      "@[In signatures, zero_alloc is only supported on function declarations.\
         @ Found no arrows in this declaration's type.\
         @ Hint: You can write %a to specify the arity\
         @ of an alias (for n > 0).@]"
      Style.inline_code "[@zero_alloc arity n]"
  | Zero_alloc_attr_bad_user_arity ->
    fprintf ppf
      "@[Invalid zero_alloc attribute: arity must be greater than 0.@]"
  | Invalid_reexport {definition; expected} ->
    fprintf ppf
      "@[Invalid reexport declaration.\
         @ Type %s must be defined equal to the primitive type %a.@]"
      (Path.name definition) Printtyp.path expected
  | Non_abstract_reexport definition ->
    fprintf ppf
      "@[Invalid reexport declaration.\
         @ Type %s must not define an explicit representation.@]"
      (Path.name definition)
  | Unsafe_mode_crossing_on_invalid_type_kind ->
    fprintf ppf
      "@[[%@%@unsafe_allow_any_mode_crossing] is not allowed on this kind of \
       type declaration.@ Only records, unboxed products, and variants are \
       supported.@]"
  | Illegal_baggage jkind ->
    fprintf ppf
      "@[Illegal %a in kind annotation of an abbreviation:@ %a@]"
      Style.inline_code "with" Jkind.format jkind
  | No_unboxed_version p ->
      fprintf ppf "@[The type %a@ has no unboxed version.@]"
        (Style.as_inline_code Printtyp.path) p

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
