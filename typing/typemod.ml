(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Longident
open Path
open Asttypes
open Parsetree
open Types
open Mode
open Format

module Style = Misc.Style

let () = Includemod_errorprinter.register ()

module Sig_component_kind = Shape.Sig_component_kind
module String = Misc.Stdlib.String

type hiding_error =
  | Illegal_shadowing of {
      shadowed_item_id: Ident.t;
      shadowed_item_kind: Sig_component_kind.t;
      shadowed_item_loc: Location.t;
      shadower_id: Ident.t;
      user_id: Ident.t;
      user_kind: Sig_component_kind.t;
      user_loc: Location.t;
    }
  | Appears_in_signature of {
      opened_item_id: Ident.t;
      opened_item_kind: Sig_component_kind.t;
      user_id: Ident.t;
      user_kind: Sig_component_kind.t;
      user_loc: Location.t;
    }

type functor_dependency_error =
    Functor_applied
  | Functor_included

type legacy_module =
  | Compilation_unit
  | Toplevel
  | Functor_body

let print_legacy_module ppf = function
  | Compilation_unit -> Format.fprintf ppf "compilation unit"
  | Functor_body -> Format.fprintf ppf "functor body"
  | Toplevel -> Format.fprintf ppf "toplevel"

type unsupported_modal_module =
  | Functor_param
  | Functor_res

let print_unsupported_modal_module ppf = function
  | Functor_param -> Format.fprintf ppf "functor parameters"
  | Functor_res -> Format.fprintf ppf "functor return"

type error =
    Cannot_apply of module_type
  | Not_included of Includemod.explanation
  | Not_included_functor of Includemod.explanation
  | Cannot_eliminate_dependency of functor_dependency_error * module_type
  | Signature_expected
  | Structure_expected of module_type
  | Functor_expected of module_type
  | Signature_parameter_expected of module_type
  | Signature_result_expected of module_type
  | Recursive_include_functor
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.explanation
  | With_makes_applicative_functor_ill_typed of
      Longident.t * Path.t * Includemod.explanation
  | With_changes_module_alias of Longident.t * Ident.t * Path.t
  | With_cannot_remove_constrained_type
  | With_package_manifest of Longident.t * type_expr
  | Repeated_name of Sig_component_kind.t * string
  | Non_generalizable of { vars : type_expr list; expression : type_expr }
  | Non_generalizable_module of
      { vars : type_expr list; item : value_description; mty : module_type }
  | Implementation_is_required of string
  | Interface_not_compiled of string
  | Not_allowed_in_functor_body
  | Not_includable_in_functor_body
  | Not_a_packed_module of type_expr
  | Incomplete_packed_module of type_expr
  | Scoping_pack of Longident.t * type_expr
  | Recursive_module_require_explicit_type
  | Apply_generative
  | Cannot_scrape_alias of Path.t
  | Cannot_scrape_package_type of Path.t
  | Badly_formed_signature of string * Typedecl.error
  | Cannot_hide_id of hiding_error
  | Invalid_type_subst_rhs
  | Non_packable_local_modtype_subst of Path.t
  | With_cannot_remove_packed_modtype of Path.t * module_type
  | Toplevel_nonvalue of string * Jkind.sort
  | Toplevel_unnamed_nonvalue of Jkind.sort
  | Strengthening_mismatch of Longident.t * Includemod.explanation
  | Cannot_pack_parameter
  | Compiling_as_parameterised_parameter
  | Cannot_compile_implementation_as_parameter
  | Cannot_implement_parameter of Compilation_unit.Name.t * Misc.filepath
  | Argument_for_non_parameter of Global_module.Name.t * Misc.filepath
  | Cannot_find_argument_type of Global_module.Parameter_name.t
  | Inconsistent_argument_types of {
      new_arg_type : Global_module.Parameter_name.t option;
      old_arg_type : Global_module.Parameter_name.t option;
      old_source_file : Misc.filepath;
    }
  | Duplicate_parameter_name of Global_module.Parameter_name.t
  | Submode_failed of Mode.Value.error
  | Item_weaker_than_structure of Mode.Value.error
  | Unsupported_modal_module of unsupported_modal_module
  | Legacy_module of legacy_module * Mode.Value.error

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

let submode ~loc ~env mode expected_mode =
  match Value.submode mode expected_mode with
  | Ok () -> ()
  | Error e -> raise (Error (loc, env, Submode_failed e))

let new_mode_var_from_annots (m : Alloc.Const.Option.t) =
  let mode = Mode.Value.newvar () in
  let min = Alloc.Const.Option.value ~default:Alloc.Const.min m in
  let max = Alloc.Const.Option.value ~default:Alloc.Const.max m in
  Value.submode_exn (min |> Alloc.of_const |> alloc_as_value) mode;
  Value.submode_exn mode (max |> Alloc.of_const |> alloc_as_value);
  mode

let register_allocation () =
  let m, _ =
    Alloc.(newvar_below
      (max_with_comonadic Areality Locality.global))
  in
  m, alloc_as_value m

open Typedtree

let rec path_concat head p =
  match p with
    Pident tail -> Pdot (Pident head, Ident.name tail)
  | Pdot (pre, s) -> Pdot (path_concat head pre, s)
  | Papply _ -> assert false
  | Pextra_ty (p, extra) -> Pextra_ty (path_concat head p, extra)

(* Extract a signature from a module type *)

let extract_sig env loc mty =
  match Mtype.scrape_alias env mty with
    Mty_signature sg -> sg
  | Mty_alias path ->
      raise(Error(loc, env, Cannot_scrape_alias path))
  | _ -> raise(Error(loc, env, Signature_expected))

let extract_sig_open env loc mty =
  match Mtype.scrape_alias env mty with
    Mty_signature sg -> sg
  | Mty_alias path ->
      raise(Error(loc, env, Cannot_scrape_alias path))
  | mty -> raise(Error(loc, env, Structure_expected mty))

(* Extract the signature of a functor's body, using the provided [sig_acc]
   signature to fill in names from its parameter *)
let extract_sig_functor_open funct_body env loc mty sig_acc =
  let sig_acc = List.rev sig_acc in
  match Mtype.scrape_alias env mty with
  | Mty_functor (Named (param, mty_param),mty_result) as mty_func ->
      let sg_param =
        match Mtype.scrape env mty_param with
        | Mty_signature sg_param -> sg_param
        | _ -> raise (Error (loc,env,Signature_parameter_expected mty_func))
      in
      let coercion =
        try
          Includemod.include_functor_signatures ~mark:true env
            sig_acc sg_param
        with Includemod.Error msg ->
          raise (Error(loc, env, Not_included_functor msg))
      in
      (* We must scrape the result type in an environment expanded with the
         parameter type (to avoid `Not_found` exceptions when it is referenced).
         Because we don't have an actual parameter, we create definitions for
         the parameter's types with [sig_make_manifest].  References to this
         fake parameter are eliminated later.  *)
      let extended_env =
        match param with
        | None -> env
        | Some id ->
          let sg_param = Mtype.sig_make_manifest sig_acc in
          Env.add_module ~arg:true id Mp_present (Mty_signature sg_param) env
      in
      let incl_kind, sg_result =
        (* Accept functor types of the forms:
              sig..end -> sig..end
           and
              sig..end -> () -> sig..end *)
        match Mtype.scrape extended_env mty_result with
        | Mty_signature sg_result -> Tincl_functor coercion, sg_result
        | Mty_functor (Unit,_) when funct_body && Mtype.contains_type env mty ->
            raise (Error (loc, env, Not_includable_in_functor_body))
        | Mty_functor (Unit,mty_result) -> begin
            match Mtype.scrape extended_env mty_result with
            | Mty_signature sg_result -> Tincl_gen_functor coercion, sg_result
            | sg -> raise (Error (loc,env,Signature_result_expected
                                            (Mty_functor (Unit,sg))))
          end
        | sg -> raise (Error (loc,env,Signature_result_expected sg))
      in
      (* Here we eliminate references to the non-existent parameter module using
         [nondep_sig]. *)
      let sg =
        match param with
        | None -> sg_result
        | Some id ->
          try Mtype.nondep_sig extended_env [id] sg_result
          with Ctype.Nondep_cannot_erase _ ->
            raise(Error(loc, env, Cannot_eliminate_dependency
                                    (Functor_included, mty_func)))
      in
      (sg, incl_kind)
  | Mty_functor (Unit,_) as mty ->
      raise(Error(loc, env, Signature_parameter_expected mty))
  | Mty_alias path -> raise(Error(loc, env, Cannot_scrape_alias path))
  | mty -> raise(Error(loc, env, Functor_expected mty))

(* Compute the environment after opening a module *)

let type_open_ ?(used_slot=ref false) ?(toplevel=false) ovf env loc lid =
  Env.open_signature ~loc ~used_slot ~toplevel ovf lid env

let initial_env ~loc ~initially_opened_module
    ~open_implicit_modules =
  let env = Lazy.force Env.initial in
  let open_module env m =
    let open Asttypes in
    let lexbuf = Lexing.from_string m in
    let txt =
      Location.init lexbuf (Printf.sprintf "command line argument: -open %S" m);
      Parse.simple_module_path lexbuf
    in
    let _, _, env = type_open_ Override env loc {txt;loc} in
    env
  in
  let add_units env units =
    String.Set.fold
      (fun name env ->
         Env.add_persistent_structure (Ident.create_persistent name) env)
      units
      env
  in
  let units =
    List.map Env.persistent_structures_of_dir (Load_path.get_visible ())
  in
  let env, units =
    match initially_opened_module with
    | None -> (env, units)
    | Some m ->
        (* Locate the directory that contains [m], adds the units it
           contains to the environment and open [m] in the resulting
           environment. *)
        let rec loop before after =
          match after with
          | [] -> None
          | units :: after ->
              if String.Set.mem m units then
                Some (units, List.rev_append before after)
              else
                loop (units :: before) after
        in
        let env, units =
          match loop [] units with
          | None ->
              (env, units)
          | Some (units_containing_m, other_units) ->
              (add_units env units_containing_m, other_units)
        in
        (open_module env m, units)
  in
  let env = List.fold_left add_units env units in
  List.fold_left open_module env open_implicit_modules

let type_open_descr ?used_slot ?toplevel env sod =
  let (path, _, newenv) =
    Builtin_attributes.warning_scope sod.popen_attributes
      (fun () ->
         type_open_ ?used_slot ?toplevel sod.popen_override env sod.popen_loc
           sod.popen_expr
      )
  in
  let od =
    {
      open_expr = (path, sod.popen_expr);
      open_bound_items = [];
      open_override = sod.popen_override;
      open_env = newenv;
      open_attributes = sod.popen_attributes;
      open_loc = sod.popen_loc;
    }
  in
  (od, newenv)

(* Forward declaration, to be filled in by type_module_type_of *)
let type_module_type_of_fwd :
    (Env.t -> Parsetree.module_expr ->
      Typedtree.module_expr * Types.module_type) ref
  = ref (fun _env _m -> assert false)

(* Additional validity checks on type definitions arising from
   recursive modules *)

let check_recmod_typedecls env decls =
  let recmod_ids = List.map fst decls in
  List.iter
    (fun (id, md) ->
      List.iter
        (fun path ->
          Typedecl.check_recmod_typedecl env md.Types.md_loc recmod_ids
                                         path (Env.find_type path env))
        (Mtype.type_paths env (Pident id) md.Types.md_type))
    decls

(* Merge one "with" constraint in a signature *)

let check_type_decl env sg loc id row_id newdecl decl =
  let fresh_id = Ident.rename id in
  let path = Pident fresh_id in
  let sub = Subst.add_type id path Subst.identity in
  let fresh_row_id, sub =
    match row_id with
    | None -> None, sub
    | Some id ->
      let fresh_row_id = Some (Ident.rename id) in
      let sub = Subst.add_type id (Pident fresh_id) sub in
      fresh_row_id, sub
  in
  let newdecl = Subst.type_declaration sub newdecl in
  let decl = Subst.type_declaration sub decl in
  let sg = List.map (Subst.signature_item Keep sub) sg in
  let env = Env.add_type ~check:false fresh_id newdecl env in
  let env =
    match fresh_row_id with
    | None -> env
    | Some fresh_row_id -> Env.add_type ~check:false fresh_row_id newdecl env
  in
  let env = Env.add_signature sg env in
  Includemod.type_declarations ~mark:true ~loc env fresh_id newdecl decl;
  Typedecl.check_coherence env loc path newdecl
    (* The use of [check_coherence] here skips the manifest subkind check
       in [narrow_to_manifest_jkind], but that's ok, because this check already
       has happened in [Includemod.type_declarations]. *)

let make_variance p n i =
  let open Variance in
  set_if p May_pos (set_if n May_neg (set_if i Inj null))

let rec iter_path_apply p ~f =
  match p with
  | Pident _ -> ()
  | Pdot (p, _) -> iter_path_apply p ~f
  | Papply (p1, p2) ->
     iter_path_apply p1 ~f;
     iter_path_apply p2 ~f;
     f p1 p2 (* after recursing, so we know both paths are well typed *)
  | Pextra_ty (p, t) ->
     match t with
     | Punboxed_ty -> iter_path_apply p ~f
     | Pcstr_ty _ | Pext_ty -> assert false

let path_is_strict_prefix =
  let rec list_is_strict_prefix l ~prefix =
    match l, prefix with
    | [], [] -> false
    | _ :: _, [] -> true
    | [], _ :: _ -> false
    | s1 :: t1, s2 :: t2 ->
       String.equal s1 s2 && list_is_strict_prefix t1 ~prefix:t2
  in
  fun path ~prefix ->
    match Path.flatten path, Path.flatten prefix with
    | `Contains_apply, _ | _, `Contains_apply -> false
    | `Ok (ident1, l1), `Ok (ident2, l2) ->
       Ident.same ident1 ident2
       && list_is_strict_prefix l1 ~prefix:l2

let rec instance_name ~loc env syntax =
  let { pmod_instance_head = head; pmod_instance_args = args } = syntax in
  let args =
    List.map
      (fun (param, value) : Global_module.Name.argument ->
         { param = Global_module.Parameter_name.of_string param;
           value = instance_name ~loc env value })
      args
  in
  match Global_module.Name.create head args with
  | Ok name -> name
  | Error (Duplicate { name; value1 = _; value2 = _ }) ->
    raise (Error (loc, env, Duplicate_parameter_name name))

let iterator_with_env env =
  let env = ref (lazy env) in
  let super = Btype.type_iterators in
  env, { super with
    Btype.it_signature = (fun self sg ->
      (* add all items to the env before recursing down, to handle recursive
         definitions *)
      let env_before = !env in
      env := lazy (Env.add_signature sg (Lazy.force env_before));
      super.Btype.it_signature self sg;
      env := env_before
    );
    Btype.it_module_type = (fun self -> function
    | Mty_functor (param, mty_body) ->
      let env_before = !env in
      begin match param with
      | Unit -> ()
      | Named (param, mty_arg) ->
        self.Btype.it_module_type self mty_arg;
        match param with
        | None -> ()
        | Some id ->
          env := lazy (Env.add_module ~arg:true id Mp_present
                       mty_arg (Lazy.force env_before))
      end;
      self.Btype.it_module_type self mty_body;
      env := env_before;
    | mty ->
      super.Btype.it_module_type self mty
    )
  }

let retype_applicative_functor_type ~loc env funct arg =
  let mty_functor = (Env.find_module funct env).md_type in
  let mty_arg = (Env.find_module arg env).md_type in
  let mty_param =
    match Mtype.scrape_alias env mty_functor with
    | Mty_functor (Named (_, mty_param), _) -> mty_param
    | _ -> assert false (* could trigger due to MPR#7611 *)
  in
  Includemod.check_functor_application ~loc env mty_arg arg mty_param

(* When doing a deep destructive substitution with type M.N.t := .., we change M
   and M.N and so we have to check that uses of the modules other than just
   extracting components from them still make sense. There are only two such
   kinds of uses:
   - applicative functor types: F(M).t might not be well typed anymore
   - aliases: module A = M still makes sense but it doesn't mean the same thing
     anymore, so it's forbidden until it's clear what we should do with it.
   This function would be called with M.N.t and N.t to check for these uses. *)
let check_usage_of_path_of_substituted_item paths ~loc ~lid env super =
    { super with
      Btype.it_signature_item = (fun self -> function
      | Sig_module (id, _, { md_type = Mty_alias aliased_path; _ }, _, _)
        when List.exists
               (fun path -> path_is_strict_prefix path ~prefix:aliased_path)
               paths
        ->
         let e = With_changes_module_alias (lid.txt, id, aliased_path) in
         raise(Error(loc, Lazy.force !env, e))
      | sig_item ->
         super.Btype.it_signature_item self sig_item
      );
      Btype.it_path = (fun referenced_path ->
        iter_path_apply referenced_path ~f:(fun funct arg ->
          if List.exists
               (fun path -> path_is_strict_prefix path ~prefix:arg)
               paths
          then
            let env = Lazy.force !env in
            match retype_applicative_functor_type ~loc env funct arg with
            | None -> ()
            | Some explanation ->
                raise(Error(loc, env,
                            With_makes_applicative_functor_ill_typed
                            (lid.txt, referenced_path, explanation)))
        )
      );
    }

let do_check_after_substitution env ~loc ~lid paths sg =
  let env, iterator = iterator_with_env env in
  let last, rest = match List.rev paths with
    | [] -> assert false
    | last :: rest -> last, rest
  in
  (* The last item is the one that's removed. We don't need to check how
        it's used since it's replaced by a more specific type/module. *)
  assert (match last with Pident _ -> true | _ -> false);
  let iterator = match rest with
    | [] -> iterator
    | _ :: _ ->
        check_usage_of_path_of_substituted_item rest ~loc ~lid env iterator
  in
  iterator.Btype.it_signature iterator sg;
  Btype.(unmark_iterators.it_signature unmark_iterators) sg

let check_usage_after_substitution env ~loc ~lid paths sg =
  match paths with
  | [_] -> ()
  | _ -> do_check_after_substitution env ~loc ~lid paths sg

(* After substitution one also needs to re-check the well-foundedness
   of type declarations in recursive modules *)
let rec extract_next_modules = function
  | Sig_module (id, _, mty, Trec_next, _) :: rem ->
      let (id_mty_l, rem) = extract_next_modules rem in
      ((id, mty) :: id_mty_l, rem)
  | sg -> ([], sg)

let check_well_formed_module env loc context mty =
  (* Format.eprintf "@[check_well_formed_module@ %a@]@."
     Printtyp.modtype mty; *)
  let open Btype in
  let iterator =
    let rec check_signature env = function
      | [] -> ()
      | Sig_module (id, _, mty, Trec_first, _) :: rem ->
          let (id_mty_l, rem) = extract_next_modules rem in
          begin try
            check_recmod_typedecls (Lazy.force env) ((id, mty) :: id_mty_l)
          with Typedecl.Error (_, err) ->
            raise (Error (loc, Lazy.force env,
                          Badly_formed_signature(context, err)))
          end;
          check_signature env rem
      | _ :: rem ->
          check_signature env rem
    in
    let env, super = iterator_with_env env in
    { super with
      it_type_expr = (fun self ty ->
        (* Check that an unboxed path is valid because substitutions can
           remove an unboxed version of a type.
           See [tests/typing-layouts/hash_types.ml]. *)
        begin match get_desc ty with
        | Tconstr (Pextra_ty(path, Punboxed_ty) as path_unboxed, _, _) ->
          let env = Lazy.force !env in
          begin try ignore (Env.find_type path_unboxed env) with
          | Not_found ->
            let err =
              Badly_formed_signature(context, Typedecl.No_unboxed_version path)
            in
            raise (Error (loc, env, err))
          end
        | _ -> ()
        end;
        super.it_type_expr self ty
      );
      it_type_declaration = (fun self td ->
        (* Optimization: the above check on [type_expr]s doesn't need to check
           unboxed versions, so we override [it_type_declaration] to skip
           [td.type_unboxed_version]. *)
        List.iter (self.it_type_expr self) td.type_params;
        Option.iter (self.it_type_expr self) td.type_manifest;
        self.it_type_kind self td.type_kind
      );
      it_signature = (fun self sg ->
        let env_before = !env in
        let env = lazy (Env.add_signature sg (Lazy.force env_before)) in
        check_signature env sg;
        super.it_signature self sg);
    }
  in
  iterator.it_module_type iterator mty;
  Btype.(unmark_iterators.it_module_type unmark_iterators) mty

let () = Env.check_well_formed_module := check_well_formed_module

let type_decl_is_alias sdecl = (* assuming no explicit constraint *)
  let eq_vars x y =
    (* Why not handle jkind annotations?

       a jkind annotation on either type variable might mean this definition
       is not an alias. Example: {v
         type ('a : value) t
         type ('a : immediate) t2 = ('a : immediate) t
       v}
       But the only way to know that t2 isn't an alias is to look at
       jkinds in the environment, which is hard to do here. So we
       conservatively say that any jkind annotations block alias
       detection.
    *)
    match x.ptyp_desc, y.ptyp_desc with
    | Ptyp_var (sx, None), Ptyp_var (sy, None) -> sx = sy
    | _, _ -> false
  in
  match sdecl.ptype_manifest with
  | Some {ptyp_desc = Ptyp_constr (lid, stl)}
       when List.length stl = List.length sdecl.ptype_params ->
    if List.for_all2 (fun x (y, _) -> eq_vars x y) stl sdecl.ptype_params
    then Some lid
    else None
  | _ -> None

let params_are_constrained =
  let rec loop = function
    | [] -> false
    | hd :: tl ->
       match get_desc hd with
       | Tvar _ -> List.memq hd tl || loop tl
       | _ -> true
  in
  loop

let rec remove_modality_and_zero_alloc_variables_sg env ~zap_modality sg =
  let sg_item = function
    | Sig_value (id, desc, vis) ->
        let val_modalities =
          desc.val_modalities
          |> zap_modality |> Mode.Modality.Value.of_const
        in
        let val_zero_alloc =
          Zero_alloc.create_const (Zero_alloc.get desc.val_zero_alloc)
        in
        let desc = {desc with val_modalities; val_zero_alloc} in
        Sig_value (id, desc, vis)
    | Sig_module (id, pres, md, re, vis) ->
        let md_type =
          remove_modality_and_zero_alloc_variables_mty env ~zap_modality
            md.md_type
        in
        let md_modalities =
          md.md_modalities |> zap_modality |> Mode.Modality.Value.of_const
        in
        let md = {md with md_type; md_modalities} in
        Sig_module (id, pres, md, re, vis)
    | item -> item
  in
  List.map sg_item sg

and remove_modality_and_zero_alloc_variables_mty env ~zap_modality mty =
  match mty with
  | Mty_ident _ | Mty_alias _ ->
    (* module types with names can't have inferred modalities. *)
    mty
  | Mty_signature sg ->
    Mty_signature
      (remove_modality_and_zero_alloc_variables_sg env ~zap_modality sg)
  | Mty_functor (param, mty) ->
    let param : Types.functor_parameter =
      match param with
      | Named (id, mty) ->
          let mty =
            remove_modality_and_zero_alloc_variables_mty env
              ~zap_modality:Mode.Modality.Value.to_const_exn mty
          in
          Named (id, mty)
      | Unit -> Unit
    in
    let mty =
      remove_modality_and_zero_alloc_variables_mty env ~zap_modality mty
    in
    Mty_functor (param, mty)
  | Mty_strengthen (mty, path, alias) ->
      let mty =
        remove_modality_and_zero_alloc_variables_mty env
        ~zap_modality:Mode.Modality.Value.to_const_exn mty
      in
      Mty_strengthen (mty, path, alias)


module Merge = struct
  (** This module hosts the functions dealing with signature constraints. There
     are of three forms :
     - type constraint [... with type t = ... ], handled by [merge_type]
     - module constraint [... with module X = ... ], handled by [merge_module]
     - module type constraints [... with module type T = ...] handled by
       [merge_modtype]

     Each constraint can be *destructive*, (with the syntax [:=]) meaning that
     the substituted identifier is removed from the signature. This imposes
     additional checks to ensure wellformedness, handled by the [post_process]
     function.

     Each constraint can be *deep*, meaning that the substituted identifier
     might be inside a submodule. This is handled by the [patch_deep_item]
     function. Deep destructive substitutions inside submodules with an alias
     signature are disallowed.

     Each constraint can be *instantiating*, if the substitution replaces an
     "abstract" field (for module constraints, "abstract" means "not already an
     alias"). If the constraint is not instantiating, equivalence checks with
     the old definition are performed.

     Finally, merging is both used in (1) "normal" mode, to build a typed tree,
     in (2) "approx" mode, during the signature approximation phase of
     typechecking recursive modules, and in (3) "package" mode, for checking
     signatures of first-class modules.

     The overall structure is similar for each form:

     1. A [patch] function is defined to identify the item to substitute, do the
     equivalence checks if needed, and optionally build the new replacement item
     (if the substitution is non-destructive)

     2. The patch is applied to the module type by using the general
     [merge_signature] function. It returns the path of the affected item (along
     with the list of all suffixes, if the substitution was deep).

     3. Some post processing is applied (actual replacement for destructive
     substitutions, wellformedness checks)

  *)

  (** For the merging of type fields [S with type P.t = tdecl], the typedtree
      for the right-hand side type declaration [tdecl] is built at the point of
      the (possibly deep) constrained item [P.t] inside [S]. It is returned as
      an extra payload by merge. Other cases (module, module types) don't use
      the payload mechanism (the payload is then [()]). *)

  let return_payload ~ghosts ~replace_by ~late_typedtree ?(paths=[]) path =
    Some ((path, path::paths, late_typedtree),
          {Signature_group.ghosts; replace_by})

  let return = return_payload ~late_typedtree:()

  let split_row_id s ghosts =
    let srow = s ^ "#row" in
    let rec split before = function
      | Sig_type(id,_,_,_) :: rest when Ident.name id = srow ->
          before, Some id, rest
      | a :: rest -> split (a::before) rest
      | [] -> before, None, []
    in
    split [] ghosts

  let unsafe_signature_subst initial_env loc sg sub =
    (* This signature will not be used directly, it will always be freshened
       by the caller. So what we do with the scope doesn't really matter. But
       making it local makes it unlikely that we will ever use the result of
       this function unfreshened without issue. *)
    match Subst.Unsafe.signature Make_local sub sg with
    | Ok x -> x
    | Error (Fcm_type_substituted_away (p,mty)) ->
        let error = With_cannot_remove_packed_modtype(p,mty) in
        raise (Error(loc,initial_env,error))

  (* After the item has been patched, post processing does the actual
     destructive substitution and checks wellformedness of the resulting
     signature *)
  let post_process ~approx ~destructive loc lid env paths sg replace =
    let sg =
      if destructive then
        (* Check that the substitution will not make the signature ill-formed *)
        let () = if not approx then
                   check_usage_after_substitution ~loc ~lid env paths sg in
        (* Actually remove the identifiers *)
        let sub = Subst.change_locs Subst.identity loc in
        let sub = List.fold_left replace sub paths in
        (* Since destructive with is implemented via substitution, we need to
          expand any type abbreviations (like strengthening) where the expanded
          form might contain the thing we need to substitute. See corresponding
          test in strengthening.ml.  *)
        let sg = Mtype.expand_to env sg paths in
        unsafe_signature_subst env loc sg sub
      else sg
    in
    (* check that the resulting signature is still wellformed *)
    let () = if not approx then
               check_well_formed_module env loc "this instantiated signature"
                 (Mty_signature sg) in
    sg

  (* Main recursive knot to handle deep merges *)
  let rec merge_signature initial_env env sg namelist loc lid
      ~patch ~destructive =
    match
      Signature_group.replace_in_place
        (patch_deep_item ~patch ~destructive
           namelist initial_env env sg loc lid) sg
    with
    | Some ((p, paths, late_typedtree), sg) -> p, paths, late_typedtree, sg
    | None -> raise(Error(loc, initial_env, With_no_component lid.txt))
    | exception Includemod.Error explanation ->
      raise(Error(loc, initial_env, With_mismatch(lid.txt, explanation)))

  and patch_deep_item ~ghosts ~patch ~destructive
      namelist initial_env (env: Env.t) outer_sg loc lid item =
    match item, namelist with
    (* Shallow constraints : call the patch function *)
    | item, [s] -> patch item s env outer_sg ~ghosts

    (* Deep constraints *)
    | Sig_module(id, _, md, rs, priv) as current_item, s :: namelist
      when Ident.name id = s ->
        let sig_env = Env.add_signature outer_sg env in
        let sg = extract_sig sig_env loc md.md_type in
        let subpath, paths, late_typedtree, newsg =
          merge_signature ~patch initial_env sig_env sg
            namelist loc lid ~destructive in
        let newsg =
          if destructive then
            remove_modality_and_zero_alloc_variables_sg sig_env
              ~zap_modality:Mode.Modality.Value.zap_to_id newsg
          else
            newsg
        in
        let path = path_concat id subpath in
        begin
          match md.md_type, destructive with
          | Mty_alias _, false ->
              (* Deep non-destructive substitutions inside aliases are checked,
                 but do not change the resulting signature *)
              return_payload ~ghosts
                ~replace_by:(Some current_item) path ~late_typedtree
          | _, _ ->
              let new_md = {md with md_type = Mty_signature newsg} in
              let new_item = Sig_module(id, Mp_present, new_md, rs, priv) in
              return_payload ~ghosts ~replace_by:(Some new_item)
                path ~paths ~late_typedtree
        end
    | _ -> None

  (* Entry point for merging *)
  let merge ~patch ~destructive env sg loc lid =
    let initial_env = env in
    let names = Longident.flatten lid.txt in
    merge_signature ~patch ~destructive initial_env env sg names loc lid

  (* merge functions *)

  (** Type constraint [sg with type lid = sdecl]

      - [sdecl] is a parse tree, it will be translated into a typed tree [tdecl]
      at the point of the constrained item (at [lid]), and returned via the
      [late_typedtree] mechanism. It is then returned along the merged
      signature *)
  let merge_type ~destructive env loc sg lid sdecl =
    let patch item s sig_env sg_for_env ~ghosts =
      match item, sdecl.ptype_kind with
      | Sig_type(id, decl, rs, priv), Ptype_abstract
        when Ident.name id = s && Typedecl.is_fixed_type sdecl ->
          let decl_row =
            let arity = List.length sdecl.ptype_params in
            { type_params =
                (* jkind any is fine on the params because they get thrown away
                  below *)
                List.map
                  (fun _ ->
                     Btype.newgenvar (Jkind.Builtin.any ~why:Dummy_jkind))
                  sdecl.ptype_params;
              type_arity = arity;
              type_kind = Type_abstract Definition;
              type_jkind =
                Jkind.Builtin.value ~why:(Unknown "merge_constraint");
              type_private = Private;
              type_manifest = None;
              type_variance =
                List.map
                  (fun (_, (v, i)) ->
                     let (c, n) =
                       match v with
                       | Covariant -> true, false
                       | Contravariant -> false, true
                       | NoVariance -> false, false
                     in
                     make_variance (not n) (not c) (i = Injective)
                  )
                  sdecl.ptype_params;
              type_separability =
                Types.Separability.default_signature ~arity;
              type_loc = sdecl.ptype_loc;
              type_is_newtype = false;
              type_expansion_scope = Btype.lowest_level;
              type_attributes = [];
              type_unboxed_default = false;
              type_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
              type_unboxed_version = None;
            }
          and id_row = Ident.create_local (s^"#row") in
          let initial_env =
            Env.add_type ~check:false id_row decl_row env
          in
          let sig_env = Env.add_signature sg_for_env sig_env in
          let tdecl =
            Typedecl.transl_with_constraint id ~fixed_row_path:(Pident id_row)
              ~sig_env ~sig_decl:decl ~outer_env:initial_env sdecl in
          let newdecl = tdecl.typ_type in
          let before_ghosts, row_id, after_ghosts = split_row_id s ghosts in
          check_type_decl sig_env sg_for_env sdecl.ptype_loc
            id row_id newdecl decl;
          let decl_row = {decl_row with type_params = newdecl.type_params} in
          let rs' = if rs = Trec_first then Trec_not else rs in
          let ghosts =
            List.rev_append before_ghosts
              (Sig_type(id_row, decl_row, rs', priv)::after_ghosts)
          in
          let path = Pident id in
          return_payload ~ghosts ~late_typedtree:tdecl
            ~replace_by:(Some (Sig_type(id, newdecl, rs, priv))) path

      | Sig_type(id, sig_decl, rs, priv), _
        when Ident.name id = s ->
          let sig_env = Env.add_signature sg_for_env sig_env in
          let tdecl =
            Typedecl.transl_with_constraint id
              ~sig_env ~sig_decl ~outer_env:env sdecl in
          let newdecl = tdecl.typ_type in
          let newloc = sdecl.ptype_loc in
          let before_ghosts, row_id, after_ghosts = split_row_id s ghosts in
          let ghosts = List.rev_append before_ghosts after_ghosts in
          check_type_decl sig_env sg_for_env newloc
            id row_id newdecl sig_decl;
          let path = Pident id in
          let item_opt =
            if not destructive then (Some(Sig_type(id, newdecl, rs, priv)))
            else None
          in
          return_payload ~ghosts ~late_typedtree:tdecl ~replace_by:item_opt path

      | _ -> None
    in
    (* Merging *)
    let path, paths, tdecl, sg = merge ~patch ~destructive env sg loc lid in
    (* Post processing *)
    let replace =
      if destructive then
        match type_decl_is_alias sdecl with
        | Some lid ->
            (* if the type is an alias of [lid], replace by the definition *)
            let replacement, _ =
              try Env.find_type_by_name lid.txt env
              with Not_found -> assert false
            in
            fun s path -> Subst.Unsafe.add_type_path path replacement s
        | None ->
            (* if the type is not an alias, try to inline it *)
            let body = Option.get tdecl.typ_type.type_manifest in
            let params = tdecl.typ_type.type_params in
            if params_are_constrained params then
              raise(Error(loc, env, With_cannot_remove_constrained_type));
            fun s path ->
              Subst.Unsafe.add_type_function path ~params ~body s
      else
        fun s _ -> s
    in
    let sg =
      post_process ~approx:false ~destructive loc lid env paths sg replace in
    (tdecl, (path, lid, sg))

  (** Approximated type constraint [sg with type lid = _]

      This function is separated from [merge_type] as its logic is much more
      restricted (it does not need the right-hand side declaration).

      - For destructive constraints, the field is removed to prevent incorrect
      shadowing in the approximated signature.

      - For non-destructive constraints, the normal merging infrastructure is
      still used with an no-op identity patch. It is done to catch ill-formed
      constraints on non-existing fields early, during the approximation phase
      (rather than during signature typechecking) *)
  let merge_type_approx ~destructive env loc sg lid =
    let patch item s _sig_env _sg_for_env ~ghosts =
      match item with
      | Sig_type(id, _, _, _) when Ident.name id = s ->
         let item_opt =
           if destructive then None
           else
             (* An identity patch is applied *)
             Some (item)
         in
         return ~ghosts ~replace_by:item_opt (Pident id)
      | _ -> None
    in
    (* Merging *)
    let _, paths, _, sg = merge ~patch ~destructive env sg loc lid in
    (* Post processing *)
    let replace = fun s _path -> s in
    post_process ~approx:true ~destructive loc lid env paths sg replace


  (** Module constraint [sg with module lid = path]

      - [md'] is the module type of the module at [path], used for equivalence
      checks

      - [~approx] is used to disable equivalence checking when merging inside
      recursive module definitions
  *)
  let merge_module ?(approx=false) ~destructive env loc sg lid
      (md': Types.module_declaration) path remove_aliases =
    let patch item s sig_env sg_for_env ~ghosts =
      match item with
      | Sig_module(id, pres, md, rs, priv) when Ident.name id = s ->
          let sig_env = Env.add_signature sg_for_env sig_env in
          let real_path = Pident id in
          if destructive then
            let aliasable = not (Env.is_functor_arg path sig_env) in
            (* Inclusion check with the strengthened definition *)
            let _ = if (not approx) then
               ignore (Includemod.strengthened_module_decl ~loc ~mark:true
                         ~aliasable sig_env ~mmodes:All md' path md)
            in
            return ~ghosts ~replace_by:None real_path
          else
            let mty = md'.md_type in
            let mty = Mtype.scrape_for_type_of ~remove_aliases sig_env mty in
            let mty =
              remove_modality_and_zero_alloc_variables_mty sig_env
                ~zap_modality:Mode.Modality.Value.zap_to_id mty
            in
            let md'' = { md' with md_type = mty } in
            let newmd =
              Mtype.strengthen_decl ~aliasable:false md'' path in
            (* Inclusion check with the original signature *)
            let _ = if (not approx) then
               ignore (Includemod.modtypes ~mark:true ~loc sig_env
                         ~modes:All newmd.md_type md.md_type) in
            return ~ghosts
              ~replace_by:(Some(Sig_module(id, pres, newmd, rs, priv)))
              real_path
      | _ -> None
    in
    let real_path,paths,_,sg = merge ~patch ~destructive env sg loc lid in
    let replace s p = Subst.Unsafe.add_module_path p path s in
    let sg =
      post_process ~approx ~destructive loc lid env paths sg replace in
    real_path, lid, sg

  (** Module type constraint [sg with module type lid = mty]

      - [~approx] is used to disable equivalence checking when merging module
      types inside recursive module definitions
  *)
  let merge_modtype ?(approx=false) ~destructive env loc sg lid mty =
    let patch item s sig_env sg_for_env ~ghosts = match item with
      | Sig_modtype(id, mtd, priv)
        when Ident.name id = s ->
          (* Check for equivalence if the previous module type was not
             abstract. In approximation mode, the check is ignored *)
          let () = match mtd.mtd_type, approx with
            | Some previous_mty, false ->
                let sig_env = Env.add_signature sg_for_env sig_env in
                Includemod.check_modtype_equiv ~loc sig_env id previous_mty mty
            | _, _ -> ()
          in
          (* Create replacement item *)
          let new_item =
            if destructive then None
            else
              let mtd': modtype_declaration = {
                mtd_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
                mtd_type = Some mty;
                mtd_attributes = [];
                mtd_loc = loc; }
              in Some(Sig_modtype(id, mtd', priv))
          in
          let path = Pident id in
          return ~ghosts ~replace_by:new_item path
      | _ -> None
    in
    let path,paths,_,sg = merge ~patch ~destructive env sg loc lid in
    let replace s p = Subst.Unsafe.add_modtype_path p mty s in
    let sg = post_process ~approx ~destructive loc lid env paths sg replace in
    path, lid, sg

  (** Type constraints inside a first class module type [(module sg with type
      lid = cty)] *)
  let merge_package env loc sg lid cty =
    let patch item s sig_env sg_for_env ~ghosts = match item with
      | Sig_type(id, sig_decl, rs, priv)
        when Ident.name id = s ->
          begin match sig_decl.type_manifest with
          | None -> ()
          | Some ty ->
              raise (Error(loc, sig_env, With_package_manifest (lid.txt, ty)))
          end;
          let tdecl =
            Typedecl.transl_package_constraint ~loc cty.ctyp_type
          in
          (* Here we constrain the jkind of "with type" manifest by the jkind
             from the declaration from the original signature. Note that this is
             also checked in [check_type_decl], but there it is check, not
             constrain, which we need here to deal with type variables in
             package constraints (see tests in
             [typing-modules/package_constraint.ml]). Because the check is
             repeated later -- and with better handling for errors -- we just
             drop any error here. *)
          ignore
            (* CR layouts v2.8: Does this type_jkind need to be instantiated? *)
            (Ctype.constrain_decl_jkind env tdecl sig_decl.type_jkind);
          check_type_decl sig_env sg_for_env loc id None tdecl sig_decl;
          let tdecl = { tdecl with type_manifest = None } in
          let path = Pident id in
          return ~ghosts ~replace_by:(Some(Sig_type(id, tdecl, rs, priv))) path
      | _ -> None
    in
    let _, _, _, sg = merge ~patch ~destructive:false env sg loc lid in
    sg

  let check_package_with_type_constraints loc env mty constraints =
    let sg = extract_sig env loc mty in
    let sg =
      List.fold_left
        (fun sg (lid, cty) ->
           merge_package env loc sg lid cty)
        sg constraints
    in
    let scope = Ctype.create_scope () in
    Mtype.freshen ~scope (Mty_signature sg)

  let () =
    Typetexp.check_package_with_type_constraints :=
      check_package_with_type_constraints

  (* Helper for handling constraints on signatures: destructive constraints,
     written with ":=", actually remove the field from the signature, whereas
     non-destructive constraints just update the field. *)
  let is_destructive constr =
    match constr with
    | Pwith_typesubst _
      | Pwith_modtypesubst _
      | Pwith_modsubst _ -> true
    | Pwith_module _
      | Pwith_type _
      | Pwith_modtype _ -> false

end

(* Add recursion flags on declarations arising from a mutually recursive
   block. *)

let map_rec fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl -> fn Trec_first d1 :: map_end (fn Trec_next) dl rem

let map_rec_type ~rec_flag fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl ->
      let first =
        match rec_flag with
        | Recursive -> Trec_first
        | Nonrecursive -> Trec_not
      in
      fn first d1 :: map_end (fn Trec_next) dl rem

let rec map_rec_type_with_row_types ~rec_flag fn decls =
  match decls with
  | [] -> []
  | d1 :: dl ->
      if Btype.is_row_name (Ident.name d1.typ_id) then
        fn Trec_not d1 :: map_rec_type_with_row_types ~rec_flag fn dl
      else
        map_rec_type ~rec_flag fn decls []

(* Add type extension flags to extension constructors *)
let map_ext fn exts =
  match exts with
  | [] -> []
  | d1 :: dl -> fn Text_first d1 :: List.map (fn Text_next) dl

let rec apply_modalities_signature ~recursive env modalities sg =
  let env = Env.add_signature sg env in
  List.map (function
  | Sig_value (id, vd, vis) ->
      let val_modalities =
        vd.val_modalities
        |> Mode.Modality.Value.to_const_exn
        |> (fun then_ -> Mode.Modality.Value.Const.concat ~then_ modalities)
        |> Mode.Modality.Value.of_const
      in
      let vd = {vd with val_modalities} in
      Sig_value (id, vd, vis)
  | Sig_module (id, pres, md, rec_, vis) when recursive ->
      let md_type = apply_modalities_module_type env modalities md.md_type in
      let md = {md with md_type} in
      Sig_module (id, pres, md, rec_, vis)
  | item -> item
  ) sg

and apply_modalities_module_type env modalities = function
  | Mty_ident p ->
      let mtd = Env.find_modtype p env in
      begin match mtd.mtd_type with
      | None -> Mty_ident p
      | Some mty -> apply_modalities_module_type env modalities mty
      end
  | Mty_strengthen (mty, p, alias) ->
      Mty_strengthen (apply_modalities_module_type env modalities mty, p, alias)
  | Mty_signature sg ->
      let sg = apply_modalities_signature ~recursive:true env modalities sg in
      Mty_signature sg
  | (Mty_functor _ | Mty_alias _) as mty -> mty

let loc_of_modes (modes : Parsetree.mode loc list) : Location.t option =
  (* CR zqian: [Parsetree.modes] should be a record with a field that is
  the location of the whole modes string. *)
  let rec loc_end_of_modes (head : Parsetree.mode loc) = function
    | [] -> head.loc.loc_end
    | head' :: rest -> loc_end_of_modes head' rest
  in
  match modes with
  | [] -> None
  | head :: rest ->
    let loc_start = head.loc.loc_start in
    let loc_end = loc_end_of_modes head rest in
    Some {loc_start; loc_end; loc_ghost=false}

let check_unsupported_modal_module ~env reason modes =
  match loc_of_modes modes with
  | None -> ()
  | Some loc -> raise(Error(loc, env, Unsupported_modal_module reason))

let transl_modalities ?(default_modalities = Mode.Modality.Value.Const.id)
  modalities =
  match modalities with
  | [] -> default_modalities
  | _ :: _ ->
    Typemode.transl_modalities ~maturity:Stable Immutable modalities

let apply_pmd_modalities env ~default_modalities pmd_modalities mty =
  let modalities = transl_modalities ~default_modalities pmd_modalities in
  (*
  Workaround for pmd_modalities

  Let [f] be the mapping representing [pmd_modalities].

  In the future with proper modal modules, let [m] be the mode of the enclosing
  structure. This module will be of mode [f m], and a value inside the module
  will of mode [g (f m)] where [g] is the modalities on the value. Note that [m]
  itself is of the form [f0 (f1 .. (fn legacy))] where [legacy] is the mode of
  the file-level structure, and [fi] is the sequence of [pmd_modalities] that
  corresponds to the enclosing structure nested inside layers of structures.
  Therefore, the aforementioned value will be of mode
  [g (f (f0 (f1 .. (fn legacy))))].

  Currently, all modules are legacy. To simulate the above effect, we apply each
  [pmd_modalities] of a structure deeply to all [val_modalities] in that
  structure.

  We still don't support [pmd_modalities] on functors.
  *)
  match Mode.Modality.Value.Const.is_id modalities with
  | true -> mty, Mode.Modality.Value.id
  | false ->
      apply_modalities_module_type env modalities mty, Mode.Modality.Value.id

(* Auxiliary for translating recursively-defined module types.
   Return a module type that approximates the shape of the given module
   type AST.  Retain only module, type, and module type
   components of signatures.  For types, retain only their arity,
   making them abstract otherwise. *)

let rec approx_modtype env smty =
  match smty.pmty_desc with
    Pmty_ident lid ->
      let path =
        Env.lookup_modtype_path ~use:false ~loc:smty.pmty_loc lid.txt env
      in
      Mty_ident path
  | Pmty_alias lid ->
      let path, _ =
        Env.lookup_module_path ~use:false ~load:false
          ~loc:smty.pmty_loc lid.txt env
      in
      Mty_alias(path)
  | Pmty_signature ssg ->
      Mty_signature(approx_sig env ssg)
  | Pmty_functor(param, sres, mres) ->
      check_unsupported_modal_module ~env Functor_res mres;
      let (param, newenv) =
        match param with
        | Unit -> Types.Unit, env
        | Named (param, sarg, marg) ->
          check_unsupported_modal_module ~env Functor_param marg;
          let arg = approx_modtype env sarg in
          match param.txt with
          | None -> Types.Named (None, arg), env
          | Some name ->
            let rarg = Mtype.scrape_for_functor_arg env arg in
            let scope = Ctype.create_scope () in
            let (id, newenv) =
              Env.enter_module ~scope ~arg:true name Mp_present rarg env
            in
            Types.Named (Some id, arg), newenv
      in
      let res = approx_modtype newenv sres in
      Mty_functor(param, res)
  | Pmty_with(sbody, constraints) ->
      (* the module type body is approximated and resolved to a signature.*)
      let approx_body = approx_modtype env sbody in
      let initial_sig = extract_sig env sbody.pmty_loc approx_body in
      (* then, the constraints are approximated and merged, instead of merged
         and approximated. For (1) type constraints, (2) module constraints and
         (3) module type constraints replacing an abstract module type, it
         should be equivalent.

         However, for module type constraints replacing a concrete module type,
         approximating the constraint and the body before merging can interact
         with the equivalence check that is done between the constraint and the
         original definition. As approximation only tries to build a skeleton of
         non-recursive module types that can be used as an under-approximation
         of the name-spaces for the typechecking phase, the equivalence check is
         disabled, allowing for ill-formed constraints to be merged. It is
         should be harmless, because the ill-formedness is caught when
         re-typechecking the module types (with the approximation in the
         environment).  *)
      Mty_signature (List.fold_left
                       (approx_constraint env) initial_sig constraints)
  | Pmty_typeof smod ->
      let (_, mty) = !type_module_type_of_fwd env smod in
      mty
  | Pmty_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))
  | Pmty_strengthen (smty, mod_id) ->
      let mty = approx_modtype env smty in
      let path, _ =
        (* CR-someday: potentially improve error message for strengthening with
           a mutually recursive module. *)
        Env.lookup_module_path ~use:false ~load:false
          ~loc:mod_id.loc mod_id.txt env
      in
      let aliasable = (not (Env.is_functor_arg path env)) in
      Mty_strengthen (mty, path, Aliasability.aliasable aliasable)

and approx_module_declaration env pmd =
  {
    Types.md_type = approx_modtype env pmd.pmd_type;
    md_modalities = Mode.Modality.Value.id;
    md_attributes = pmd.pmd_attributes;
    md_loc = pmd.pmd_loc;
    md_uid = Uid.internal_not_actually_unique;
  }

and approx_sig env {psg_items; _} = approx_sig_items env psg_items

and approx_sig_items env ssg=
  match ssg with
    [] -> []
  | item :: srem ->
      match item.psig_desc with
      | Psig_type (rec_flag, sdecls) ->
          let decls = Typedecl.approx_type_decl sdecls in
          let rem = approx_sig_items env srem in
          map_rec_type ~rec_flag
            (fun rs (id, info) -> Sig_type(id, info, rs, Exported)) decls rem
      | Psig_typesubst _ -> approx_sig_items env srem
      | Psig_module { pmd_name = { txt = None; _ }; _ } ->
          approx_sig_items env srem
      | Psig_module pmd ->
          let scope = Ctype.create_scope () in
          let md = approx_module_declaration env pmd in
          let pres =
            match md.Types.md_type with
            | Mty_alias _ -> Mp_absent
            | _ -> Mp_present
          in
          (* Assume the enclosing structure is legacy, for backward
              compatibility *)
          let id, newenv =
            Env.enter_module_declaration ~scope (Option.get pmd.pmd_name.txt)
              pres md ~mode:Value.legacy env
          in
          Sig_module(id, pres, md, Trec_not, Exported) :: approx_sig_items newenv srem
      | Psig_modsubst pms ->
          let scope = Ctype.create_scope () in
          let _, md, _ =
            Env.lookup_module ~use:false ~loc:pms.pms_manifest.loc
               pms.pms_manifest.txt env
          in
          let pres =
            match md.Types.md_type with
            | Mty_alias _ -> Mp_absent
            | _ -> Mp_present
          in
          (* Assume the enclosing structure is legacy, for backward
              compatibility *)
          let _, newenv =
            Env.enter_module_declaration ~scope pms.pms_name.txt pres md env
              ~mode:Value.legacy
          in
          approx_sig_items newenv srem
      | Psig_recmodule sdecls ->
          let scope = Ctype.create_scope () in
          let decls =
            List.filter_map
              (fun pmd ->
                 Option.map (fun name ->
                   Ident.create_scoped ~scope name,
                   approx_module_declaration env pmd
                 ) pmd.pmd_name.txt
              )
              sdecls
          in
          let newenv =
            List.fold_left
              (fun env (id, md) -> Env.add_module_declaration ~check:false
                  id Mp_present md ~mode:(Value.min) env)
              env decls
          in
          map_rec
            (fun rs (id, md) -> Sig_module(id, Mp_present, md, rs, Exported))
            decls
            (approx_sig_items newenv srem)
      | Psig_modtype d ->
          let info = approx_modtype_info env d in
          let scope = Ctype.create_scope () in
          let (id, newenv) =
            Env.enter_modtype ~scope d.pmtd_name.txt info env
          in
          Sig_modtype(id, info, Exported) :: approx_sig_items newenv srem
      | Psig_modtypesubst d ->
          let info = approx_modtype_info env d in
          let scope = Ctype.create_scope () in
          let (_id, newenv) =
            Env.enter_modtype ~scope d.pmtd_name.txt info env
          in
          approx_sig_items newenv srem
      | Psig_open sod ->
          let _, env = type_open_descr env sod in
          approx_sig_items env srem
      | Psig_include ({pincl_loc=loc; pincl_mod=mod_; pincl_kind=kind;
          pincl_attributes=attrs}, moda) ->
          begin match kind with
          | Functor ->
              Language_extension.assert_enabled ~loc Include_functor ();
              raise (Error(loc, env, Recursive_include_functor))
          | Structure ->
              let mty = approx_modtype env mod_ in
              let scope = Ctype.create_scope () in
              let sg = extract_sig env loc mty in
              let sg =
                match moda with
                | [] -> sg
                | _ ->
                  let modalities =
                    Typemode.transl_modalities ~maturity:Stable Immutable moda
                  in
                  let recursive =
                    not @@ Builtin_attributes.has_attribute "no_recursive_modalities" attrs
                  in
                  apply_modalities_signature ~recursive env modalities sg
              in
              let sg, newenv = Env.enter_signature ~scope sg env in
              sg @ approx_sig_items newenv srem
          end
      | Psig_class sdecls | Psig_class_type sdecls ->
          let decls, env = Typeclass.approx_class_declarations env sdecls in
          let rem = approx_sig_items env srem in
          map_rec (fun rs decl ->
            let open Typeclass in [
              Sig_class_type(decl.clsty_ty_id, decl.clsty_ty_decl, rs,
                             Exported);
              Sig_type(decl.clsty_obj_id, decl.clsty_obj_abbr, rs, Exported);
            ]
          ) decls [rem]
          |> List.flatten
      | Psig_kind_abbrev _ ->
          Misc.fatal_error "kind_abbrev not supported!"
      | _ ->
          approx_sig_items env srem

and approx_modtype_info env sinfo =
  {
   mtd_type = Option.map (approx_modtype env) sinfo.pmtd_type;
   mtd_attributes = sinfo.pmtd_attributes;
   mtd_loc = sinfo.pmtd_loc;
   mtd_uid = Uid.internal_not_actually_unique;
  }

and approx_constraint env body constr =
  (* constraints are first approximated then merged, disabling all equivalence
     and wellformedness checks. Only ill-formed constraints where the field does
     not exists are caught at approximation phase, other errors (non-equivalent
     constraints) will be caught when typechecking the signatures (with the
     approximation in the environment). *)
  let destructive = Merge.is_destructive constr in
  match constr with
  | Pwith_type (l, decl)
  | Pwith_typesubst (l, decl) ->
     Merge.merge_type_approx ~destructive env decl.ptype_loc body l

  | Pwith_modtype (id, smty)
  | Pwith_modtypesubst (id, smty) ->
      let approx_smty = approx_modtype env smty in
      let _,_,sg = Merge.merge_modtype ~approx:true ~destructive
          env smty.pmty_loc body id approx_smty in
      sg
  | Pwith_module (id, lid)
  | Pwith_modsubst (id, lid) ->
      (* Lookup the module to make sure that it is not recursive.
         (GPR#1626) *)
      let path, approx_md, _ =
        Env.lookup_module ~use:false ~loc:lid.loc lid.txt env in
      let _,_,sg =
        Merge.merge_module ~approx:true ~destructive env
          lid.loc body id approx_md path false in
      sg


let approx_modtype env smty =
  Warnings.without_warnings
    (fun () -> approx_modtype env smty)

(* Auxiliaries for checking the validity of name shadowing in signatures and
   structures.
   If a shadowing is valid, we also record some information (its ident,
   location where it first appears, etc) about the item that gets shadowed. *)
module Signature_names : sig
  type t

 type shadowable =
    {
      self: Ident.t;
      group: Ident.t list;
      (** group includes the element itself and all elements
                that should be removed at the same time
      *)
      loc:Location.t;
    }

  type info = [
    | `Exported
    | `From_open
    | `Shadowable of shadowable
    | `Substituted_away of Subst.Unsafe.t
  ]

  val create : unit -> t

  val check_value     : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_type      : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_typext    : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_module    : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_modtype   : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_class     : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_class_type: ?info:info -> t -> Location.t -> Ident.t -> unit

  val check_sig_item:
    ?info:info -> t -> Location.t -> Signature_group.rec_group -> unit

  val simplify: Env.t -> t -> Types.signature -> Types.signature
end = struct

  type shadowable =
    {
      self: Ident.t;
      group: Ident.t list;
      (** group includes the element itself and all elements
                that should be removed at the same time
      *)
      loc:Location.t;
    }

  type bound_info = [
    | `Exported
    | `Shadowable of shadowable
  ]

  type info = [
    | `From_open
    | `Substituted_away of Subst.Unsafe.t
    | bound_info
  ]

  type hide_reason =
    | From_open
    | Shadowed_by of Ident.t * Location.t

  type to_be_removed = {
    mutable subst: Subst.Unsafe.t;
    mutable hide: (Sig_component_kind.t * Location.t * hide_reason) Ident.Map.t;
  }

  type names_infos = (string, bound_info) Hashtbl.t

  type names = {
    values: names_infos;
    types: names_infos;
    modules: names_infos;
    modtypes: names_infos;
    typexts: names_infos;
    classes: names_infos;
    class_types: names_infos;
  }

  let new_names () = {
    values = Hashtbl.create 16;
    types = Hashtbl.create 16;
    modules = Hashtbl.create 16;
    modtypes = Hashtbl.create 16;
    typexts = Hashtbl.create 16;
    classes = Hashtbl.create 16;
    class_types = Hashtbl.create 16;
  }

  type t = {
    bound: names;
    to_be_removed: to_be_removed;
  }

  let create () = {
    bound = new_names ();
    to_be_removed = {
      subst = Subst.identity;
      hide = Ident.Map.empty;
    };
  }

  let table_for component names =
    let open Sig_component_kind in
    match component with
    | Value -> names.values
    | Type | Label | Unboxed_label | Constructor -> names.types
    | Module -> names.modules
    | Module_type -> names.modtypes
    | Extension_constructor -> names.typexts
    | Class -> names.classes
    | Class_type -> names.class_types

  let check_unsafe_subst loc env: _ result -> _ = function
    | Ok x -> x
    | Error (Subst.Unsafe.Fcm_type_substituted_away (p,_)) ->
        raise (Error (loc, env, Non_packable_local_modtype_subst p))

  let check cl t loc id (info : info) =
    let to_be_removed = t.to_be_removed in
    match info with
    | `Substituted_away s ->
        let subst =
          check_unsafe_subst loc Env.empty @@
          Subst.Unsafe.compose s to_be_removed.subst
        in
        to_be_removed.subst <- subst;
    | `From_open ->
        to_be_removed.hide <-
          Ident.Map.add id (cl, loc, From_open) to_be_removed.hide
    | #bound_info as bound_info ->
        let tbl = table_for cl t.bound in
        let name = Ident.name id in
        match Hashtbl.find_opt tbl name with
        | None -> Hashtbl.add tbl name bound_info
        | Some (`Shadowable s) ->
            Hashtbl.replace tbl name bound_info;
            let reason = Shadowed_by (id, loc) in
            List.iter (fun shadowed_id ->
            to_be_removed.hide <-
              Ident.Map.add shadowed_id (cl, s.loc, reason)
                to_be_removed.hide
              ) s.group
        | Some `Exported ->
            raise(Error(loc, Env.empty, Repeated_name(cl, name)))

  let check_value ?info t loc id =
    let info =
      match info with
      | Some i -> i
      | None -> `Shadowable {self=id; group=[id]; loc}
    in
    check Sig_component_kind.Value t loc id info
  let check_type ?(info=`Exported) t loc id =
    check Sig_component_kind.Type t loc id info
  let check_module ?(info=`Exported) t loc id =
    check Sig_component_kind.Module t loc id info
  let check_modtype ?(info=`Exported) t loc id =
    check Sig_component_kind.Module_type t loc id info
  let check_typext ?(info=`Exported) t loc id =
    check Sig_component_kind.Extension_constructor t loc id info
  let check_class ?(info=`Exported) t loc id =
    check Sig_component_kind.Class t loc id info
  let check_class_type ?(info=`Exported) t loc id =
    check Sig_component_kind.Class_type t loc id info

  let classify =
    let open Sig_component_kind in
    function
    | Sig_type(id, _, _, _) -> Type, id
    | Sig_module(id, _, _, _, _) -> Module, id
    | Sig_modtype(id, _, _) -> Module_type, id
    | Sig_typext(id, _, _, _) -> Extension_constructor, id
    | Sig_value (id, _, _) -> Value, id
    | Sig_class (id, _, _, _) -> Class, id
    | Sig_class_type (id, _, _, _) -> Class_type, id

  let check_item ?info names loc kind id ids =
    let info =
      match info with
      | None -> `Shadowable {self=id; group=ids; loc}
      | Some i -> i
    in
    check kind names loc id info

  let check_sig_item ?info names loc (item:Signature_group.rec_group) =
    let check ?info names loc item =
      let all = List.map classify (Signature_group.flatten item) in
      let group = List.map snd all in
      List.iter (fun (kind,id) -> check_item ?info names loc kind id group)
        all
    in
    (* we can ignore x.pre_ghosts: they are eliminated by strengthening, and
       thus never appear in includes *)
     List.iter (check ?info names loc) (Signature_group.rec_items item.group)

  (* We usually require name uniqueness of signature components (e.g. types,
     modules, etc), however in some situation reusing the name is allowed: if
     the component is a value or an extension, or if the name is introduced by
     an include.
     When there are multiple specifications of a component with the same name,
     we try to keep only the last (rightmost) one, removing all references to
     the previous ones from the signature.
     If some reference cannot be removed, then we error out with
     [Cannot_hide_id].
  *)
  let simplify env t sg =
    let to_remove = t.to_be_removed in
    let ids_to_remove =
      Ident.Map.fold (fun id (kind,  _, _) lst ->
        if Sig_component_kind.can_appear_in_types kind then
          id :: lst
        else
          lst
      ) to_remove.hide []
    in
    let simplify_item (component: Types.signature_item) =
      let user_kind, user_id, user_loc =
        let open Sig_component_kind in
        match component with
        | Sig_value(id, v, _) -> Value, id, v.val_loc
        | Sig_type (id, td, _, _) -> Type, id, td.type_loc
        | Sig_typext (id, te, _, _) -> Extension_constructor, id, te.ext_loc
        | Sig_module (id, _, md, _, _) -> Module, id, md.md_loc
        | Sig_modtype (id, mtd, _) -> Module_type, id, mtd.mtd_loc
        | Sig_class (id, c, _, _) -> Class, id, c.cty_loc
        | Sig_class_type (id, ct, _, _) -> Class_type, id, ct.clty_loc
      in
      if Ident.Map.mem user_id to_remove.hide then
        None
      else begin
        let component =
          if to_remove.subst == Subst.identity then
            component
          else
            check_unsafe_subst user_loc env @@
            Subst.Unsafe.signature_item Keep to_remove.subst component
        in
        let component =
          match ids_to_remove with
          | [] -> component
          | ids ->
            try Mtype.nondep_sig_item env ids component with
            | Ctype.Nondep_cannot_erase removed_item_id ->
              let (removed_item_kind, removed_item_loc, reason) =
                Ident.Map.find removed_item_id to_remove.hide
              in
              let err_loc, hiding_error =
                match reason with
                | From_open ->
                  removed_item_loc,
                  Appears_in_signature {
                    opened_item_kind = removed_item_kind;
                    opened_item_id = removed_item_id;
                    user_id;
                    user_kind;
                    user_loc;
                  }
                | Shadowed_by (shadower_id, shadower_loc) ->
                  shadower_loc,
                  Illegal_shadowing {
                    shadowed_item_kind = removed_item_kind;
                    shadowed_item_id = removed_item_id;
                    shadowed_item_loc = removed_item_loc;
                    shadower_id;
                    user_id;
                    user_kind;
                    user_loc;
                  }
              in
              raise (Error(err_loc, env, Cannot_hide_id hiding_error))
        in
        Some component
      end
    in
    List.filter_map simplify_item sg
end

let has_remove_aliases_attribute attr =
  let remove_aliases =
    Attr_helper.get_no_payload_attribute "remove_aliases" attr
  in
  match remove_aliases with
  | None -> false
  | Some _ -> true

(* Check and translate a module type expression *)

let transl_modtype_longident loc env lid =
  Env.lookup_modtype_path ~loc lid env

let transl_module_alias loc env lid =
  let path, _ = Env.lookup_module_path ~load:false ~loc lid env in
  path

let mkmty desc typ env loc attrs =
  let mty = {
    mty_desc = desc;
    mty_type = typ;
    mty_loc = loc;
    mty_env = env;
    mty_attributes = attrs;
    } in
  Cmt_format.add_saved_type (Cmt_format.Partial_module_type mty);
  mty

let mksig desc env loc =
  let sg = { sig_desc = desc; sig_loc = loc; sig_env = env } in
  Cmt_format.add_saved_type (Cmt_format.Partial_signature_item sg);
  sg

(* let signature sg = List.map (fun item -> item.sig_type) sg *)

let rec transl_modtype env smty =
  Builtin_attributes.warning_scope smty.pmty_attributes
    (fun () -> transl_modtype_aux env smty)

and transl_modtype_functor_arg env sarg =
  let mty = transl_modtype env sarg in
  {mty with mty_type = Mtype.scrape_for_functor_arg env mty.mty_type}

and transl_modtype_aux env smty =
  let loc = smty.pmty_loc in
  match smty.pmty_desc with
    Pmty_ident lid ->
      let path = transl_modtype_longident loc env lid.txt in
      mkmty (Tmty_ident (path, lid)) (Mty_ident path) env loc
        smty.pmty_attributes
  | Pmty_alias lid ->
      let path = transl_module_alias loc env lid.txt in
      mkmty (Tmty_alias (path, lid)) (Mty_alias path) env loc
        smty.pmty_attributes
  | Pmty_signature ssg ->
      let sg = transl_signature env ssg in
      mkmty (Tmty_signature sg) (Mty_signature sg.sig_type) env loc
        smty.pmty_attributes
  | Pmty_functor(sarg_opt, sres, mres) ->
      check_unsupported_modal_module ~env Functor_res mres;
      let t_arg, ty_arg, newenv =
        match sarg_opt with
        | Unit -> Unit, Types.Unit, env
        | Named (param, sarg, marg) ->
          check_unsupported_modal_module ~env Functor_param marg;
          let arg = transl_modtype_functor_arg env sarg in
          let (id, newenv) =
            match param.txt with
            | None -> None, env
            | Some name ->
              let scope = Ctype.create_scope () in
              let id, newenv =
                let arg_md =
                  { md_type = arg.mty_type;
                    md_modalities = Mode.Modality.Value.id;
                    md_attributes = [];
                    md_loc = param.loc;
                    md_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
                  }
                in
                Env.enter_module_declaration ~scope ~arg:true name Mp_present
                  ~mode:(alloc_as_value functor_param_mode) arg_md env
              in
              Some id, newenv
          in
          Named (id, param, arg), Types.Named (id, arg.mty_type), newenv
      in
      let res = transl_modtype newenv sres in
      mkmty (Tmty_functor (t_arg, res))
        (Mty_functor(ty_arg, res.mty_type)) env loc
        smty.pmty_attributes
  | Pmty_with(sbody, constraints) ->
      let body = transl_modtype env sbody in
      let init_sg = extract_sig env sbody.pmty_loc body.mty_type in
      let remove_aliases = has_remove_aliases_attribute smty.pmty_attributes in
      let (rev_tcstrs, final_sg) =
        List.fold_left (transl_with ~loc:smty.pmty_loc env remove_aliases)
        ([],init_sg) constraints in
      let scope = Ctype.create_scope () in
      mkmty (Tmty_with ( body, List.rev rev_tcstrs))
        (Mtype.freshen ~scope (Mty_signature final_sg)) env loc
        smty.pmty_attributes
  | Pmty_typeof smod ->
      let env = Env.in_signature false env in
      let tmty, mty = !type_module_type_of_fwd env smod in
      mkmty (Tmty_typeof tmty) mty env loc smty.pmty_attributes
  | Pmty_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))
  | Pmty_strengthen (mty, mod_id) ->
      Language_extension.assert_enabled ~loc:smty.pmty_loc
        Module_strengthening ();
      let tmty = transl_modtype_aux env mty in
      let path, md, _ =
        Env.lookup_module ~use:false ~loc:mod_id.loc mod_id.txt env
      in
      let aliasable = not (Env.is_functor_arg path env) in
      try
        ignore
          (Includemod.modtypes ~loc env ~modes:All
            ~mark:true md.md_type tmty.mty_type);
        mkmty
          (Tmty_strengthen (tmty, path, mod_id))
          (Mty_strengthen
            (tmty.mty_type, path, Aliasability.aliasable aliasable))
          env
          loc
          []
      with Includemod.Error explanation ->
        raise(Error(loc, env, Strengthening_mismatch(mod_id.txt, explanation)))
      ;

and transl_with ~loc env remove_aliases (rev_tcstrs, sg) constr =
  let destructive = Merge.is_destructive constr in
  let constr, (path, lid, sg) = match constr with
    | Pwith_type (l, decl)
    | Pwith_typesubst (l, decl) ->
        let tdecl, merge_res =
          Merge.merge_type ~destructive env loc sg l decl
        in
        let constr = if destructive then
            (Twith_typesubst tdecl)
          else
            (Twith_type tdecl)
        in
        (constr, merge_res)

    | Pwith_module (l, l')
    | Pwith_modsubst (l,l') ->
        let path, md, _ = Env.lookup_module ~loc l'.txt env in
        let constr = if destructive then
            (Twith_modsubst (path, l'))
          else
            (Twith_module (path, l'))
        in
        (constr,
         Merge.merge_module ~destructive env loc sg l md path remove_aliases)

    | Pwith_modtype (l,smty)
    | Pwith_modtypesubst (l,smty) ->
        let tmty = transl_modtype env smty in
        let constr = if destructive then
            (Twith_modtypesubst tmty)
          else
            (Twith_modtype tmty)
        in
        (constr, Merge.merge_modtype ~destructive env loc sg l tmty.mty_type)

  in
  ((path, lid, constr) :: rev_tcstrs, sg)

and transl_signature env {psg_items; psg_modalities; psg_loc} =
  let names = Signature_names.create () in

  let sig_modalities = transl_modalities psg_modalities in

  let transl_include ~loc env sig_acc sincl modalities =
    let smty = sincl.pincl_mod in
    let tmty =
      Builtin_attributes.warning_scope sincl.pincl_attributes
        (fun () -> transl_modtype env smty)
    in
    let mty = tmty.mty_type in
    let scope = Ctype.create_scope () in
    let incl_kind, sg =
      match sincl.pincl_kind with
      | Functor ->
        Language_extension.assert_enabled ~loc Include_functor ();
        let sg, incl_kind =
          extract_sig_functor_open false env smty.pmty_loc mty sig_acc
        in
        incl_kind, sg
      | Structure ->
        Tincl_structure, extract_sig env smty.pmty_loc mty
    in
    let modalities =
      transl_modalities ~default_modalities:sig_modalities modalities
    in
    let recursive =
      not @@ Builtin_attributes.has_attribute "no_recursive_modalities"
        sincl.pincl_attributes
    in
    let sg = apply_modalities_signature ~recursive env modalities sg in
    (* Assume the structure is legacy, for backward compatibility *)
    let sg, newenv = Env.enter_signature ~scope sg ~mode:Value.legacy env in
    Signature_group.iter
      (Signature_names.check_sig_item names loc)
      sg;
    let incl =
      { incl_mod = tmty;
        incl_type = sg;
        incl_kind;
        incl_attributes = sincl.pincl_attributes;
        incl_loc = sincl.pincl_loc;
      }
    in
    mksig (Tsig_include (incl, modalities)) env loc, sg, newenv
  in

  let transl_sig_item env sig_acc item =
    let loc = item.psig_loc in
    match item.psig_desc with
    | Psig_value sdesc ->
        let modalities =
          match sdesc.pval_modalities with
          | [] -> sig_modalities
          | l -> Typemode.transl_modalities ~maturity:Stable Immutable l
        in
        let modalities = Mode.Modality.Value.of_const modalities in
        let (tdesc, newenv) =
          Typedecl.transl_value_decl env ~modalities item.psig_loc sdesc
        in
        Signature_names.check_value names tdesc.val_loc tdesc.val_id;
        mksig (Tsig_value tdesc) env loc,
        [Sig_value(tdesc.val_id, tdesc.val_val, Exported)],
        newenv
    | Psig_type (rec_flag, sdecls) ->
        let (decls, newenv, _shapes) =
          Typedecl.transl_type_decl env rec_flag sdecls
        in
        List.iter (fun td ->
          Signature_names.check_type names td.typ_loc td.typ_id;
        ) decls;
        let sig_items =
          map_rec_type_with_row_types ~rec_flag
            (fun rs td -> Sig_type(td.typ_id, td.typ_type, rs, Exported))
            decls
        in
        mksig (Tsig_type (rec_flag, decls)) env loc, sig_items, newenv
    | Psig_typesubst sdecls ->
        let (decls, newenv, _shapes) =
          Typedecl.transl_type_decl env Nonrecursive sdecls
        in
        List.iter (fun td ->
          if td.typ_kind <> Ttype_abstract || td.typ_manifest = None ||
             td.typ_private = Private
          then
            raise (Error (td.typ_loc, env, Invalid_type_subst_rhs));
          let params = td.typ_type.type_params in
          if params_are_constrained params
          then raise(Error(loc, env, With_cannot_remove_constrained_type));
          let info =
            let subst =
              Subst.Unsafe.add_type_function (Pident td.typ_id)
                ~params
                ~body:(Option.get td.typ_type.type_manifest)
                Subst.identity
            in
            Some (`Substituted_away subst)
          in
          Signature_names.check_type ?info names td.typ_loc td.typ_id
        ) decls;
        mksig (Tsig_typesubst decls) env loc, [], newenv
    | Psig_typext styext ->
        let (tyext, newenv, _shapes) =
          Typedecl.transl_type_extension false env item.psig_loc styext
        in
        let constructors = tyext.tyext_constructors in
        List.iter (fun ext ->
          Signature_names.check_typext names ext.ext_loc ext.ext_id
        ) constructors;
        let tsg = map_ext (fun es ext ->
            Sig_typext(ext.ext_id, ext.ext_type, es, Exported)
          ) constructors
        in
        mksig (Tsig_typext tyext) env loc,
        tsg,
        newenv
    | Psig_exception sext ->
        let (ext, newenv, _shapes) = Typedecl.transl_type_exception env sext in
        let constructor = ext.tyexn_constructor in
        Signature_names.check_typext names constructor.ext_loc
          constructor.ext_id;
        let tsg =
          Sig_typext(constructor.ext_id, constructor.ext_type,
                     Text_exception, Exported)
        in
        mksig (Tsig_exception ext) env loc, [tsg], newenv
    | Psig_module pmd ->
        let scope = Ctype.create_scope () in
        let tmty =
          Builtin_attributes.warning_scope pmd.pmd_attributes
            (fun () -> transl_modtype env pmd.pmd_type)
        in
        let mty_type, md_modalities =
          apply_pmd_modalities env ~default_modalities:sig_modalities
            pmd.pmd_modalities tmty.mty_type
        in
        let tmty = {tmty with mty_type} in
        let pres =
          match tmty.mty_type with
          | Mty_alias _ -> Mp_absent
          | _ -> Mp_present
        in
        let md = {
          md_type=tmty.mty_type;
          md_modalities;
          md_attributes=pmd.pmd_attributes;
          md_loc=pmd.pmd_loc;
          md_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
        }
        in
        let id, newenv =
          match pmd.pmd_name.txt with
          | None -> None, env
          | Some name ->
            let id, newenv =
              (* Assume the enclosing structure is legacy, for backward
                 compatibility *)
              Env.enter_module_declaration ~scope name pres md
                ~mode:Value.legacy env
            in
            Signature_names.check_module names pmd.pmd_name.loc id;
            Some id, newenv
        in
        let sig_item =
          mksig (Tsig_module {md_id=id; md_name=pmd.pmd_name;
                              md_uid=md.md_uid; md_presence=pres;
                              md_type=tmty;
                              md_modalities=md.md_modalities;
                              md_loc=pmd.pmd_loc;
                              md_attributes=pmd.pmd_attributes})
            env loc
        in
        let tsg =
          match id with
          | None -> []
          | Some id -> [Sig_module(id, pres, md, Trec_not, Exported)]
        in
        sig_item, tsg, newenv
    | Psig_modsubst pms ->
        let scope = Ctype.create_scope () in
        let path, md, _ =
          Env.lookup_module ~loc:pms.pms_manifest.loc pms.pms_manifest.txt env
        in
        let aliasable = not (Env.is_functor_arg path env) in
        let md =
          if not aliasable then
            md
          else
            { md_type = Mty_alias path;
              md_modalities = Mode.Modality.Value.id;
              md_attributes = pms.pms_attributes;
              md_loc = pms.pms_loc;
              md_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
            }
        in
        let pres =
          match md.md_type with
          | Mty_alias _ -> Mp_absent
          | _ -> Mp_present
        in
        (* Assume the enclosing structure is legacy, for backward
            compatibility *)
        let id, newenv =
          Env.enter_module_declaration ~scope pms.pms_name.txt pres md
            ~mode:Value.legacy env
        in
        let info =
          `Substituted_away (Subst.add_module id path Subst.identity)
        in
        Signature_names.check_module ~info names pms.pms_name.loc id;
        let sig_item =
          mksig (Tsig_modsubst {ms_id=id; ms_name=pms.pms_name;
                                ms_uid=md.md_uid; ms_manifest=path;
                                ms_txt=pms.pms_manifest; ms_loc=pms.pms_loc;
                                ms_attributes=pms.pms_attributes})
            env loc
        in
        sig_item, [], newenv
    | Psig_recmodule sdecls ->
        (* None of the modules have modes specified, since we're in a signature
           *)
        let sdecls = List.map (fun sdecl -> (sdecl, None)) sdecls in
        let (tdecls, newenv) =
          transl_recmodule_modtypes env ~sig_modalities sdecls in
        let decls =
          List.filter_map (fun (md, _, uid, _) ->
            match md.md_id with
            | None -> None
            | Some id -> Some (id, md, uid)
          ) tdecls
        in
        List.iter (fun (id, md, _uid) ->
          Signature_names.check_module names md.md_loc id;
        ) decls;
        let sig_items =
          map_rec (fun rs (id, md, uid) ->
            let d = {Types.md_type = md.md_type.mty_type;
                     md_modalities = md.md_modalities;
                     md_attributes = md.md_attributes;
                     md_loc = md.md_loc;
                     md_uid = uid;
                    } in
            Sig_module(id, Mp_present, d, rs, Exported))
            decls []
        in
        mksig (Tsig_recmodule (List.map (fun (md, _, _, _) -> md) tdecls))
          env loc,
        sig_items,
        newenv
    | Psig_modtype pmtd ->
        let newenv, mtd, decl = transl_modtype_decl env pmtd in
        Signature_names.check_modtype names pmtd.pmtd_loc mtd.mtd_id;
        mksig (Tsig_modtype mtd) env loc,
        [Sig_modtype (mtd.mtd_id, decl, Exported)],
        newenv
    | Psig_modtypesubst pmtd ->
        let newenv, mtd, _decl = transl_modtype_decl env pmtd in
        let info =
          let mty = match mtd.mtd_type with
            | Some tmty -> tmty.mty_type
            | None ->
                (* parsetree invariant, see Ast_invariants *)
                assert false
          in
          let subst = Subst.Unsafe.add_modtype mtd.mtd_id mty Subst.identity in
          `Substituted_away subst
        in
        Signature_names.check_modtype ~info names pmtd.pmtd_loc mtd.mtd_id;
        mksig (Tsig_modtypesubst mtd) env loc,
        [],
        newenv
    | Psig_open sod ->
        let (od, newenv) = type_open_descr env sod in
        mksig (Tsig_open od) env loc, [], newenv
    | Psig_include (sincl, modalities) ->
        transl_include ~loc env sig_acc sincl modalities
    | Psig_class cl ->
        let (classes, newenv) = Typeclass.class_descriptions env cl in
        List.iter (fun cls ->
          let open Typeclass in
          let loc = cls.cls_id_loc.Location.loc in
          Signature_names.check_type names loc cls.cls_obj_id;
          Signature_names.check_class names loc cls.cls_id;
          Signature_names.check_class_type names loc cls.cls_ty_id;
        ) classes;
        let tsg =
          map_rec (fun rs cls ->
            let open Typeclass in
            [Sig_class(cls.cls_id, cls.cls_decl, rs, Exported);
             Sig_class_type(cls.cls_ty_id, cls.cls_ty_decl, rs, Exported);
             Sig_type(cls.cls_obj_id, cls.cls_obj_abbr, rs, Exported)]
          ) classes [] |> List.flatten
        in
        let typedtree =
          mksig (Tsig_class
                   (List.map (fun decr ->
                      decr.Typeclass.cls_info) classes)) env loc
        in
        typedtree, tsg, newenv
    | Psig_class_type cl ->
        let (classes, newenv) = Typeclass.class_type_declarations env cl in
        List.iter (fun decl ->
          let open Typeclass in
          let loc = decl.clsty_id_loc.Location.loc in
          Signature_names.check_class_type names loc decl.clsty_ty_id;
          Signature_names.check_type names loc decl.clsty_obj_id;
        ) classes;
        let tsg =
          map_rec (fun rs decl ->
            let open Typeclass in
            [Sig_class_type(decl.clsty_ty_id, decl.clsty_ty_decl, rs,
                            Exported);
             Sig_type(decl.clsty_obj_id, decl.clsty_obj_abbr, rs, Exported);
            ]
          ) classes []
          |> List.flatten
        in
        let typedtree =
          mksig
            (Tsig_class_type
               (List.map (fun decl -> decl.Typeclass.clsty_info) classes))
            env loc
        in
        typedtree, tsg, newenv
    | Psig_attribute attr ->
        Builtin_attributes.parse_standard_interface_attributes attr;
        mksig (Tsig_attribute attr) env loc, [], env
    | Psig_extension (ext, _attrs) ->
        raise (Error_forward (Builtin_attributes.error_of_extension ext))
    | Psig_kind_abbrev _ ->
        Misc.fatal_error "kind_abbrev not supported!"
  in
  let rec transl_sig env sig_items sig_type = function
    | [] -> List.rev sig_items, List.rev sig_type, env
    | item :: srem ->
      let new_item , new_types , env = transl_sig_item env sig_type item in
      transl_sig env
        (new_item :: sig_items)
        (List.rev_append new_types sig_type)
        srem
  in
  let previous_saved_types = Cmt_format.get_saved_types () in
  Builtin_attributes.warning_scope []
    (fun () ->
       let (trem, rem, final_env) =
         transl_sig (Env.in_signature true env) [] [] psg_items
       in
       let rem = Signature_names.simplify final_env names rem in
       let sg =
         { sig_items = trem; sig_type = rem; sig_final_env = final_env;
           sig_modalities; sig_sloc = psg_loc }
       in
       Cmt_format.set_saved_types
         ((Cmt_format.Partial_signature sg) :: previous_saved_types);
       sg
    )

and transl_modtype_decl env pmtd =
  Builtin_attributes.warning_scope pmtd.pmtd_attributes
    (fun () -> transl_modtype_decl_aux env pmtd)

and transl_modtype_decl_aux env
    {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} =
  let tmty =
    Option.map (transl_modtype (Env.in_signature true env)) pmtd_type
  in
  let decl =
    {
     Types.mtd_type=Option.map (fun t -> t.mty_type) tmty;
     mtd_attributes=pmtd_attributes;
     mtd_loc=pmtd_loc;
     mtd_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
    }
  in
  let scope = Ctype.create_scope () in
  let (id, newenv) = Env.enter_modtype ~scope pmtd_name.txt decl env in
  let mtd =
    {
     mtd_id=id;
     mtd_name=pmtd_name;
     mtd_uid=decl.mtd_uid;
     mtd_type=tmty;
     mtd_attributes=pmtd_attributes;
     mtd_loc=pmtd_loc;
    }
  in
  newenv, mtd, decl

and transl_recmodule_modtypes env ~sig_modalities sdecls =
  let make_env curr =
    List.fold_left (fun env (id_shape, _, md, mode, _) ->
      let mode = Option.map Mode.Value.disallow_right mode in
      Option.fold ~none:env ~some:(fun (id, shape) ->
        Env.add_module_declaration ~check:true ~shape ~arg:true
          id Mp_present md ?mode env
      ) id_shape
    ) env curr
  in
  let transition env_c curr =
    List.map2
      (fun (pmd, _) (id_shape, id_loc, md, mmode, _) ->
        let tmty =
          Builtin_attributes.warning_scope pmd.pmd_attributes
            (fun () -> transl_modtype env_c pmd.pmd_type)
        in
        let mty_type, md_modalities =
          apply_pmd_modalities env ~default_modalities:sig_modalities
            pmd.pmd_modalities tmty.mty_type
        in
        let tmty = {tmty with mty_type} in
        let md = { md with Types.md_type = tmty.mty_type; md_modalities } in
        (id_shape, id_loc, md, mmode, tmty))
      sdecls curr in
  let map_mtys curr =
    List.filter_map
      (fun (id_shape, _, md, _, _) ->
         Option.map (fun (id, _) -> (id, md)) id_shape)
      curr
  in
  let scope = Ctype.create_scope () in
  let ids =
    List.map (fun (x, _) -> Option.map (Ident.create_scoped ~scope)
      x.pmd_name.txt)
      sdecls
  in
  let approx_env container =
    List.fold_left
      (fun env ->
         Option.fold ~none:env ~some:(fun id -> (* cf #5965 *)
           Env.enter_unbound_module (Ident.name id)
             (Mod_unbound_illegal_recursion
                { container; unbound = Ident.name id })
             env
         ))
      env ids
  in
  let init =
    List.map2
      (fun id (pmd, smmode) ->
         let md_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) in
         let md_type, md_modalities =
          approx_modtype (approx_env pmd.pmd_name.txt) pmd.pmd_type
          |> apply_pmd_modalities env ~default_modalities:sig_modalities
              pmd.pmd_modalities
         in
         let md =
           { md_type;
             md_modalities;
             md_loc = pmd.pmd_loc;
             md_attributes = pmd.pmd_attributes;
             md_uid }
         in
         let id_shape =
           Option.map (fun id -> id, Shape.var md_uid id) id
         in
         let mmode =
           Option.map (fun smmode ->
            smmode
            |> Typemode.transl_mode_annots
            (* CR zqian: mode annotations on rec modules default to legacy for
            now. We can remove this workaround once [module type of] doesn't
            require zapping. *)
            |> Alloc.Const.Option.value ~default:Alloc.Const.legacy
            |> Alloc.of_const
            |> alloc_as_value) smmode
          in
         (id_shape, pmd.pmd_name, md, mmode, ()))
      ids sdecls
  in
  let env0 = make_env init in
  let dcl1 =
    Warnings.without_warnings
      (fun () -> transition env0 init)
  in
  let env1 = make_env dcl1 in
  check_recmod_typedecls env1 (map_mtys dcl1);
  let dcl2 = transition env1 dcl1 in
(*
  List.iter
    (fun (id, mty) ->
      Format.printf "%a: %a@." Printtyp.ident id Printtyp.modtype mty)
    dcl2;
*)
  let env2 = make_env dcl2 in
  check_recmod_typedecls env2 (map_mtys dcl2);
  let dcl2 =
    List.map2 (fun (pmd, _) (id_shape, id_loc, md, mmode, mty) ->
      let tmd =
        {md_id=Option.map fst id_shape; md_name=id_loc; md_type=mty;
         md_modalities = md.Types.md_modalities;
         md_uid=md.Types.md_uid; md_presence=Mp_present;
         md_loc=pmd.pmd_loc;
         md_attributes=pmd.pmd_attributes}
      in
      tmd, mmode, md.Types.md_uid, Option.map snd id_shape
    ) sdecls dcl2
  in
  (dcl2, env2)

(* Try to convert a module expression to a module path. *)

exception Not_a_path

let rec path_of_module mexp =
  match mexp.mod_desc with
  | Tmod_ident (p,_) -> p
  | Tmod_apply(funct, arg, _coercion) when !Clflags.applicative_functors ->
      Papply(path_of_module funct, path_of_module arg)
  | Tmod_constraint (mexp, _, _, _) ->
      path_of_module mexp
  | (Tmod_structure _ | Tmod_functor _ | Tmod_apply_unit _ | Tmod_unpack _ |
    Tmod_apply _) ->
    raise Not_a_path

let path_of_module mexp =
 try Some (path_of_module mexp) with Not_a_path -> None

(* Check that all core type schemes in a structure
   do not contain non-generalized type variable *)

let rec nongen_modtype env f = function
    Mty_ident _ -> None
  | Mty_alias _ -> None
  | Mty_signature sg ->
      let env = Env.add_signature sg env in
      List.find_map (nongen_signature_item env f) sg
  | Mty_functor(arg_opt, body) ->
      let env =
        match arg_opt with
        | Unit
        | Named (None, _) -> env
        | Named (Some id, param) ->
            Env.add_module ~arg:true id Mp_present param env
      in
      nongen_modtype env f body
  | Mty_strengthen (mty,_ ,_) -> nongen_modtype env f mty

and nongen_signature_item env f = function
  | Sig_value(_id, desc, _) ->
      f env desc.val_type
      |> Option.map (fun vars -> (vars, desc))
  | Sig_module(_id, _, md, _, _) -> nongen_modtype env f md.md_type
  | _ -> None

let check_nongen_modtype env loc mty =
  nongen_modtype env Ctype.nongen_vars_in_schema mty
  |> Option.iter (fun (vars, item) ->
      let vars = Btype.TypeSet.elements vars in
      let error =
        Non_generalizable_module { vars; item; mty }
      in
      raise(Error(loc, env, error))
    )

let check_nongen_signature_item env sig_item =
  match sig_item with
    Sig_value(_id, vd, _) ->
      Ctype.nongen_vars_in_schema env vd.val_type
      |> Option.iter (fun vars ->
          let vars = Btype.TypeSet.elements vars in
          let error =
            Non_generalizable { vars; expression = vd.val_type }
          in
          raise (Error (vd.val_loc, env, error))
        )
  | Sig_module (_id, _, md, _, _) ->
      check_nongen_modtype env md.md_loc md.md_type
  | _ -> ()

let check_nongen_signature env sg =
  List.iter (check_nongen_signature_item env) sg

let remove_mode_and_jkind_variables env sg =
  let rm _env ty = Ctype.remove_mode_and_jkind_variables ty; None in
  List.find_map (nongen_signature_item env rm) sg |> ignore

(* Helpers for typing recursive modules *)

let anchor_submodule name anchor =
  match anchor, name with
  | None, _
  | _, None ->
      None
  | Some p, Some name ->
      Some(Pdot(p, name))

let anchor_recmodule = Option.map (fun id -> Pident id)

let enrich_type_decls anchor decls oldenv newenv =
  match anchor with
    None -> newenv
  | Some p ->
      List.fold_left
        (fun e info ->
          let id = info.typ_id in
          let info' =
            Mtype.enrich_typedecl oldenv (Pdot(p, Ident.name id))
              id info.typ_type
          in
            Env.add_type ~check:true id info' e)
        oldenv decls

let enrich_module_type anchor name mty env =
  match anchor, name with
  | None, _
  | _, None ->
      mty
  | Some p, Some name ->
      Mtype.enrich_modtype env (Pdot(p, name)) mty

let check_recmodule_inclusion env bindings =
  (* PR#4450, PR#4470: consider
        module rec X : DECL = MOD  where MOD has inferred type ACTUAL
     The "natural" typing condition
        E, X: ACTUAL |- ACTUAL <: DECL
     leads to circularities through manifest types.
     Instead, we "unroll away" the potential circularities a finite number
     of times.  The (weaker) condition we implement is:
        E, X: DECL,
           X1: ACTUAL,
           X2: ACTUAL{X <- X1}/X1
           ...
           Xn: ACTUAL{X <- X(n-1)}/X(n-1)
        |- ACTUAL{X <- Xn}/Xn <: DECL{X <- Xn}
     so that manifest types rooted at X(n+1) are expanded in terms of X(n),
     avoiding circularities.  The strengthenings ensure that
     Xn.t = X(n-1).t = ... = X2.t = X1.t.
     N can be chosen arbitrarily; larger values of N result in more
     recursive definitions being accepted.  A good choice appears to be
     the number of mutually recursive declarations. *)

  let subst_and_strengthen scope s id mty =
    let mty = Subst.modtype (Rescope scope) s mty in
    match id with
    | None -> mty
    | Some id ->
        Mtype.strengthen ~aliasable:false mty (Subst.module_path s (Pident id))
  in

  let rec check_incl first_time n env s =
    let scope = Ctype.create_scope () in
    if n > 0 then begin
      (* Generate fresh names Y_i for the rec. bound module idents X_i *)
      let bindings1 =
        List.map
          (fun (id, _name, _mty_decl, _modl,
                mty_actual, _mmode, _attrs, _loc, shape, _uid) ->
             let ids =
               Option.map
                 (fun id -> (id, Ident.create_scoped ~scope (Ident.name id))) id
             in
             (ids, mty_actual, shape))
          bindings in
      (* Enter the Y_i in the environment with their actual types substituted
         by the input substitution s *)
      let env' =
        List.fold_left
          (fun env (ids, mty_actual, shape) ->
             match ids with
             | None -> env
             | Some (id, id') ->
               let mty_actual' =
                 if first_time
                 then mty_actual
                 else subst_and_strengthen scope s (Some id) mty_actual
               in
               Env.add_module ~arg:false ~shape id' Mp_present mty_actual' env)
          env bindings1 in
      (* Build the output substitution Y_i <- X_i *)
      let s' =
        List.fold_left
          (fun s (ids, _mty_actual, _shape) ->
             match ids with
             | None -> s
             | Some (id, id') -> Subst.add_module id (Pident id') s)
          Subst.identity bindings1 in
      (* Recurse with env' and s' *)
      check_incl false (n-1) env' s'
    end else begin
      (* Base case: check inclusion of s(mty_actual) in s(mty_decl)
         and insert coercion if needed *)
      let check_inclusion
            (id, name, mty_decl, modl, mty_actual, mode_decl, attrs, loc, shape
            ,uid) =
        let mty_decl' = Subst.modtype (Rescope scope) s mty_decl.mty_type
        and mty_actual' = subst_and_strengthen scope s id mty_actual in
        let mode_actual, locks = modl.mod_mode in
        let modes : Includemod.modes =
          Specific (
            mode_actual,
            Mode.Value.disallow_left mode_decl,
            locks)
        in
        let coercion, shape =
          try
            Includemod.modtypes_constraint ~shape
              ~loc:modl.mod_loc ~mark:true
              env ~modes mty_actual' mty_decl'
          with Includemod.Error msg ->
            raise(Error(modl.mod_loc, env, Not_included msg)) in
        let modl' =
            { mod_desc = Tmod_constraint(modl, mty_decl.mty_type,
                Tmodtype_explicit mty_decl, coercion);
              mod_type = mty_decl.mty_type;
              mod_mode = Value.disallow_right mode_decl, None;
              mod_env = env;
              mod_loc = modl.mod_loc;
              mod_attributes = [];
             } in
        let mb =
          {
            mb_id = id;
            mb_name = name;
            mb_uid = uid;
            mb_presence = Mp_present;
            mb_expr = modl';
            mb_attributes = attrs;
            mb_loc = loc;
          }
        in
        mb, shape, uid
      in
      List.map check_inclusion bindings
    end
  in check_incl true (List.length bindings) env Subst.identity

(* Helper for unpack *)

let rec package_constraints_sig env loc sg constrs =
  List.map
    (function
      | Sig_type (id, ({type_params=[]} as td), rs, priv)
        when List.mem_assoc [Ident.name id] constrs ->
          let ty = List.assoc [Ident.name id] constrs in
          Sig_type (id, {td with type_manifest = Some ty}, rs, priv)
      | Sig_module (id, pres, md, rs, priv) ->
          let rec aux = function
            | (m :: ((_ :: _) as l), t) :: rest when m = Ident.name id ->
                (l, t) :: aux rest
            | _ :: rest -> aux rest
            | [] -> []
          in
          let md =
            {md with
             md_type = package_constraints env loc md.md_type (aux constrs)
            }
          in
          Sig_module (id, pres, md, rs, priv)
      | item -> item
    )
    sg

and package_constraints env loc mty constrs =
  if constrs = [] then mty
  else begin
    match Mtype.scrape env mty with
    | Mty_signature sg ->
        Mty_signature (package_constraints_sig env loc sg constrs)
    | mty ->
      let rec ident = function
          Mty_ident p -> p
        | Mty_strengthen (mty,_,_) -> ident mty
        | Mty_functor _ | Mty_alias _ | Mty_signature _ -> assert false
      in
      raise(Error(loc, env, Cannot_scrape_package_type (ident mty)))
  end

let modtype_of_package env loc p fl =
  (* We call Ctype.correct_levels to ensure that the types being added to the
     module type are at generic_level. *)
  let mty =
    package_constraints env loc (Mty_ident p)
      (List.map (fun (n, t) -> Longident.flatten n, Ctype.correct_levels t) fl)
  in
  Subst.modtype Keep Subst.identity mty

(* CR zqian: [package_subtype] should take [modes], but piping this through
  [ctype] is too much. Instead, we take the conservative approach. *)
let package_subtype env p1 fl1 p2 fl2 =
  let mkmty p fl =
    let fl =
      List.filter (fun (_n,t) -> Ctype.closed_type_expr t) fl in
    modtype_of_package env Location.none p fl
  in
  match mkmty p1 fl1, mkmty p2 fl2 with
  | exception Error(_, _, Cannot_scrape_package_type _) -> false
  | mty1, mty2 ->
    let loc = Location.none in
    match Includemod.modtypes ~loc ~mark:true env ~modes:All mty1 mty2 with
    | Tcoerce_none -> true
    | _ | exception Includemod.Error _ -> false

let () = Ctype.package_subtype := package_subtype

let wrap_constraint_package env mark arg mty mode explicit =
  let mty1 = Subst.modtype Keep Subst.identity arg.mod_type in
  let mty2 = Subst.modtype Keep Subst.identity mty in
  let arg_mode, held_locks = arg.mod_mode in
  let modes : Includemod.modes =
    Specific (arg_mode, Value.disallow_left mode, held_locks)
  in
  let coercion =
    try
      Includemod.modtypes ~loc:arg.mod_loc env ~mark ~modes mty1 mty2
    with Includemod.Error msg ->
      raise(Error(arg.mod_loc, env, Not_included msg)) in
  { mod_desc = Tmod_constraint(arg, mty, explicit, coercion);
    mod_type = mty;
    mod_mode = Value.disallow_right mode, None;
    mod_env = env;
    mod_attributes = [];
    mod_loc = arg.mod_loc }

let wrap_constraint_with_shape env mark arg mty mode
  shape explicit =
  let arg_mode, held_locks = arg.mod_mode in
  let modes : Includemod.modes =
    Specific (arg_mode, Value.disallow_left mode, held_locks)
  in
  let coercion, shape =
    try
      Includemod.modtypes_constraint ~shape ~loc:arg.mod_loc env ~mark
        ~modes arg.mod_type mty
    with Includemod.Error msg ->
      raise(Error(arg.mod_loc, env, Not_included msg)) in
  { mod_desc = Tmod_constraint(arg, mty, explicit, coercion);
    mod_type = mty;
    mod_mode = Value.disallow_right mode, None;
    mod_env = env;
    mod_attributes = [];
    mod_loc = arg.mod_loc }, shape

(* Type a module value expression *)


(* These describe the X in [F(X)] (which might be missing, for [F ()]) *)
type argument_summary = {
  is_syntactic_unit: bool;
  arg: Typedtree.module_expr;
  path: Path.t option;
  shape: Shape.t
}

type application_summary = {
  loc: Location.t;
  attributes: attributes;
  f_loc: Location.t; (* loc for F *)
  arg: argument_summary option (* None for () *)
}

let simplify_app_summary app_view = match app_view.arg with
  | None ->
    Includemod.Error.Unit, Mty_signature [], Typedtree.min_mode_with_locks
  | Some arg ->
    let mty = arg.arg.mod_type in
    let mode = arg.arg.mod_mode in
    match arg.is_syntactic_unit , arg.path with
    | true , _      -> Includemod.Error.Empty_struct, mty, mode
    | false, Some p -> Includemod.Error.Named p, mty, mode
    | false, None   -> Includemod.Error.Anonymous, mty, mode

let infer_modalities ~loc ~env ~md_mode ~mode =
    (* Values are packed into a structure at modes weaker than they actually
      are. This is to allow our legacy zapping behavior. For example:

      module M = struct
        let foo x = x
        let bar = use_portable foo
      end
      module type S = module type of M
      use_portable M.foo

      would type error at the last line.
    *)
    let mode, _ = Mode.Value.newvar_above mode in
    (* Upon construction, for comonadic (prescriptive) axes, module
    must be weaker than the values therein, for otherwise operations
    would be allowed to performed on the module (and extended to the
    values) that's disallowed for the values.

    For monadic (descriptive) axes, the restriction is not on the
    construction but on the projection, which is modelled by the
    [Diff] modality in [mode.ml]. *)
    begin match Mode.Value.Comonadic.submode
      mode.Mode.comonadic
      md_mode.Mode.comonadic with
      | Ok () -> ()
      | Error (Error (ax, e)) ->
          raise (Error (loc, env, Item_weaker_than_structure
            (Error (Comonadic ax, e))))
    end;
    Mode.Modality.Value.infer ~md_mode ~mode

(** Given a signature [sg] to be included in a structure. The signature contains
  modalities relative to [mode], this function returns a signature with
  modalities relative to [md_mode].  *)
let rebase_modalities ~loc ~env ~md_mode ~mode sg =
  List.map (function
    | Sig_value (id, vd, vis) ->
        let mode = Mode.Modality.Value.apply vd.val_modalities mode in
        let val_modalities = infer_modalities ~loc ~env ~md_mode ~mode in
        let vd = {vd with val_modalities} in
        Sig_value (id, vd, vis)
    | Sig_module (id, pres, md, rec_, vis) ->
        let mode = Mode.Modality.Value.apply md.md_modalities mode in
        let md_modalities = infer_modalities ~loc ~env ~md_mode ~mode in
        let md = {md with md_modalities} in
        Sig_module (id, pres, md, rec_, vis)
    | item -> item
    ) sg

let rec type_module ?alias sttn funct_body anchor env ?expected_mode smod =
  let md, shape =
    type_module_maybe_hold_locks ?alias ~hold_locks:false sttn funct_body anchor
      env ?expected_mode smod
  in
  md, shape

and  type_module_maybe_hold_locks ?(alias=false) ~hold_locks sttn funct_body
  anchor env ?expected_mode smod =
  Builtin_attributes.warning_scope smod.pmod_attributes
    (fun () -> type_module_aux ~alias ~hold_locks sttn funct_body anchor env
      ?expected_mode smod)

and type_module_aux ~alias ~hold_locks sttn funct_body anchor env
  ?expected_mode smod =
  (* If the module is an identifier, there might be locks between the
  declaration site and the use site.
  - If [hold_locks] is [true], the locks are held and stored in [mod_mode].
  - If [hold_locks] is [false], the locks are walked.

  If the module is not an identifier, [hold_locks] has no effect. *)
  match smod.pmod_desc with
    Pmod_ident lid ->
      let path, mode_with_locks =
        Env.lookup_module_path ~load:(not alias) ~loc:smod.pmod_loc lid.txt env
      in
      type_module_path_aux ~alias ~hold_locks sttn env path mode_with_locks lid
        smod
  | Pmod_structure sstr ->
      let (str, sg, mode, names, shape, _finalenv) =
        type_structure funct_body anchor env ?expected_mode sstr in
      let md =
        { mod_desc = Tmod_structure str;
          mod_type = Mty_signature sg;
          mod_mode = Value.disallow_right mode, None;
          mod_env = env;
          mod_attributes = smod.pmod_attributes;
          mod_loc = smod.pmod_loc }
      in
      let sg' = Signature_names.simplify _finalenv names sg in
      let md, shape =
        if List.length sg' = List.length sg then md, shape else
        wrap_constraint_with_shape env false md
          (Mty_signature sg') mode shape Tmodtype_implicit
      in
      md, shape
  | Pmod_functor(arg_opt, sbody) ->
      let _, mode = register_allocation () in
      Option.iter (fun x -> Value.submode mode x |> ignore) expected_mode;
      let newenv = Env.add_closure_lock Functor mode.comonadic env in
      let t_arg, ty_arg, newenv, funct_shape_param, funct_body =
        match arg_opt with
        | Unit ->
          Unit, Types.Unit, newenv, Shape.for_unnamed_functor_param, false
        | Named (param, smty, smode) ->
          check_unsupported_modal_module ~env Functor_param smode;
          let mty = transl_modtype_functor_arg env smty in
          let scope = Ctype.create_scope () in
          let (id, newenv, var) =
            match param.txt with
            | None -> None, newenv, Shape.for_unnamed_functor_param
            | Some name ->
              let md_uid =  Uid.mk ~current_unit:(Env.get_unit_name ()) in
              let arg_md =
                { md_type = mty.mty_type;
                  md_modalities = Modality.Value.id;
                  md_attributes = [];
                  md_loc = param.loc;
                  md_uid;
                }
              in
              let id = Ident.create_scoped ~scope name in
              let shape = Shape.var md_uid id in
              let mode = alloc_as_value functor_param_mode in
              let newenv = Env.add_module_declaration
                ~shape ~arg:true ~check:true id Mp_present arg_md ~mode newenv
              in
              Some id, newenv, id
          in
          Named (id, param, mty), Types.Named (id, mty.mty_type), newenv,
          var, true
      in
      let expected_mode =
        alloc_as_value Types.functor_res_mode |> Value.disallow_left
      in
      let body, body_shape =
        type_module true funct_body None newenv ~expected_mode sbody
      in
      let body_mode = mode_without_locks_exn body.mod_mode in
      begin match Value.submode body_mode  expected_mode with
      | Ok () -> ()
      | Error e -> raise (Error (sbody.pmod_loc, newenv,
          Legacy_module (Functor_body, e)))
      end;
      { mod_desc = Tmod_functor(t_arg, body);
        mod_type = Mty_functor(ty_arg, body.mod_type);
        mod_mode = Value.disallow_right mode, None;
        mod_env = env;
        mod_attributes = smod.pmod_attributes;
        mod_loc = smod.pmod_loc },
      Shape.abs funct_shape_param body_shape
  | Pmod_apply _ | Pmod_apply_unit _ ->
      type_application smod.pmod_loc sttn funct_body env smod
  | Pmod_constraint(sarg, smty, smode) ->
      (* Only hold locks if coercion *)
      let hold_locks = Option.is_some smty in
      let mode = smode
        |> Typemode.transl_mode_annots
        |> new_mode_var_from_annots
      in
      let arg, arg_shape =
        type_module_maybe_hold_locks ~alias ~hold_locks true funct_body
          anchor env ~expected_mode:(mode |> Value.disallow_left) sarg
      in
      let md, final_shape =
        match smty with
        | None ->
            (* CR zqian: Ideally, we want to call [wrap_constraint_with_shape]
            even when [smty] is [None], to get a mode error messsage that
            specifies the bad item (instead of the whole module). This is
            currently impossible because inferred modalities can't be on the
            RHS. *)
            let arg_mode = Typedtree.mode_without_locks_exn arg.mod_mode in
            submode ~loc:sarg.pmod_loc ~env arg_mode mode;
            { arg with mod_mode = (Mode.Value.disallow_right mode, None)},
            arg_shape
        | Some smty ->
            let mty = transl_modtype env smty in
            wrap_constraint_with_shape env true arg mty.mty_type mode
              arg_shape (Tmodtype_explicit mty)
      in
      { md with
        mod_loc = smod.pmod_loc;
        mod_attributes = smod.pmod_attributes;
      },
      final_shape
  | Pmod_unpack sexp ->
      let mode = Value.newvar () in
      let exp =
        Ctype.with_local_level_if_principal
          (fun () -> Typecore.type_exp env sexp
            ~mode:(Value.disallow_left mode))
          ~post:Typecore.generalize_structure_exp
      in
      let mty =
        match get_desc (Ctype.expand_head env exp.exp_type) with
          Tpackage (p, fl) ->
            if List.exists (fun (_n, t) -> not (Ctype.closed_type_expr t)) fl
            then
              raise (Error (smod.pmod_loc, env,
                            Incomplete_packed_module exp.exp_type));
            if !Clflags.principal &&
              not (Typecore.generalizable (Btype.generic_level-1) exp.exp_type)
            then
              Location.prerr_warning smod.pmod_loc
                (Warnings.Not_principal "this module unpacking");
            modtype_of_package env smod.pmod_loc p fl
        | Tvar _ ->
            raise (Typecore.Error
                     (smod.pmod_loc, env, Typecore.Cannot_infer_signature))
        | _ ->
            raise (Error(smod.pmod_loc, env, Not_a_packed_module exp.exp_type))
      in
      if funct_body && Mtype.contains_type env mty then
        raise (Error (smod.pmod_loc, env, Not_allowed_in_functor_body));
      { mod_desc = Tmod_unpack(exp, mty);
        mod_type = mty;
        mod_mode = Value.disallow_right mode, None;
        mod_env = env;
        mod_attributes = smod.pmod_attributes;
        mod_loc = smod.pmod_loc },
      Shape.leaf_for_unpack
  | Pmod_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))
  | Pmod_instance glob ->
      Language_extension.assert_enabled ~loc:smod.pmod_loc Instances ();
      let glob = instance_name ~loc:smod.pmod_loc env glob in
      let path, locks =
        Env.lookup_module_instance_path ~load:(not alias) ~loc:smod.pmod_loc
          glob env
      in
      let mode_with_locks = (Value.disallow_right Env.mode_unit, locks) in
      let lid =
        (* Only used by [untypeast] *)
        let name =
          Format.asprintf "*instance %a*" Global_module.Name.print glob
        in
        Location.(mkloc (Lident name) (ghostify smod.pmod_loc))
      in
      type_module_path_aux ~alias ~hold_locks sttn env path mode_with_locks lid
        smod

and type_module_path_aux ~alias ~hold_locks sttn env path
  (mode, locks) (lid : _ loc) smod =
  let mod_mode =
    if hold_locks then mode, Some (locks, lid.txt, lid.loc)
    else
      let vmode =
        Env.walk_locks ~env ~loc:lid.loc lid.txt ~item:Module None (mode, locks)
      in
      vmode.mode, None
  in
  let md = { mod_desc = Tmod_ident (path, lid);
             mod_type = Mty_alias path;
             mod_mode;
             mod_env = env;
             mod_attributes = smod.pmod_attributes;
             mod_loc = smod.pmod_loc } in
  let aliasable = not (Env.is_functor_arg path env) in
  let shape =
    Env.shape_of_path ~namespace:Shape.Sig_component_kind.Module env path
  in
  let shape = if alias && aliasable then Shape.alias shape else shape in
  let md =
    if alias && aliasable then
      (Env.add_required_global path env; md)
    else begin
      let mty = Mtype.find_type_of_module
          ~strengthen:sttn ~aliasable env path
      in
      match mty with
      | Mty_alias p1 when not alias ->
          let p1 = Env.normalize_module_path (Some smod.pmod_loc) env p1 in
          let mty = Includemod.expand_module_alias
              ~strengthen:sttn env p1 in
          { md with
            mod_desc =
              Tmod_constraint (md, mty, Tmodtype_implicit,
                               Tcoerce_alias (env, path, Tcoerce_none));
            mod_type = mty }
      | mty ->
          { md with mod_type = mty }
    end
  in
  md, shape

and type_application loc strengthen funct_body env smod =
  let rec extract_application funct_body env sargs smod =
    match smod.pmod_desc with
    | Pmod_apply(f, sarg) ->
        let arg, shape =
          type_module_maybe_hold_locks ~hold_locks:true true funct_body None env
            sarg
        in
        let summary = {
          loc = smod.pmod_loc;
          attributes = smod.pmod_attributes;
          f_loc = f.pmod_loc;
          arg = Some {
            is_syntactic_unit = sarg.pmod_desc = Pmod_structure [];
            arg;
            path = path_of_module arg;
            shape;
          }
        } in
        extract_application funct_body env (summary::sargs) f
    | Pmod_apply_unit f ->
        let summary = {
          loc = smod.pmod_loc;
          attributes = smod.pmod_attributes;
          f_loc = f.pmod_loc;
          arg = None
        } in
        extract_application funct_body env (summary::sargs) f
    | _ -> smod, sargs
  in
  let sfunct, args = extract_application funct_body env [] smod in
  let funct, funct_shape =
    let has_path { arg } = match arg with
      | None | Some { path = None } -> false
      | Some { path = Some _ } -> true
    in
    let strengthen = strengthen && List.for_all has_path args in
    type_module strengthen funct_body None env sfunct
  in
  List.fold_left
    (type_one_application ~ctx:(loc, sfunct, funct, args) funct_body env)
    (funct, funct_shape) args

and type_one_application ~ctx:(apply_loc,sfunct,md_f,args)
    funct_body env (funct, funct_shape) app_view =
  match Mtype.scrape_alias env funct.mod_type with
  | Mty_functor (Unit, mty_res) ->
      begin match app_view.arg with
        | None -> ()
        | Some arg ->
          if arg.is_syntactic_unit then
            (* this call to warning_scope allows e.g.
               [ F (struct end [@warning "-73"]) ]
               not to warn; useful when generating code that must
               work over multiple versions of OCaml *)
            Builtin_attributes.warning_scope arg.arg.mod_attributes @@ fun () ->
            Location.prerr_warning arg.arg.mod_loc
              Warnings.Generative_application_expects_unit
          else
            raise (Error (app_view.f_loc, env, Apply_generative));
      end;
      if funct_body && Mtype.contains_type env funct.mod_type then
        raise (Error (apply_loc, env, Not_allowed_in_functor_body));
      { mod_desc = Tmod_apply_unit funct;
        mod_type = mty_res;
        mod_mode = alloc_as_value (Alloc.disallow_right functor_res_mode), None;
        mod_env = env;
        mod_attributes = app_view.attributes;
        mod_loc = funct.mod_loc },
      Shape.app funct_shape ~arg:Shape.dummy_mod
  | Mty_functor (Named (param, mty_param), mty_res) as mty_functor ->
      let apply_error () =
        let args = List.map simplify_app_summary args in
        let mty_f = md_f.mod_type in
        let app_name = match sfunct.pmod_desc with
          | Pmod_ident l -> Includemod.Named_leftmost_functor l.txt
          | _ -> Includemod.Anonymous_functor
        in
        raise(Includemod.Apply_error {loc=apply_loc;env;app_name;mty_f;args})
      in
      begin match app_view with
      | { arg = None; _ } -> apply_error ()
      | { loc = app_loc; attributes = app_attributes;
          arg = Some { shape = arg_shape; path = arg_path; arg } } ->
      let coercion =
        try Includemod.modtypes
              ~loc:arg.mod_loc ~mark:true env arg.mod_type mty_param
              ~modes:(Includemod.modes_functor_param arg.mod_mode)
        with Includemod.Error _ -> apply_error ()
      in
      let mty_appl =
        match arg_path with
        | Some path ->
            let scope = Ctype.create_scope () in
            let subst =
              match param with
              | None -> Subst.identity
              | Some p -> Subst.add_module p path Subst.identity
            in
            Subst.modtype (Rescope scope) subst mty_res
        | None ->
            let env, nondep_mty =
              match param with
              | None -> env, mty_res
              | Some param ->
                  let env =
                    Env.add_module ~arg:true param Mp_present arg.mod_type env
                  in
                  check_well_formed_module env app_loc
                    "the signature of this functor application" mty_res;
                  try env, Mtype.nondep_supertype env [param] mty_res
                  with Ctype.Nondep_cannot_erase _ ->
                    let error = Cannot_eliminate_dependency
                                  (Functor_applied, mty_functor) in
                    raise (Error(app_loc, env, error))
            in
            begin match
              Includemod.modtypes
                ~loc:app_loc ~mark:false env mty_res nondep_mty
                ~modes:Includemod.modes_functor_res
            with
            | Tcoerce_none -> ()
            | _ ->
                fatal_error
                  "unexpected coercion from original module type to \
                   nondep_supertype one"
            | exception Includemod.Error _ ->
                fatal_error
                  "nondep_supertype not included in original module type"
            end;
            nondep_mty
      in
      check_well_formed_module env apply_loc
        "the signature of this functor application" mty_appl;
      { mod_desc = Tmod_apply(funct, arg, coercion);
        mod_type = mty_appl;
        mod_mode = alloc_as_value (Alloc.disallow_right functor_res_mode), None;
        mod_env = env;
        mod_attributes = app_attributes;
        mod_loc = app_loc },
      Shape.app ~arg:arg_shape funct_shape
    end
  | Mty_alias path ->
      raise(Error(app_view.f_loc, env, Cannot_scrape_alias path))
  | Mty_ident _ | Mty_signature _ | Mty_strengthen _ ->
      let args = List.map simplify_app_summary args in
      let mty_f = md_f.mod_type in
      let app_name = match sfunct.pmod_desc with
        | Pmod_ident l -> Includemod.Named_leftmost_functor l.txt
        | _ -> Includemod.Anonymous_functor
      in
      raise(Includemod.Apply_error {loc=apply_loc;env;app_name;mty_f;args})

and type_open_decl ?used_slot ?toplevel funct_body names env sod =
  Builtin_attributes.warning_scope sod.popen_attributes
    (fun () ->
       type_open_decl_aux ?used_slot ?toplevel funct_body names env sod
    )

and type_open_decl_aux ?used_slot ?toplevel funct_body names env od =
  let loc = od.popen_loc in
  match od.popen_expr.pmod_desc with
  | Pmod_ident lid ->
    let path, (mode, locks), newenv =
      type_open_ ?used_slot ?toplevel od.popen_override env loc lid
    in
    let md = { mod_desc = Tmod_ident (path, lid);
               mod_type = Mty_alias path;
               mod_mode = mode, Some (locks, lid.txt, lid.loc);
               mod_env = env;
               mod_attributes = od.popen_expr.pmod_attributes;
               mod_loc = od.popen_expr.pmod_loc }
    in
    let open_descr = {
      open_expr = md;
      open_bound_items = [];
      open_override = od.popen_override;
      open_env = newenv;
      open_loc = loc;
      open_attributes = od.popen_attributes
    } in
    open_descr, Mode.Value.(max |> disallow_right), [], newenv
  | _ ->
    let md, mod_shape = type_module true funct_body None env od.popen_expr in
    let mode = mode_without_locks_exn md.mod_mode in
    let scope = Ctype.create_scope () in
    let sg, newenv =
      Env.enter_signature ~scope ~mod_shape
        (extract_sig_open env md.mod_loc md.mod_type) ~mode env
    in
    let info, visibility =
      match toplevel with
      | Some false | None -> Some `From_open, Hidden
      | Some true -> None, Exported
    in
    Signature_group.iter (Signature_names.check_sig_item ?info names loc) sg;
    let sg =
      List.map (function
        | Sig_value(id, vd, _) -> Sig_value(id, vd, visibility)
        | Sig_type(id, td, rs, _) -> Sig_type(id, td, rs, visibility)
        | Sig_typext(id, ec, et, _) -> Sig_typext(id, ec, et, visibility)
        | Sig_module(id, mp, md, rs, _) ->
            Sig_module(id, mp, md, rs, visibility)
        | Sig_modtype(id, mtd, _) -> Sig_modtype(id, mtd, visibility)
        | Sig_class(id, cd, rs, _) -> Sig_class(id, cd, rs, visibility)
        | Sig_class_type(id, ctd, rs, _) ->
            Sig_class_type(id, ctd, rs, visibility)
      ) sg
    in
    let open_descr = {
      open_expr = md;
      open_bound_items = sg;
      open_override = od.popen_override;
      open_env = newenv;
      open_loc = loc;
      open_attributes = od.popen_attributes
    } in
    open_descr, mode, sg, newenv

and type_structure ?(toplevel = None) funct_body anchor env ?expected_mode
  sstr =
  let names = Signature_names.create () in
  let _, md_mode = register_allocation () in
  Option.iter (fun x -> Value.submode md_mode x |> ignore)
    expected_mode;

  let type_str_include ~loc env shape_map sincl sig_acc =
    let smodl = sincl.pincl_mod in
    let modl, modl_shape =
      Builtin_attributes.warning_scope sincl.pincl_attributes
        (fun () -> type_module true funct_body None env smodl)
    in
    let scope = Ctype.create_scope () in
    let incl_kind, sg, mode =
      match sincl.pincl_kind with
      | Functor ->
        Language_extension.assert_enabled ~loc Include_functor ();
        let sg, incl_kind =
          extract_sig_functor_open funct_body env smodl.pmod_loc
            modl.mod_type sig_acc
        in
        incl_kind, sg, alloc_as_value (Alloc.disallow_right functor_res_mode)
      | Structure ->
        Tincl_structure, extract_sig_open env smodl.pmod_loc modl.mod_type,
          (Typedtree.mode_without_locks_exn modl.mod_mode)
    in
    (* Rename all identifiers bound by this signature to avoid clashes *)
    let sg, shape, new_env =
      Env.enter_signature_and_shape ~scope ~parent_shape:shape_map
        modl_shape sg ~mode env
    in
    let sg = rebase_modalities ~loc ~env ~md_mode ~mode sg in
    Signature_group.iter (Signature_names.check_sig_item names loc) sg;
    let incl =
      { incl_mod = modl;
        incl_type = sg;
        incl_kind;
        incl_attributes = sincl.pincl_attributes;
        incl_loc = sincl.pincl_loc;
      }
    in
    Tstr_include incl, sg, shape, new_env
  in

  let force_toplevel =
    (* A couple special cases are needed for the toplevel:

       - Expressions bound by '_' still escape in the toplevel, because they may
         be printed even though they are not named, and therefore can't be local
       - Those expressions and also all [Pstr_eval]s must have types of layout
         value for the same reason (see the special case in
         [Opttoploop.execute_phrase]).
    *)
    Option.is_some toplevel
  in

  let type_str_item
        env shape_map {pstr_loc = loc; pstr_desc = desc} sig_acc =
    match desc with
    | Pstr_eval (sexpr, attrs) ->
        let expr, sort =
          (* We could consider allowing [any] here when not in the toplevel,
             though for now the sort is used in the void safety check. *)
          Builtin_attributes.warning_scope attrs
            (fun () -> Typecore.type_representable_expression
                         ~why:Structure_item_expression env sexpr)
        in
        if force_toplevel then
          (* See comment on [force_toplevel]. *)
          begin match Jkind.Sort.default_to_value_and_get sort with
          | Base Value -> ()
          | Product _
          | Base (Void | Float64 | Float32 | Word | Bits8 | Bits16 | Bits32
                 | Bits64 | Vec128 | Vec256 | Vec512) ->
            raise (Error (sexpr.pexp_loc, env, Toplevel_unnamed_nonvalue sort))
          end;
        Tstr_eval (expr, sort, attrs), [], shape_map, env
    | Pstr_value (rec_flag, sdefs) ->
        let (defs, newenv) =
          Typecore.type_binding env Immutable rec_flag ~force_toplevel sdefs in
        let defs = match rec_flag with
          | Recursive -> Typecore.annotate_recursive_bindings env defs
          | Nonrecursive -> defs
        in
        if force_toplevel then
          (* See comment on [force_toplevel] *)
          List.iter (fun vb ->
            match vb.vb_pat.pat_desc with
            | Tpat_any ->
              begin match Jkind.Sort.default_to_value_and_get vb.vb_sort with
              | Base Value -> ()
              | Product _
              | Base (Void | Float64 | Float32 | Word | Bits8 | Bits16 | Bits32
                     | Bits64 | Vec128 | Vec256 | Vec512) ->
                raise (Error (vb.vb_loc, env,
                              Toplevel_unnamed_nonvalue vb.vb_sort))
              end
            | _ -> ()
          ) defs;
        (* Note: Env.find_value does not trigger the value_used event. Values
           will be marked as being used during the signature inclusion test. *)
        let items, shape_map =
          List.fold_left
            (fun (acc, shape_map) (id, id_info, zero_alloc) ->
              List.iter
                (fun (loc, _mode, sort) ->
                   (* CR layouts v5: this jkind check has the effect of
                      defaulting the sort of top-level bindings to value, which
                      will change. *)
                   if not Jkind.Sort.(equate sort value)
                   then raise (Error (loc, env,
                                   Toplevel_nonvalue (Ident.name id,sort)))
                )
                id_info;
              let zero_alloc =
                (* We only allow "Check" attributes in signatures.  Here we
                   convert "Assume"s in structures to the equivalent "Check" for
                   the signature. *)
                let open Builtin_attributes in
                match[@warning "+9"] Zero_alloc.get zero_alloc with
                | Default_zero_alloc | Check _ -> zero_alloc
                | Assume { strict; arity; loc;
                           never_returns_normally = _;
                           never_raises = _} ->
                  Zero_alloc.create_const
                    (Check { strict; arity; loc; opt = false;
                             custom_error_msg = None; })
                | Ignore_assert_all -> Zero_alloc.default
              in
              let (first_loc, _, _) = List.hd id_info in
              Signature_names.check_value names first_loc id;
              let vd, mode =  Env.find_value_no_locks_exn id newenv in
              let vd = Subst.Lazy.force_value_description vd in
              let modalities =
                infer_modalities ~loc:first_loc ~env ~md_mode ~mode
              in
              let vd =
                { vd with
                  val_zero_alloc = zero_alloc;
                  val_modalities = modalities }
              in
              Sig_value(id, vd, Exported) :: acc,
              Shape.Map.add_value shape_map id vd.val_uid
            )
            ([], shape_map)
            (let_bound_idents_with_modes_sorts_and_checks defs)
        in
        Tstr_value(rec_flag, defs),
        List.rev items,
        shape_map,
        newenv
    | Pstr_primitive sdesc ->
        (* primitive in structure still carries modalities, which doesn't make
        sense. We convert them to modes. *)
        (* CR zqian: remove this hack *)
        let modality_to_mode {txt = Modality m; loc} = {txt = Mode m; loc} in
        let modes = List.map modality_to_mode sdesc.pval_modalities in
        let mode =
          modes
          |> Typemode.transl_alloc_mode
          |> Alloc.of_const
          |> alloc_as_value
        in
        let modalities = infer_modalities ~loc ~env ~md_mode ~mode in
        let (desc, newenv) =
          Typedecl.transl_value_decl env ~modalities loc sdesc
        in
        Signature_names.check_value names desc.val_loc desc.val_id;
        Tstr_primitive desc,
        [Sig_value(desc.val_id, desc.val_val, Exported)],
        Shape.Map.add_value shape_map desc.val_id desc.val_val.val_uid,
        newenv
    | Pstr_type (rec_flag, sdecls) ->
        let (decls, newenv, shapes) =
          Typedecl.transl_type_decl env rec_flag sdecls
        in
        List.iter
          Signature_names.(fun td -> check_type names td.typ_loc td.typ_id)
          decls;
        let items = map_rec_type_with_row_types ~rec_flag
          (fun rs info -> Sig_type(info.typ_id, info.typ_type, rs, Exported))
          decls
        in
        let shape_map = List.fold_left2
          (fun map { typ_id; _} shape ->
            Shape.Map.add_type map typ_id shape)
          shape_map
          decls
          shapes
        in
        Tstr_type (rec_flag, decls),
        items,
        shape_map,
        enrich_type_decls anchor decls env newenv
    | Pstr_typext styext ->
        let (tyext, newenv, shapes) =
          Typedecl.transl_type_extension true env loc styext
        in
        let constructors = tyext.tyext_constructors in
        let shape_map = List.fold_left2 (fun shape_map ext shape ->
            Signature_names.check_typext names ext.ext_loc ext.ext_id;
            Shape.Map.add_extcons shape_map ext.ext_id shape
          ) shape_map constructors shapes
        in
        (Tstr_typext tyext,
         map_ext
           (fun es ext -> Sig_typext(ext.ext_id, ext.ext_type, es, Exported))
           constructors,
        shape_map,
         newenv)
    | Pstr_exception sext ->
        let (ext, newenv, shape) = Typedecl.transl_type_exception env sext in
        let constructor = ext.tyexn_constructor in
        Signature_names.check_typext names constructor.ext_loc
          constructor.ext_id;
        Tstr_exception ext,
        [Sig_typext(constructor.ext_id,
                    constructor.ext_type,
                    Text_exception,
                    Exported)],
        Shape.Map.add_extcons shape_map
          constructor.ext_id
          shape,
        newenv
    | Pstr_module {pmb_name = name; pmb_expr = smodl; pmb_attributes = attrs;
                   pmb_loc;
                  } ->
        let outer_scope = Ctype.get_current_level () in
        let scope = Ctype.create_scope () in
        let modl, md_shape =
          Builtin_attributes.warning_scope attrs
            (fun () ->
               type_module ~alias:true true funct_body
                 (anchor_submodule name.txt anchor) env smodl
            )
        in
        let pres =
          match modl.mod_type with
          | Mty_alias _ -> Mp_absent
          | _ -> Mp_present
        in
        let md_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) in
        let mode = mode_without_locks_exn modl.mod_mode in
        let md_modalities = infer_modalities ~loc:pmb_loc ~env ~md_mode ~mode in
        let md =
          { md_type = enrich_module_type anchor name.txt modl.mod_type env;
            md_modalities;
            md_attributes = attrs;
            md_loc = pmb_loc;
            md_uid;
          }
        in
        let md_shape = Shape.set_uid_if_none md_shape md_uid in
        (*prerr_endline (Ident.unique_toplevel_name id);*)
        Mtype.lower_nongen outer_scope md.md_type;
        let id, newenv, sg =
          match name.txt with
          | None -> None, env, []
          | Some name ->
            let id, e = Env.enter_module_declaration
              ~scope ~shape:md_shape name pres md ~mode:md_mode env
            in
            Signature_names.check_module names pmb_loc id;
            Some id, e,
            [Sig_module(id, pres,
                        {md_type = modl.mod_type;
                         md_modalities;
                         md_attributes = attrs;
                         md_loc = pmb_loc;
                         md_uid;
                        }, Trec_not, Exported)]
        in
        let shape_map = match id with
          | Some id -> Shape.Map.add_module shape_map id md_shape
          | None -> shape_map
        in
        Tstr_module {mb_id=id; mb_name=name; mb_uid = md.md_uid;
                     mb_expr=modl; mb_presence=pres; mb_attributes=attrs;
                     mb_loc=pmb_loc; },
        sg,
        shape_map,
        newenv
    | Pstr_recmodule sbind ->
        let sbind =
          List.map
            (function
              | {pmb_name = name;
                 pmb_expr = {pmod_desc=Pmod_constraint(expr, Some typ, mode)};
                 pmb_attributes = attrs;
                 pmb_loc = loc;
                } ->
                  name, typ, mode, expr, attrs, loc
              | mb ->
                  raise (Error (mb.pmb_expr.pmod_loc, env,
                                Recursive_module_require_explicit_type))
            )
            sbind
        in
        let (decls, newenv) =
          transl_recmodule_modtypes env
            ~sig_modalities:Mode.Modality.Value.Const.id
            (List.map (fun (name, smty, smode, _smodl, attrs, loc) ->
                 ({pmd_name=name; pmd_type=smty;
                   pmd_attributes=attrs; pmd_loc=loc; pmd_modalities=[]}
                  , Some smode)) sbind
            ) in
        List.iter
          (fun (md, _, _, _) ->
             Option.iter Signature_names.(check_module names md.md_loc) md.md_id
          ) decls;
        let bindings1 =
          List.map2
            (fun ({md_id=id; md_type=mty}, mode, uid, _prev_shape)
                 (name, _, _, smodl, attrs, loc) ->
               let modl, shape =
                 Builtin_attributes.warning_scope attrs
                   (fun () ->
                      type_module true funct_body (anchor_recmodule id)
                        newenv smodl
                   )
               in
               let mty' =
                 enrich_module_type anchor name.txt modl.mod_type newenv
               in
               (id, name, mty, modl, mty', Option.get mode, attrs, loc, shape,
                uid))
            decls sbind in
        let newenv = (* allow aliasing recursive modules from outside *)
          List.fold_left
            (fun env (id_opt, _, mty, _, _, mode, attrs, loc, shape, uid) ->
               match id_opt with
               | None -> env
               | Some id ->
                   let mdecl =
                     {
                       md_type = mty.mty_type;
                       md_modalities = Modality.Value.id;
                       md_attributes = attrs;
                       md_loc = loc;
                       md_uid = uid;
                     }
                   in
                   Env.add_module_declaration ~check:true ~shape
                     id Mp_present mdecl ~mode env
            )
            env bindings1
        in
        let bindings2 =
          check_recmodule_inclusion newenv bindings1 in
        let mbs =
          List.filter_map (fun (mb, shape, uid) ->
            Option.map (fun id -> id, mb, uid, shape)  mb.mb_id
          ) bindings2
        in
        let shape_map =
          List.fold_left (fun map (id, _mb, _uid, shape) ->
            Shape.Map.add_module map id shape
          ) shape_map mbs
        in
        Tstr_recmodule (List.map (fun (mb, _, _) -> mb) bindings2),
        map_rec (fun rs (id, mb, uid, _shape) ->
            let mode = mode_without_locks_exn mb.mb_expr.mod_mode in
            let md_modalities =
              infer_modalities ~loc:mb.mb_loc ~env ~md_mode ~mode
            in
            Sig_module(id, Mp_present, {
                md_type=mb.mb_expr.mod_type;
                md_modalities;
                md_attributes=mb.mb_attributes;
                md_loc=mb.mb_loc;
                md_uid = uid;
              }, rs, Exported))
           mbs [],
        shape_map,
        newenv
    | Pstr_modtype pmtd ->
        (* check that it is non-abstract *)
        let newenv, mtd, decl = transl_modtype_decl env pmtd in
        Signature_names.check_modtype names pmtd.pmtd_loc mtd.mtd_id;
        let id = mtd.mtd_id in
        let map = Shape.Map.add_module_type shape_map id decl.mtd_uid in
        Tstr_modtype mtd, [Sig_modtype (id, decl, Exported)], map, newenv
    | Pstr_open sod ->
        let toplevel = Option.is_some toplevel in
        let (od, mode, sg, newenv) =
          type_open_decl ~toplevel funct_body names env sod
        in
        let sg = rebase_modalities ~loc ~env ~md_mode ~mode sg in
        Tstr_open od, sg, shape_map, newenv
    | Pstr_class cl ->
        begin match Mode.Value.submode Value.legacy md_mode with
          | Ok () -> ()
          | Error e ->
              raise (Error (loc, env, Item_weaker_than_structure e))
        end;
        let (classes, new_env) = Typeclass.class_declarations env cl in
        let shape_map = List.fold_left (fun acc cls ->
            let open Typeclass in
            let loc = cls.cls_id_loc.Location.loc in
            Signature_names.check_class names loc cls.cls_id;
            Signature_names.check_class_type names loc cls.cls_ty_id;
            Signature_names.check_type names loc cls.cls_obj_id;
            let uid = cls.cls_decl.cty_uid in
            let map f id v acc = f acc id v in
            map Shape.Map.add_class cls.cls_id uid acc
            |> map Shape.Map.add_class_type cls.cls_ty_id uid
            |> map Shape.Map.add_type cls.cls_obj_id (Shape.leaf uid)
          ) shape_map classes
        in
        Tstr_class
          (List.map (fun cls ->
               (cls.Typeclass.cls_info,
                cls.Typeclass.cls_pub_methods)) classes),
        List.flatten
          (map_rec
            (fun rs cls ->
              let open Typeclass in
              [Sig_class(cls.cls_id, cls.cls_decl, rs, Exported);
               Sig_class_type(cls.cls_ty_id, cls.cls_ty_decl, rs, Exported);
               Sig_type(cls.cls_obj_id, cls.cls_obj_abbr, rs, Exported)
              ])
             classes []),
        shape_map,
        new_env
    | Pstr_class_type cl ->
        let (classes, new_env) = Typeclass.class_type_declarations env cl in
        let shape_map = List.fold_left (fun acc decl ->
            let open Typeclass in
            let loc = decl.clsty_id_loc.Location.loc in
            Signature_names.check_class_type names loc decl.clsty_ty_id;
            Signature_names.check_type names loc decl.clsty_obj_id;
            let uid = decl.clsty_ty_decl.clty_uid in
            let map f id v acc = f acc id v in
            map Shape.Map.add_class_type decl.clsty_ty_id uid acc
            |> map Shape.Map.add_type decl.clsty_obj_id (Shape.leaf uid)
          ) shape_map classes
        in
        Tstr_class_type
          (List.map (fun cl ->
               (cl.Typeclass.clsty_ty_id,
                cl.Typeclass.clsty_id_loc,
                cl.Typeclass.clsty_info)) classes),
        List.flatten
          (map_rec
             (fun rs decl ->
                let open Typeclass in
                [Sig_class_type(decl.clsty_ty_id, decl.clsty_ty_decl, rs,
                                Exported);
                 Sig_type(decl.clsty_obj_id, decl.clsty_obj_abbr, rs, Exported);
                ])
             classes []),
        shape_map,
        new_env
    | Pstr_include sincl ->
        type_str_include ~loc env shape_map sincl sig_acc
    | Pstr_extension (ext, _attrs) ->
        raise (Error_forward (Builtin_attributes.error_of_extension ext))
    | Pstr_attribute x ->
        Builtin_attributes.parse_standard_implementation_attributes x;
        if Option.is_some toplevel
        || not (Warnings.is_active (Misplaced_attribute "")) then
          Builtin_attributes.mark_alert_used x;
        Tstr_attribute x, [], shape_map, env
    | Pstr_kind_abbrev _ ->
        Misc.fatal_error "kind_abbrev not supported!"
  in
  let toplevel_sig = Option.value toplevel ~default:[] in
  let rec type_struct env shape_map sstr str_acc sig_acc
            sig_acc_include_functor =
    match sstr with
    | [] ->
      (List.rev str_acc, List.rev sig_acc, shape_map, env)
    | pstr :: srem ->
        let previous_saved_types = Cmt_format.get_saved_types () in
        let desc, sg, shape_map, new_env =
          type_str_item env shape_map pstr sig_acc_include_functor
        in
        let str = { str_desc = desc; str_loc = pstr.pstr_loc; str_env = env } in
        Cmt_format.set_saved_types (Cmt_format.Partial_structure_item str
                                    :: previous_saved_types);
        type_struct new_env shape_map srem (str :: str_acc)
          (List.rev_append sg sig_acc)
          (List.rev_append sg sig_acc_include_functor)
  in
  let previous_saved_types = Cmt_format.get_saved_types () in
  let run () =
    let (items, sg, shape_map, final_env) =
      type_struct env Shape.Map.empty sstr [] [] toplevel_sig
    in
    let str = { str_items = items; str_type = sg; str_final_env = final_env } in
    Cmt_format.set_saved_types
      (Cmt_format.Partial_structure str :: previous_saved_types);
    str, sg, md_mode, names, Shape.str shape_map, final_env
  in
  if Option.is_some toplevel then run ()
  else Builtin_attributes.warning_scope [] run

(* The toplevel will print some types not present in the signature *)
let remove_mode_and_jkind_variables_for_toplevel str =
  match str.str_items with
  | [{ str_desc =
         ( Tstr_eval (exp, _, _)
         | Tstr_value (Nonrecursive,
                       [{vb_pat = {pat_desc=Tpat_any};
                         vb_expr = exp}])) }] ->
     (* These types are printed by the toplevel,
        even though they do not appear in sg *)
     Ctype.remove_mode_and_jkind_variables exp.exp_type
  | _ -> ()

let type_toplevel_phrase env sig_acc s =
  Env.reset_required_globals ();
  Env.reset_probes ();
  Typecore.reset_allocations ();
  let expected_mode = Value.(legacy |> disallow_left) in
  let (str, sg, mode, to_remove_from_sg, shape, env) =
    type_structure ~toplevel:(Some sig_acc) false None env ~expected_mode s in
  begin match Value.submode mode Value.legacy with
  | Ok () -> ()
  | Error e -> raise (Error (Location.none, env, (Legacy_module (Toplevel, e))))
  end;
  remove_mode_and_jkind_variables env sg;
  remove_mode_and_jkind_variables_for_toplevel str;
  Typecore.optimise_allocations ();
  (str, sg, to_remove_from_sg, shape, env)

let type_module_alias env smod =
  type_module_maybe_hold_locks ~alias:true ~hold_locks:true true false
    None env smod

let type_module = type_module true false None
let type_module_maybe_hold_locks = type_module_maybe_hold_locks true false None
let type_structure = type_structure false None

(* Normalize types in a signature *)

let rec normalize_modtype = function
    Mty_ident _
  | Mty_alias _ -> ()
  | Mty_signature sg -> normalize_signature sg
  | Mty_functor(_param, body) -> normalize_modtype body
  | Mty_strengthen (mty,_,_) -> normalize_modtype mty

and normalize_signature sg = List.iter normalize_signature_item sg

and normalize_signature_item = function
    Sig_value(_id, desc, _) -> Ctype.normalize_type desc.val_type
  | Sig_module(_id, _, md, _, _) -> normalize_modtype md.md_type
  | _ -> ()

(* Extract the module type of a module expression *)

let type_module_type_of env smod =
  let remove_aliases = has_remove_aliases_attribute smod.pmod_attributes in
  let tmty =
    match smod.pmod_desc with
    | Pmod_ident lid -> (* turn off strengthening in this case *)
        let path, md, (mode, locks) =
          Env.lookup_module ~loc:smod.pmod_loc lid.txt env
        in
          { mod_desc = Tmod_ident (path, lid);
            mod_type = md.md_type;
            mod_mode = mode, Some (locks, lid.txt, lid.loc);
            mod_env = env;
            mod_attributes = smod.pmod_attributes;
            mod_loc = smod.pmod_loc }
    | _ ->
        let me, _shape = type_module env smod in
        me
  in
  let mty = Mtype.scrape_for_type_of ~remove_aliases env tmty.mod_type in
  (* PR#5036: must not contain non-generalized type variables *)
  check_nongen_modtype env smod.pmod_loc mty;
  let zap_modality = Ctype.zap_modalities_to_floor_if_modes_enabled_at Stable in
  let mty =
    remove_modality_and_zero_alloc_variables_mty env ~zap_modality mty
  in
  tmty, mty

(* For Typecore *)

(* Graft a longident onto a path *)
let rec extend_path path =
  fun lid ->
    match lid with
    | Lident name -> Pdot(path, name)
    | Ldot(m, name) -> Pdot(extend_path path m, name)
    | Lapply _ -> assert false

(* Lookup a type's longident within a signature *)
let lookup_type_in_sig sg =
  let types, modules =
    List.fold_left
      (fun acc item ->
         match item with
         | Sig_type(id, _, _, _) ->
             let types, modules = acc in
             let types = String.Map.add (Ident.name id) id types in
             types, modules
         | Sig_module(id, _, _, _, _) ->
             let types, modules = acc in
             let modules = String.Map.add (Ident.name id) id modules in
             types, modules
         | _ -> acc)
      (String.Map.empty, String.Map.empty) sg
  in
  let rec module_path = function
    | Lident name -> Pident (String.Map.find name modules)
    | Ldot(m, name) -> Pdot(module_path m, name)
    | Lapply _ -> assert false
  in
  fun lid ->
    match lid with
    | Lident name -> Pident (String.Map.find name types)
    | Ldot(m, name) -> Pdot(module_path m, name)
    | Lapply _ -> assert false

let type_package env m p fl =
  (* Same as Pexp_letmodule *)
  (* remember original level *)
  let outer_scope = Ctype.get_current_level () in
  let modl, scope =
    Typetexp.TyVarEnv.with_local_scope begin fun () ->
      (* type the module and create a scope in a raised level *)
      Ctype.with_local_level begin fun () ->
        let modl, _mod_shape =
          type_module_maybe_hold_locks ~hold_locks:true env m
        in
        let scope = Ctype.create_scope () in
        modl, scope
      end
    end
  in
  Mtype.lower_nongen outer_scope modl.mod_type;
  let fl', env =
    match fl with
    | [] -> [], env
    | fl ->
      let type_path, env =
        match modl.mod_desc with
        | Tmod_ident (mp,_)
        | Tmod_constraint
            ({mod_desc=Tmod_ident (mp,_)}, _, Tmodtype_implicit, _) ->
          (* We special case these because interactions between
             strengthening of module types and packages can cause
             spurious escape errors. See examples from PR#6982 in the
             testsuite. This can be removed when such issues are
             fixed. *)
          extend_path mp, env
        | _ ->
          let sg = extract_sig_open env modl.mod_loc modl.mod_type in
          let sg, env = Env.enter_signature ~scope sg env in
          lookup_type_in_sig sg, env
      in
      let fl' =
        List.fold_right
          (fun (lid, _t) fl ->
             match type_path lid with
             | exception Not_found -> fl
             | path -> begin
                 match Env.find_type path env with
                 | exception Not_found -> fl
                 | decl ->
                     if decl.type_arity > 0 then begin
                       fl
                     end else begin
                       let t = Btype.newgenty (Tconstr (path,[],ref Mnil)) in
                       (lid, t) :: fl
                     end
               end)
          fl []
      in
      fl', env
  in
  let mty =
    if fl = [] then (Mty_ident p)
    else modtype_of_package env modl.mod_loc p fl'
  in
  List.iter
    (fun (n, ty) ->
      try Ctype.unify env ty
            (Ctype.newvar (Jkind.Builtin.any ~why:Dummy_jkind))
      with Ctype.Unify _ ->
        raise (Error(modl.mod_loc, env, Scoping_pack (n,ty))))
    fl';
  let _, mode = register_allocation () in
  let modl =
    wrap_constraint_package env true modl mty mode Tmodtype_implicit
  in
  modl, fl'

(* Fill in the forward declarations *)

let type_open_decl ?used_slot env od =
  let od, _, _, env =
    type_open_decl ?used_slot ?toplevel:None false (Signature_names.create ())
      env od
  in
  od, env

let type_open_descr ?used_slot env od =
  type_open_descr ?used_slot ?toplevel:None env od

let type_open_ ?used_slot ?toplevel ovf env loc lid =
  let path, _, newenv = type_open_ ?used_slot ?toplevel ovf env loc lid in
  path, newenv

let () =
  Typecore.type_module := type_module_alias;
  Typetexp.transl_modtype_longident := transl_modtype_longident;
  Typetexp.transl_modtype := transl_modtype;
  Typecore.type_open := type_open_ ?toplevel:None;
  Typetexp.type_open := type_open_ ?toplevel:None;
  Typecore.type_open_decl := type_open_decl;
  Typecore.type_package := type_package;
  Typeclass.type_open_descr := type_open_descr;
  type_module_type_of_fwd := type_module_type_of


(* Typecheck an implementation file *)

let gen_annot target annots =
  let annot = Unit_info.annot target in
  Cmt2annot.gen_annot (Some (Unit_info.Artifact.filename annot))
    ~sourcefile:(Unit_info.Artifact.source_file annot)
    ~use_summaries:false
    annots

let cms_register_toplevel_attributes ~sourcefile ~uid ~f ast =
  (* Cms files do not store the typetree. This can be a problem for Merlin as
    it uses attributes - which is why we manually construct a mapping from uid
    to attributes while typing.
    Generally `Pstr_attribute` and `Psig_attribute` are not needed by Merlin,
    except if it is the first element of the compilation unit structure or
    signature. *)
  let attr =
    match ast with
    | x :: _ -> f x
    | [] -> None
  in
  match attr with
  | None -> ()
  | Some attr ->
    Cms_format.register_toplevel_attributes uid
      ~loc:(Location.in_file sourcefile)
      ~attributes:[ attr ]

let cms_register_toplevel_struct_attributes ~sourcefile ~uid ast =
  cms_register_toplevel_attributes ~sourcefile ~uid ast
      ~f:(function
        | { pstr_desc = Pstr_attribute attr; _ }  -> Some attr
        | _ -> None)

let check_argument_type_if_given env sourcefile actual_sig arg_module_opt =
  match arg_module_opt with
  | None -> None
  | Some arg_param ->
      let arg_import =
        Compilation_unit.Name.of_parameter_name arg_param
      in
      (* CR lmaurer: This "look for known name in path" code is duplicated
         all over the place. *)
      let basename = arg_import |> Compilation_unit.Name.to_string in
      let arg_filename =
        try
          Load_path.find_normalized (basename ^ ".cmi")
        with Not_found ->
          raise(Error(Location.none, Env.empty,
                      Cannot_find_argument_type arg_param)) in
      let for_pack_prefix =
        (* Packed modules can't be arguments *)
        Compilation_unit.Prefix.empty
      in
      let arg_cmi =
        Unit_info.Artifact.from_filename ~for_pack_prefix arg_filename
      in
      let arg_module = Global_module.Name.of_parameter_name arg_param in
      let arg_sig = Env.read_signature arg_module arg_cmi in
      if not (Env.is_parameter_unit arg_module) then
        raise (Error (Location.none, env,
                      Argument_for_non_parameter (arg_module, arg_filename)));
      let coercion =
        Includemod.compunit_as_argument env sourcefile actual_sig
          arg_filename arg_sig
      in
      Some { ai_signature = arg_sig;
             ai_coercion_from_primary = coercion;
           }

let type_implementation target modulename initial_env ast =
  let sourcefile = Unit_info.source_file target in
  let error e =
    raise (Error (Location.in_file sourcefile, initial_env, e))
  in
  let save_cmt_and_cms target annots initial_env cmi shape =
      let decl_deps =
        (* This is cleared after saving the cmt so we have to save is before *)
        Cmt_format.get_declaration_dependencies ()
      in
    Cmt_format.save_cmt (Unit_info.cmt target) modulename
      annots initial_env cmi shape;
    Cms_format.save_cms (Unit_info.cms target) modulename
      annots initial_env shape decl_deps;
    gen_annot target annots;
  in
  Cmt_format.clear ();
  Misc.try_finally (fun () ->
      Typecore.reset_delayed_checks ();
      Typecore.reset_allocations ();
      Env.reset_required_globals ();
      Env.reset_probes ();
      if !Clflags.print_types then (* #7656 *)
        ignore @@ Warnings.parse_options false "-32-34-37-38-60";
      if !Clflags.as_parameter then
        error Cannot_compile_implementation_as_parameter;
      let expected_mode = Env.mode_unit |> Value.disallow_left in
      let (str, sg, mode, names, shape, finalenv) =
        Profile.record_call "infer" (fun () ->
          type_structure initial_env ~expected_mode ast) in
      begin match Value.submode mode Env.mode_unit with
      | Ok () -> ()
      | Error e -> error (Legacy_module (Compilation_unit, e))
      end;
      let uid = Uid.of_compilation_unit_id modulename in
      let shape = Shape.set_uid_if_none shape uid in
      if !Clflags.binary_annotations_cms then
        cms_register_toplevel_struct_attributes ~sourcefile ~uid ast;
      let simple_sg = Signature_names.simplify finalenv names sg in
      if !Clflags.print_types then begin
        remove_mode_and_jkind_variables finalenv sg;
        let zap_modality =
          Ctype.zap_modalities_to_floor_if_modes_enabled_at Alpha
        in
        let simple_sg =
          (* Printing [.mli] from [.ml], we zap to identity modality for legacy
             compatibility. *)
          remove_modality_and_zero_alloc_variables_sg finalenv ~zap_modality
            simple_sg
        in
        Typecore.force_delayed_checks ();
        Typecore.optimise_allocations ();
        let shape = Shape_reduce.local_reduce Env.empty shape in
        Printtyp.wrap_printing_env ~error:false initial_env
          (fun () -> fprintf std_formatter "%a@."
              (Printtyp.printed_signature @@ Unit_info.source_file target)
              simple_sg
          );
        gen_annot target (Cmt_format.Implementation str);
        { structure = str;
          coercion = Tcoerce_none;
          shape;
          signature = simple_sg;
          argument_interface = None;
        } (* result is ignored by Compile.implementation *)
      end else begin
        let arg_type =
          !Clflags.as_argument_for
          |> Option.map Global_module.Parameter_name.of_string
        in
        let cu_name = Compilation_unit.name modulename in
        let basename = cu_name |> Compilation_unit.Name.to_string in
        let source_intf = Unit_info.mli_from_source target in
        if !Clflags.cmi_file <> None
        || Sys.file_exists source_intf then begin
          let for_pack_prefix = Compilation_unit.for_pack_prefix modulename in
          let compiled_intf_file =
            match !Clflags.cmi_file with
            | Some cmi_file ->
              Unit_info.Artifact.from_filename ~for_pack_prefix cmi_file
            | None ->
              let cmi_file =
                try
                  Load_path.find_normalized (basename ^ ".cmi")
                with Not_found ->
                  raise(Error(Location.in_file sourcefile, Env.empty,
                        Interface_not_compiled source_intf))
              in
              Unit_info.Artifact.from_filename ~for_pack_prefix cmi_file
          in
          (* We use pre-5.2 behaviour as regards which interface-related file
             is reported in error messages. *)
          let compiled_intf_file_name =
            Unit_info.Artifact.filename compiled_intf_file
          in
          let global_name =
            Compilation_unit.to_global_name_without_prefix modulename
          in
          let dclsig = Env.read_signature global_name compiled_intf_file in
          if Env.is_parameter_unit global_name then
            error (Cannot_implement_parameter (cu_name, source_intf));
          let arg_type_from_cmi = Env.implemented_parameter global_name in
          if not (Option.equal Global_module.Parameter_name.equal
                    arg_type arg_type_from_cmi) then
            error (Inconsistent_argument_types
                     { new_arg_type = arg_type; old_source_file = source_intf;
                       old_arg_type = arg_type_from_cmi });
          let coercion, shape =
            Profile.record_call "check_sig" (fun () ->
              Includemod.compunit initial_env ~mark:true
                sourcefile sg compiled_intf_file_name dclsig shape)
          in
          (* Check the _mli_ against the argument type, since the mli determines
             the visible type of the module and that's what needs to conform to
             the argument type.

             This is somewhat redundant with the checking that was done when
             compiling the .mli. However, this isn't just a boolean check - we
             need to get the coercion out. An alternative would be to store the
             coercion in the .cmi if we can sort out the dependency issues
             ([Tcoerce_primitive] is a pain in particular). *)
          let argument_interface =
            check_argument_type_if_given initial_env source_intf dclsig arg_type
          in
          Typecore.force_delayed_checks ();
          Typecore.optimise_allocations ();
          (* It is important to run these checks after the inclusion test above,
             so that value declarations which are not used internally but
             exported are not reported as being unused. *)
          Profile.record_call "save_cmt" (fun () ->
            let shape = Shape_reduce.local_reduce Env.empty shape in
            let annots = Cmt_format.Implementation str in
            save_cmt_and_cms target annots initial_env None (Some shape));
          { structure = str;
            coercion;
            shape;
            signature = dclsig;
            argument_interface;
          }
        end else begin
          Location.prerr_warning
            (Location.in_file (Unit_info.source_file target))
            Warnings.Missing_mli;
          let coercion, shape =
            Profile.record_call "check_sig" (fun () ->
              Includemod.compunit initial_env ~mark:true
                sourcefile sg "(inferred signature)" simple_sg shape)
          in
          check_nongen_signature finalenv simple_sg;
          let zap_modality =
            (* Generating [cmi] without [mli]. This [cmi] could be on the RHS of
               inclusion check, so we zap to identity if mode extension is
               disabled. Otherwise, zapping to floor (strongest) is better. *)
            Ctype.zap_modalities_to_floor_if_modes_enabled_at Stable
          in
          let simple_sg =
            remove_modality_and_zero_alloc_variables_sg finalenv ~zap_modality
              simple_sg
          in
          normalize_signature simple_sg;
          let argument_interface =
            check_argument_type_if_given initial_env sourcefile simple_sg arg_type
          in
          Typecore.force_delayed_checks ();
          Typecore.optimise_allocations ();
          (* See comment above. Here the target signature contains all
             the values being exported. We can still capture unused
             declarations like "let x = true;; let x = 1;;", because in this
             case, the inferred signature contains only the last declaration. *)
          let shape = Shape_reduce.local_reduce Env.empty shape in
          let alerts = Builtin_attributes.alerts_of_str ~mark:true ast in
          if not !Clflags.dont_write_files then begin
            let name = Compilation_unit.name modulename in
            let kind =
              Cmi_format.Normal { cmi_impl = modulename; cmi_arg_for = arg_type }
            in
            let cmi =
              Profile.record_call "save_cmi" (fun () ->
                Env.save_signature ~alerts simple_sg name kind
                  (Unit_info.cmi target))
            in
            Profile.record_call "save_cmt" (fun () ->
              let annots = Cmt_format.Implementation str in
              save_cmt_and_cms target annots initial_env (Some cmi) (Some shape));
          end;
          { structure = str;
            coercion;
            shape;
            signature = simple_sg;
            argument_interface
          }
        end
      end
    )
    ~exceptionally:(fun () ->
        Profile.record_call "save_cmt" (fun () ->
          let annots =
            Cmt_format.Partial_implementation
              (Array.of_list (Cmt_format.get_saved_types ()))
          in
          save_cmt_and_cms target annots initial_env None None)
      )

let save_signature target modname tsg initial_env cmi =
  let decl_deps =
    (* This is cleared after saving the cmt so we have to save is before *)
    Cmt_format.get_declaration_dependencies ()
  in
  Cmt_format.save_cmt (Unit_info.cmti target) modname
    (Cmt_format.Interface tsg) initial_env (Some cmi) None;
  Cms_format.save_cms  (Unit_info.cmsi target) modname
    (Cmt_format.Interface tsg) initial_env None decl_deps

let cms_register_toplevel_signature_attributes ~sourcefile ~uid ast =
  cms_register_toplevel_attributes ~sourcefile ~uid ast.psg_items
    ~f:(function
        | { psig_desc = Psig_attribute attr; _ } -> Some attr
        | _ -> None)

let type_interface ~sourcefile modulename env ast =
  let error e =
    raise (Error (Location.none, Env.empty, e))
  in
  if !Clflags.as_parameter && Compilation_unit.is_packed modulename then begin
    error Cannot_pack_parameter
  end;
  if !Clflags.as_parameter && !Clflags.parameters <> [] then begin
    error Compiling_as_parameterised_parameter
  end;
  if !Clflags.binary_annotations_cms then begin
    let uid = Shape.Uid.of_compilation_unit_id modulename in
    cms_register_toplevel_signature_attributes ~uid ~sourcefile ast
  end;
  let sg = transl_signature env ast in
  let arg_type =
    !Clflags.as_argument_for
    |> Option.map Global_module.Parameter_name.of_string
  in
  ignore (check_argument_type_if_given env sourcefile sg.sig_type arg_type
          : Typedtree.argument_interface option);
  sg

(* "Packaging" of several compilation units into one unit
   having them as sub-modules.  *)

let package_signatures units =
  let units_with_ids =
    List.map
      (fun (name, sg) ->
        let name = name |> Compilation_unit.Name.to_string in
        let oldid = Ident.create_persistent name in
        let newid = Ident.create_local name in
        (oldid, newid, sg))
      units
  in
  let subst =
    List.fold_left
      (fun acc (oldid, newid, _) ->
        Subst.add_module oldid (Pident newid) acc)
      Subst.identity units_with_ids
  in
  List.map
    (fun (_, newid, sg) ->
      (* This signature won't be used for anything, it'll just be saved in a cmi
         and cmt. *)
      let sg = Subst.signature Make_local subst sg in
      let md =
        { md_type=Mty_signature sg;
          md_modalities=Modality.Value.id;
          md_attributes=[];
          md_loc=Location.none;
          md_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
        }
      in
      Sig_module(newid, Mp_present, md, Trec_not, Exported))
    units_with_ids

let package_units initial_env objfiles target_cmi modulename =
  (* Read the signatures of the units *)
  let units =
    List.map
      (fun f ->
         let for_pack_prefix = Compilation_unit.to_prefix modulename in
         let artifact = Unit_info.Artifact.from_filename ~for_pack_prefix f in
         let modname = Unit_info.Artifact.modname artifact in
         let global_name =
           Compilation_unit.to_global_name_without_prefix modname
         in
         let sg =
           Env.read_signature global_name (Unit_info.companion_cmi artifact)
         in
         if Unit_info.is_cmi artifact &&
            not(Mtype.no_code_needed_sig (Lazy.force Env.initial) sg)
         then raise(Error(Location.none, Env.empty,
                          Implementation_is_required f));
         Compilation_unit.name modname, sg)
      objfiles in
  (* Compute signature of packaged unit *)
  Ident.reinit();
  let sg = package_signatures units in
  (* Compute the shape of the package *)
  let pack_uid = Uid.of_compilation_unit_id modulename in
  let shape =
    List.fold_left (fun map (name, _sg) ->
      let name = Compilation_unit.Name.to_string name in
      let id = Ident.create_persistent name in
      Shape.Map.add_module map id (Shape.for_persistent_unit name)
    ) Shape.Map.empty units
    |> Shape.str ~uid:pack_uid
  in
  (* See if explicit interface is provided *)
  let mli = Unit_info.mli_from_artifact target_cmi in
  if Sys.file_exists mli then begin
    if not (Sys.file_exists @@ Unit_info.Artifact.filename target_cmi) then
    begin
      raise(Error(Location.in_file mli, Env.empty,
                  Interface_not_compiled mli))
    end;
    let name = Compilation_unit.to_global_name_without_prefix modulename in
    let dclsig = Env.read_signature name target_cmi in
    let cc, _shape =
      Includemod.compunit initial_env ~mark:true
        "(obtained by packing)" sg mli dclsig shape
    in
    let decl_deps =
      (* This is cleared after saving the cmt so we have to save is before *)
      Cmt_format.get_declaration_dependencies ()
    in
    Cmt_format.save_cmt  (Unit_info.companion_cmt target_cmi) modulename
      (Cmt_format.Packed (sg, objfiles)) initial_env  None (Some shape);
    Cms_format.save_cms  (Unit_info.companion_cms target_cmi) modulename
      (Cmt_format.Packed (sg, objfiles)) initial_env (Some shape) decl_deps;
    cc
  end else begin
    (* Determine imports *)
    let unit_names = List.map fst units in
    let imports =
      List.filter (fun import ->
          let name = Import_info.name import in
          not (List.mem name unit_names))
        (Env.imports()) in
    (* Write packaged signature *)
    if not !Clflags.dont_write_files then begin
      let cmi_arg_for =
        (* Packs aren't supported as arguments *)
        None
      in
      let name = Compilation_unit.name modulename in
      let kind = Cmi_format.Normal { cmi_impl = modulename; cmi_arg_for } in
      let cmi =
        Env.save_signature_with_imports ~alerts:Misc.Stdlib.String.Map.empty
          sg name kind target_cmi (Array.of_list imports)
      in
      let sign = Subst.Lazy.force_signature cmi.Cmi_format.cmi_sign in
      let decl_deps =
        (* This is cleared after saving the cmt so we have to save is before *)
        Cmt_format.get_declaration_dependencies ()
      in
      Cmt_format.save_cmt (Unit_info.companion_cmt target_cmi)  modulename
        (Cmt_format.Packed (sign, objfiles)) initial_env (Some cmi) (Some shape);
      Cms_format.save_cms (Unit_info.companion_cms target_cmi)  modulename
        (Cmt_format.Packed (sign, objfiles)) initial_env (Some shape) decl_deps;
    end;
    Tcoerce_none
  end


(* Error report *)


open Printtyp

let report_error ~loc _env = function
    Cannot_apply mty ->
      Location.errorf ~loc
        "@[This module is not a functor; it has type@ %a@]"
        (Style.as_inline_code modtype) mty
  | Not_included errs ->
      let main = Includemod_errorprinter.err_msgs errs in
      Location.errorf ~loc "@[<v>Signature mismatch:@ %t@]" main
  | Not_included_functor errs ->
      let main = Includemod_errorprinter.err_msgs errs in
      Location.errorf ~loc
        "@[<v>Signature mismatch in included functor's parameter:@ %t@]" main
  | Cannot_eliminate_dependency (dep_type, mty) ->
      let hint =
        match dep_type with
        | Functor_applied -> "Please bind the argument to a module identifier"
        | Functor_included -> "This functor can't be included directly; please \
                               apply it to an explicit argument"
      in
      Location.errorf ~loc
        "@[This functor has type@ %a@ \
           The parameter cannot be eliminated in the result type.@ \
           %s.@]"
        (Style.as_inline_code modtype) mty
        hint
  | Signature_expected ->
      Location.errorf ~loc "This module type is not a signature"
  | Structure_expected mty ->
      Location.errorf ~loc
        "@[This module is not a structure; it has type@ %a"
        (Style.as_inline_code modtype) mty
  | Functor_expected mty ->
      Location.errorf ~loc
        "@[This module is not a functor; it has type@ %a"
        (Style.as_inline_code modtype) mty
  | Signature_parameter_expected mty ->
      Location.errorf ~loc
        "@[The type of this functor is:@ %a. @ Its parameter is not a signature."
        (Style.as_inline_code modtype) mty
  | Signature_result_expected mty ->
      Location.errorf ~loc
        "@[The type of this functor's result is not includable; it is@ %a"
        (Style.as_inline_code modtype) mty
  | Recursive_include_functor ->
      Location.errorf ~loc
        "@[Including a functor is not supported in recursive module signatures @]"
  | With_no_component lid ->
      Location.errorf ~loc
        "@[The signature constrained by %a has no component named %a@]"
        Style.inline_code "with"
        (Style.as_inline_code longident) lid
  | With_mismatch(lid, explanation) ->
      let main = Includemod_errorprinter.err_msgs explanation in
      Location.errorf ~loc
        "@[<v>\
           @[In this %a constraint, the new definition of %a@ \
             does not match its original definition@ \
             in the constrained signature:@]@ \
         %t@]"
        Style.inline_code "with"
        (Style.as_inline_code longident) lid main
  | With_makes_applicative_functor_ill_typed(lid, path, explanation) ->
      let main = Includemod_errorprinter.err_msgs explanation in
      Location.errorf ~loc
        "@[<v>\
           @[This %a constraint on %a makes the applicative functor @ \
             type %a ill-typed in the constrained signature:@]@ \
         %t@]"
        Style.inline_code "with"
        (Style.as_inline_code longident) lid
        Style.inline_code (Path.name path)
        main
  | With_changes_module_alias(lid, id, path) ->
      Location.errorf ~loc
        "@[<v>\
           @[This %a constraint on %a changes %a, which is aliased @ \
             in the constrained signature (as %a)@].@]"
        Style.inline_code "with"
        (Style.as_inline_code longident) lid
        Style.inline_code (Path.name path)
        Style.inline_code (Ident.name id)
  | With_cannot_remove_constrained_type ->
      Location.errorf ~loc
        "@[<v>Destructive substitutions are not supported for constrained @ \
              types (other than when replacing a type constructor with @ \
              a type constructor with the same arguments).@]"
  | With_cannot_remove_packed_modtype (p,mty) ->
      let[@manual.ref "ss:module-type-substitution"] manual_ref =
        [ 12; 7; 3 ]
      in
      let pp_constraint ppf (p,mty) =
        Format.fprintf ppf "%s := %a" (Path.name p) Printtyp.modtype mty
      in
      Location.errorf ~loc
        "This %a constraint@ %a@ makes a packed module ill-formed.@ %a"
        Style.inline_code "with"
        (Style.as_inline_code pp_constraint) (p,mty)
        Misc.print_see_manual manual_ref
  | With_package_manifest (lid, ty) ->
      Location.errorf ~loc
        "In the constrained signature, type %a is defined to be %a.@ \
         Package %a constraints may only be used on abstract types."
        (Style.as_inline_code longident) lid
        (Style.as_inline_code Printtyp.type_expr) ty
        Style.inline_code "with"
  | Repeated_name(kind, name) ->
      Location.errorf ~loc
        "@[Multiple definition of the %s name %a.@ \
         Names must be unique in a given structure or signature.@]"
        (Sig_component_kind.to_string kind) Style.inline_code name
  | Non_generalizable { vars; expression } ->
      let[@manual.ref "ss:valuerestriction"] manual_ref = [ 6; 1; 2 ] in
      prepare_for_printing vars;
      add_type_to_preparation expression;
      Location.errorf ~loc
        "@[The type of this expression,@ %a,@ \
         contains the non-generalizable type variable(s): %a.@ %a@]"
        (Style.as_inline_code prepared_type_scheme) expression
        (pp_print_list ~pp_sep:(fun f () -> fprintf f ",@ ")
           (Style.as_inline_code prepared_type_scheme)) vars
        Misc.print_see_manual manual_ref
  | Non_generalizable_module { vars; mty; item } ->
      let[@manual.ref "ss:valuerestriction"] manual_ref = [ 6; 1; 2 ] in
      prepare_for_printing vars;
      add_type_to_preparation item.val_type;
      let sub =
        [ Location.msg ~loc:item.val_loc
            "The type of this value,@ %a,@ \
             contains the non-generalizable type variable(s) %a."
            (Style.as_inline_code prepared_type_scheme)
            item.val_type
            (pp_print_list ~pp_sep:(fun f () -> fprintf f ",@ ")
               @@ Style.as_inline_code prepared_type_scheme) vars
        ]
      in
      Location.errorf ~loc ~sub
        "@[The type of this module,@ %a,@ \
         contains non-generalizable type variable(s).@ %a@]"
        modtype mty
        Misc.print_see_manual manual_ref
  | Implementation_is_required intf_name ->
      Location.errorf ~loc
        "@[The interface %a@ declares values, not just types.@ \
           An implementation must be provided.@]"
        Location.print_filename intf_name
  | Interface_not_compiled intf_name ->
      Location.errorf ~loc
        "@[Could not find the .cmi file for interface@ %a.@]"
        Location.print_filename intf_name
  | Not_allowed_in_functor_body ->
      Location.errorf ~loc
        "@[This expression creates fresh types.@ %s@]"
        "It is not allowed inside applicative functors."
  | Not_includable_in_functor_body ->
      Location.errorf ~loc
        "@[This functor creates fresh types when applied.@ %s@]"
        "Including it is not allowed inside applicative functors."
  | Not_a_packed_module ty ->
      Location.errorf ~loc
        "This expression is not a packed module. It has type@ %a"
        (Style.as_inline_code type_expr) ty
  | Incomplete_packed_module ty ->
      Location.errorf ~loc
        "The type of this packed module contains variables:@ %a"
        (Style.as_inline_code type_expr) ty
  | Scoping_pack (lid, ty) ->
      Location.errorf ~loc
        "The type %a in this module cannot be exported.@ \
         Its type contains local dependencies:@ %a"
        (Style.as_inline_code longident) lid
        (Style.as_inline_code type_expr) ty
  | Recursive_module_require_explicit_type ->
      Location.errorf ~loc "Recursive modules require an explicit module type."
  | Apply_generative ->
      Location.errorf ~loc
        "This is a generative functor. It can only be applied to %a"
        Style.inline_code "()"
  | Cannot_scrape_alias p ->
      Location.errorf ~loc
        "This is an alias for module %a, which is missing"
        (Style.as_inline_code path) p
  | Cannot_scrape_package_type p ->
      Location.errorf ~loc
        "The type of this packed module refers to %a, which is missing"
        (Style.as_inline_code path) p
  | Badly_formed_signature (context, err) ->
      Location.errorf ~loc "@[In %s:@ %a@]" context Typedecl.report_error err
  | Cannot_hide_id Illegal_shadowing
      { shadowed_item_kind; shadowed_item_id; shadowed_item_loc;
        shadower_id; user_id; user_kind; user_loc } ->
      let shadowed =
        Printtyp.namespaced_ident shadowed_item_kind shadowed_item_id
      in
      let shadower =
        Printtyp.namespaced_ident shadowed_item_kind shadower_id
      in
      let shadowed_item_kind= Sig_component_kind.to_string shadowed_item_kind in
      let shadowed_msg =
        Location.msg ~loc:shadowed_item_loc
          "@[%s %a came from this include.@]"
          (String.capitalize_ascii shadowed_item_kind)
          Style.inline_code shadowed
      in
      let user_msg =
        Location.msg ~loc:user_loc
        "@[The %s %a has no valid type@ if %a is shadowed.@]"
        (Sig_component_kind.to_string user_kind)
         Style.inline_code (Ident.name user_id)
         Style.inline_code shadowed
      in
      Location.errorf ~loc ~sub:[shadowed_msg; user_msg]
        "Illegal shadowing of included %s %a@ by %a."
        shadowed_item_kind
        Style.inline_code shadowed
        Style.inline_code shadower
  | Cannot_hide_id Appears_in_signature
      { opened_item_kind; opened_item_id; user_id; user_kind; user_loc } ->
      let opened_item_kind= Sig_component_kind.to_string opened_item_kind in
      let opened_id = Ident.name opened_item_id in
      let user_msg =
        Location.msg ~loc:user_loc
          "@[The %s %a has no valid type@ if %a is hidden.@]"
          (Sig_component_kind.to_string user_kind)
          Style.inline_code (Ident.name user_id)
          Style.inline_code opened_id
      in
      Location.errorf ~loc ~sub:[user_msg]
        "The %s %a introduced by this open appears in the signature."
        opened_item_kind
        Style.inline_code opened_id
  | Invalid_type_subst_rhs ->
      Location.errorf ~loc "Only type synonyms are allowed on the right of %a"
        Style.inline_code  ":="
  | Non_packable_local_modtype_subst p ->
      let[@manual.ref "ss:module-type-substitution"] manual_ref =
        [ 12; 7; 3 ]
      in
      Location.errorf ~loc
        "The module type@ %a@ is not a valid type for a packed module:@ \
         it is defined as a local substitution (temporary name)@ \
         for an anonymous module type.@ %a"
        Style.inline_code (Path.name p)
        Misc.print_see_manual manual_ref
  | Toplevel_nonvalue (id, sort) ->
      Location.errorf ~loc
        "@[Types of top-level module bindings must have layout %a, but@ \
         the type of %a has layout@ %a.@]"
        Style.inline_code "value"
        Style.inline_code id
        (Style.as_inline_code Jkind.Sort.format) sort
  | Toplevel_unnamed_nonvalue sort ->
      Location.errorf ~loc
        "@[Types of unnamed expressions must have layout value when using@ \
           the toplevel, but this expression has layout@ %a.@]"
        (Style.as_inline_code Jkind.Sort.format) sort
 | Strengthening_mismatch(lid, explanation) ->
      let main = Includemod_errorprinter.err_msgs explanation in
      Location.errorf ~loc
        "@[<v>\
           @[In this strengthened module type, the type of %a@ \
             does not match the underlying type@]@ \
           %t@]"
        (Style.as_inline_code longident) lid
        main
  | Cannot_pack_parameter ->
      Location.errorf ~loc
        "Cannot compile a parameter with -for-pack."
  | Compiling_as_parameterised_parameter ->
      Location.errorf ~loc
        "@[Cannot combine -as-parameter with -parameter: parameters cannot@ \
         be parameterised.@]"
  | Cannot_compile_implementation_as_parameter ->
      Location.errorf ~loc
        "Cannot compile an implementation with -as-parameter."
  | Cannot_implement_parameter(modname, _filename) ->
      Location.errorf ~loc
        "@[The interface for %a@ was compiled with -as-parameter.@ \
         It cannot be implemented directly.@]"
        (Style.as_inline_code Compilation_unit.Name.print) modname
  | Argument_for_non_parameter(param, path) ->
      Location.errorf ~loc
        "Interface %a@ found for module@ %a@ is not flagged as a parameter.@ \
         It cannot be the parameter type for this argument module."
        Style.inline_code path
        (Style.as_inline_code Global_module.Name.print) param
  | Inconsistent_argument_types
        { new_arg_type; old_source_file; old_arg_type } ->
      let pp_arg_type ppf arg_type =
        match arg_type with
        | None -> Format.fprintf ppf "without -as-argument-for"
        | Some arg_type ->
            Format.fprintf ppf "with -as-argument-for %a"
              Global_module.Parameter_name.print arg_type
      in
      Location.errorf ~loc
        "Inconsistent usage of -as-argument-for. Interface@ %s@ was compiled \
         %a@ but this module is being compiled@ %a."
        old_source_file
        pp_arg_type old_arg_type
        pp_arg_type new_arg_type
  | Cannot_find_argument_type arg_type ->
      Location.errorf ~loc
        "Parameter module %a@ specified by -as-argument-for cannot be found."
        (Style.as_inline_code Global_module.Parameter_name.print) arg_type
  | Duplicate_parameter_name name ->
      Location.errorf ~loc
        "This instance has multiple arguments with the name %a."
        (Style.as_inline_code Global_module.Parameter_name.print) name
  | Item_weaker_than_structure (Error (ax, {left; right})) ->
      let d =
        match ax with
        | Comonadic Areality -> Format.dprintf "a structure"
        | _ ->
            Format.dprintf "a %a structure"
              (Style.as_inline_code (Mode.Value.Const.print_axis ax)) right
      in
      Location.errorf ~loc
        "This is %a, but expected to be %a because it is inside %t."
        (Style.as_inline_code (Mode.Value.Const.print_axis ax)) left
        (Style.as_inline_code (Mode.Value.Const.print_axis ax)) right
        d
  | Submode_failed (Error (ax, {left; right})) ->
      Location.errorf ~loc
        "This is %a, but expected to be %a."
        (Style.as_inline_code (Mode.Value.Const.print_axis ax)) left
        (Style.as_inline_code (Mode.Value.Const.print_axis ax)) right
  | Unsupported_modal_module e ->
      Location.errorf ~loc
        "Mode annotations on %a are not supported yet."
        print_unsupported_modal_module e
  | Legacy_module (reason, Error (ax, {left; right})) ->
      Location.errorf ~loc
        "This is %a, but expected to be %a because it is a %a."
        (Style.as_inline_code (Mode.Value.Const.print_axis ax)) left
        (Style.as_inline_code (Mode.Value.Const.print_axis ax)) right
        print_legacy_module reason

let report_error env ~loc err =
  Printtyp.wrap_printing_env_error env
    (fun () -> report_error env ~loc err)

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, env, err) ->
        Some (report_error ~loc env err)
      | Error_forward err ->
        Some err
      | _ ->
        None
    )

let reset ~preserve_persistent_env =
  Env.reset_cache ~preserve_persistent_env;
  Envaux.reset_cache ~preserve_persistent_env;
  Typetexp.TyVarEnv.reset ()
