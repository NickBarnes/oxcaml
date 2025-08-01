(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Compilation of generic recursive definitions *)

(** The surface language allows a wide range of recursive definitions, but
    Lambda only allows syntactic functions in recursive bindings.
    This file implements the translation from generic definitions to Lambda.

    The first step occurs during typechecking, in [Value_rec_check]:
    [Dynamic] bindings need to be compiled as normal let bindings. This file
    mostly deals with the [Static] bindings.

    The three phases in this module are the following:

    - Sizing: we first classify the definitions by their size, which determines
      the compilation strategy for each binding.

    - Function lifting: we then apply a transformation from general function
      definitions to syntactic functions accepted by [Lletrec].
      Examples:
      {[
        let rec f x = f x (* Syntactic *)
        let rec f = fun x -> f x (* Syntactic *)
        let rec f = let g x = f x in g (* Not syntactic *)
        let rec f = let a = ... in (fun x -> f x) (* Not syntactic *)
      ]}

    - Compilation: we finally combine all of this to produce a Lambda term
      for the recursive bindings.
*)

open Lambda

(** {1. Sizing} *)

(* Simple blocks *)
type block_size =
  | Regular_block of int
  | Float_record of int
  | Mixed_record of Lambda.mixed_block_shape

type size =
  | Unreachable
  (** Non-returning expressions, like [raise exn].
      In [Value_rec_check], they would be classified as [Dynamic],
      but some of those appear during translation to Lambda.
      For example, in [let rec f = let [| x |] = ... in fun y -> x + y]
      the inner let binding gets translated to code that raises
      [Match_failure] for non-matching branches.
      Tracking [Unreachable] explicitly allows us to recover the size
      of the only non-raising branch. *)
  | Constant
  (** Constant values.
      Can be either an integer-like constant ([0], ['a'], [None],
      the empty list or the unit constructor), or a structured constant
      (["hello"], [Some 1], ...).

      Integer constants cannot be pre-allocated, so need their own
      classification and compilation scheme (See {!Compilation} below).
      Structured constants could fit into the [Block] category, but we
      choose to reuse the [constant] classification to avoid sorting
      through the [Lconst] definitions.
      It also generates slightly better code. *)
  | Function of Lambda.lfunction
  (** Function definitions.
      This includes more than just obvious, syntactic function definitions;
      see {!Function Lifting} for details.
      Note that to be able to eta-expand variables bound to functions,
      we need a bunch of metadata such as the arity so we store the actual
      definition to be sure we can recover all we need. *)
  | Block of block_size
  (** Allocated values of a fixed size.
      This corresponds to expressions ending in a single obvious allocation,
      but also some more complex expressions where the block is bound to
      an intermediate variable before being returned.
  *)

type binding_size = (lambda_with_env, size) Lazy_backtrack.t
and lambda_with_env = {
  lambda : lambda;
  env : binding_size Ident.Map.t;
}

let dynamic_size lam =
  Misc.fatal_errorf "letrec: No size found for Static binding:@ %a"
    Printlambda.lambda lam

(* [join_sizes] is used to compute the size of an expression with multiple
   branches. Such expressions are normally classified as [Dynamic] by
   [Value_rec_check], so the default behaviour is a compile-time failure.
   However, for partial pattern-matching (typically in let bindings)
   the compiler will later add a branch for the failing cases, and this
   is handled here with the [Unreachable] case.
   Note that the current compilation scheme would work if we allowed the
   [Constant] and [Block] cases to be joined, but [Function] needs to be
   a single function. *)
let join_sizes lam size1 size2 =
  match size1, size2 with
  | Unreachable, size | size, Unreachable -> size
  | _, _ -> dynamic_size lam

let compute_static_size lam =
  let rec compute_expression_size env lam =
    match lam with
    | Lvar v ->
      begin match Ident.Map.find_opt v env with
      | None ->
        dynamic_size lam
      | Some binding_size ->
        Lazy_backtrack.force
          (fun { lambda; env } -> compute_expression_size env lambda)
          binding_size
      end
    | Lmutvar _ -> dynamic_size lam
    | Lconst _ -> Constant
    | Lapply _ -> dynamic_size lam
    | Lfunction lfun -> Function lfun
    | Llet (_, _, id, _, def, body) ->
      let env =
        Ident.Map.add id (Lazy_backtrack.create { lambda = def; env }) env
      in
      compute_expression_size env body
    | Lmutlet(_, _, _, _, body) ->
      compute_expression_size env body
    | Lletrec (bindings, body) ->
      let env =
        List.fold_left (fun env_acc { id; def } ->
            Ident.Map.add id (Lazy_backtrack.create_forced (Function def)) env_acc)
          env bindings
      in
      compute_expression_size env body
    | Lprim (p, args, _) ->
      size_of_primitive env p args
    | Lswitch (_, sw, _, _) ->
      let fail_case =
        match sw.sw_failaction with
        | None -> []
        | Some fail -> [0 (* ignored *), fail]
      in
      compute_and_join_sizes_switch env [sw.sw_consts; sw.sw_blocks; fail_case]
    | Lstringswitch (_, cases, fail, _, _) ->
      let fail_case =
        match fail with
        | None -> []
        | Some fail -> ["" (* ignored *), fail]
      in
      compute_and_join_sizes_switch env [cases; fail_case]
    | Lstaticraise _ -> Unreachable
    | Lstaticcatch (body, _, handler, _, _)
    | Ltrywith (body, _, _, handler, _) ->
      compute_and_join_sizes env [body; handler]
    | Lifthenelse (_cond, ifso, ifnot, _) ->
      compute_and_join_sizes env [ifso; ifnot]
    | Lsequence (_, e) ->
      compute_expression_size env e
    | Lwhile _
    | Lfor _
    | Lassign _ -> Constant
    | Lsend _ -> dynamic_size lam
    | Levent (e, _) ->
      compute_expression_size env e
    | Lifused _ -> Constant
    | Lregion (e, _) ->
      compute_expression_size env e
    | Lexclave _ ->
      (* Lexclave should only occur in tail position of a function.
         Since we only compute sizes for let-bound definitions, we should never
         reach this case.
         This justifies using [assert false] instead of [dynamic_size lam],
         the latter meaning that [Value_rec_check] should have forbidden that case.
      *)
      assert false
  and compute_and_join_sizes env branches =
    List.fold_left (fun size branch ->
        join_sizes branch size (compute_expression_size env branch))
      Unreachable branches
  and compute_and_join_sizes_switch :
    type a. binding_size Ident.Map.t -> (a * lambda) list list -> size =
    fun env all_cases ->
      List.fold_left (fun size cases ->
          List.fold_left (fun size (_key, action) ->
              join_sizes action size (compute_expression_size env action))
            size cases)
        Unreachable all_cases
  and size_of_primitive env p args =
    match p with
    | Pignore
    | Psetfield _
    | Psetfield_computed _
    | Psetfloatfield _
    | Psetmixedfield _
    | Poffsetint _
    | Poffsetref _
    | Pbytessetu
    | Pbytessets
    | Parraysetu _
    | Parraysets _
    | Pbigarrayset _
    | Pbytes_set_16 _
    | Pbytes_set_32 _
    | Pbytes_set_f32 _
    | Pbytes_set_64 _
    | Pbigstring_set_16 _
    | Pbigstring_set_32 _
    | Pbigstring_set_f32 _
    | Pbigstring_set_64 _
    | Ppoll
    | Patomic_add_field
    | Patomic_sub_field
    | Patomic_land_field
    | Patomic_lor_field
    | Patomic_lxor_field
    | Pcpu_relax ->
        (* Unit-returning primitives. Most of these are only generated from
           external declarations and not special-cased by [Value_rec_check],
           but it doesn't hurt to be consistent. *)
        Constant

    | Pduprecord (repres, size) ->
        begin match repres with
        | Record_boxed _
        | Record_inlined (_, Constructor_uniform_value,
                          (Variant_boxed _ | Variant_extensible)) ->
            Block (Regular_block size)
        | Record_float ->
            Block (Float_record size)
        | Record_inlined (_, Constructor_mixed shape,
                          (Variant_boxed _ | Variant_extensible))
        | Record_mixed shape ->
            Block (Mixed_record (Lambda.transl_mixed_product_shape
              ~get_value_kind:(fun _i -> Lambda.generic_value)
              shape))
        | Record_unboxed | Record_ufloat
        | Record_inlined (_, _, (Variant_unboxed | Variant_with_null)) ->
            Misc.fatal_error "size_of_primitive"
        end
    | Pmakeblock _ | Pmakelazyblock _ ->
        (* The block shape is unfortunately an option, so we rely on the
           number of arguments instead.
           Note that flat float arrays/records use Pmakearray, so we don't need
           to check the tag here. *)
        Block (Regular_block (List.length args))
    | Pmakemixedblock (_, _, shape, _) ->
        Block (Mixed_record (shape))
    | Pmakearray (kind, _, _) ->
        let size = List.length args in
        begin match kind with
        | Pgenarray | Paddrarray | Pintarray ->
            Block (Regular_block size)
        | Pfloatarray ->
            Block (Float_record size)
        | Punboxedfloatarray _ | Punboxedintarray _ | Punboxedvectorarray _
        | Pgcscannableproductarray _ | Pgcignorableproductarray _ ->
            Misc.fatal_error "size_of_primitive"
        end
    | Pmakearray_dynamic _ -> Misc.fatal_error "size_of_primitive"
    | Parrayblit _ -> Constant
    | Pduparray _ ->
        (* The size has to be recovered from the size of the argument *)
        begin match args with
        | [arg] ->
            compute_expression_size env arg
        | [] | _ :: _ :: _ ->
            Misc.fatal_error "size_of_primitive"
        end

    | Praise _ ->
        Unreachable

    | Pctconst _ ->
        (* These primitives are not special-cased by [Value_rec_check],
           so we should never end up here; but these are constants anyway. *)
        Constant

    | Pbytes_to_string
    | Pbytes_of_string
    | Pgetglobal _
    | Psetglobal _
    | Pfield _
    | Pfield_computed _
    | Pfloatfield _
    | Pmixedfield _
    | Prunstack
    | Pperform
    | Presume
    | Preperform
    | Pccall _
    | Psequand | Psequor | Pnot
    | Pnegint | Paddint | Psubint | Pmulint
    | Pdivint _ | Pmodint _
    | Pandint | Porint | Pxorint
    | Plslint | Plsrint | Pasrint
    | Pintcomp _
    | Pcompare_ints | Pcompare_floats _ | Pcompare_bints _
    | Pintoffloat _ | Pfloatofint _
    | Pnegfloat _ | Pabsfloat _
    | Paddfloat _ | Psubfloat _ | Pmulfloat _ | Pdivfloat _
    | Pfloatcomp _
    | Pstringlength | Pstringrefu  | Pstringrefs
    | Pbyteslength | Pbytesrefu | Pbytesrefs
    | Parraylength _
    | Parrayrefu _
    | Parrayrefs _
    | Pisint _
    | Pisnull
    | Pisout
    | Pbintofint _
    | Pintofbint _
    | Pcvtbint _
    | Pnegbint _
    | Paddbint _
    | Psubbint _
    | Pmulbint _
    | Pdivbint _
    | Pmodbint _
    | Pandbint _
    | Porbint _
    | Pxorbint _
    | Plslbint _
    | Plsrbint _
    | Pasrbint _
    | Pbintcomp _
    | Pbigarrayref _
    | Pbigarraydim _
    | Pstring_load_16 _
    | Pstring_load_32 _
    | Pstring_load_f32 _
    | Pstring_load_64 _
    | Pbytes_load_16 _
    | Pbytes_load_32 _
    | Pbytes_load_f32 _
    | Pbytes_load_64 _
    | Pbigstring_load_16 _
    | Pbigstring_load_32 _
    | Pbigstring_load_f32 _
    | Pbigstring_load_64 _
    | Pbswap16
    | Pbbswap _
    | Pint_as_pointer _
    | Patomic_load_field _
    | Patomic_set_field _
    | Patomic_exchange_field _
    | Patomic_compare_exchange_field _
    | Patomic_compare_set_field _
    | Patomic_fetch_add_field
    | Popaque _
    | Pdls_get
    | Ppeek _
    | Ppoke _ ->
        dynamic_size lam

    (* Primitives specific to oxcaml *)
    | Pmakefloatblock (_, _) ->
        let size = List.length args in
        Block (Float_record size)

    | Psetufloatfield (_, _)
    | Pbytes_set_vec _
    | Pbigstring_set_vec _
    | Pfloatarray_set_vec _
    | Pfloat_array_set_vec _
    | Pint_array_set_vec _
    | Punboxed_float_array_set_vec _
    | Punboxed_float32_array_set_vec _
    | Punboxed_int32_array_set_vec _
    | Punboxed_int64_array_set_vec _
    | Punboxed_nativeint_array_set_vec _
    | Parray_element_size_in_bytes _
    | Punbox_unit ->
        Constant

    | Pmakeufloatblock (_, _)
    | Pmake_unboxed_product _ ->
        dynamic_size lam (* Not allowed *)

    | Pobj_dup
    | Parray_to_iarray
    | Parray_of_iarray
    | Pgetpredef _
    | Pufloatfield (_, _)
    | Punboxed_product_field (_, _)
    | Punboxed_float_comp (_, _)
    | Punboxed_int_comp (_, _)
    | Pstring_load_vec _
    | Pbytes_load_vec _
    | Pbigstring_load_vec _
    | Pfloatarray_load_vec _
    | Pfloat_array_load_vec _
    | Pint_array_load_vec _
    | Punboxed_float_array_load_vec _
    | Punboxed_float32_array_load_vec _
    | Punboxed_int32_array_load_vec _
    | Punboxed_int64_array_load_vec _
    | Punboxed_nativeint_array_load_vec _
    | Pprobe_is_enabled _
    | Pobj_magic _
    | Punbox_float _
    | Pbox_float (_, _)
    | Puntag_int _
    | Ptag_int _
    | Punbox_int _
    | Pbox_int (_, _)
    | Punbox_vector _
    | Pbox_vector (_, _)
    | Pfloatoffloat32 _
    | Pfloat32offloat _
    | Pget_header _
    | Preinterpret_tagged_int63_as_unboxed_int64
    | Preinterpret_unboxed_int64_as_tagged_int63 ->
        dynamic_size lam
  in
  compute_expression_size Ident.Map.empty lam

let lfunction_with_body { kind; params; return; body = _; attr; loc;
                          mode; ret_mode } body =
  lfunction' ~kind ~params ~return ~body ~attr ~loc ~mode ~ret_mode

(** {1. Function Lifting} *)

(* The compiler allows recursive definitions of functions that are not
   syntactic functions:
   {[
     let rec f_syntactic_function = fun x ->
       f_syntactic_function x

     let rec g_needs_lift =
       let () = ... in
       (fun x -> g_needs_lift (foo x))

     let rec h_needs_lift_and_closure =
       let v = ref 0 in
       (fun x -> incr v; h_needs_lift_and_closure (bar x))

     let rec i_needs_lift_and_eta =
       let aux x = i_needs_lift_and_eta (baz x) in
       aux
   ]}

   We need to translate those using only syntactic functions or blocks.
   For some functions, we only need to lift a syntactic function in tail
   position from its surrounding context:
   {[
     let rec g_context =
       let () = ... in
       ()
     and g_lifted = fun x ->
       g_lifted (foo x)
   ]}

   In general the function may refer to local variables, so we perform
   a local closure conversion before lifting:
   {[
     let rec h_context =
       let v = ref 0 in
       { v }
     and h_lifted = fun x ->
       incr h_context.v;
       h_lifted (bar x)
   ]}
   Note that the closure environment computed from the context is passed as a
   mutually recursive definition, that is, a free variable, and not as an
   additional function parameter (which is customary for closure conversion).

   Finally, when the tail expression is a variable, we perform an eta-expansion
   to get a syntactic function, that we can then close and lift:
   {[
     let rec i_context =
       let aux x = i_lifted (baz x) in
       { aux }
     and i_lifted = fun x -> i_context.aux x
   ]}
*)

type lifted_function =
  { lfun : Lambda.lfunction;
    free_vars_block_size : int;
  }

type 'a split_result =
  | Unreachable
  | Reachable of lifted_function * 'a

let ( let+ ) res f =
  match res with
  | Unreachable -> Unreachable
  | Reachable (func, lam) -> Reachable (func, f lam)

(* The closure blocks are immutable.
   (Note: It is usually safe to declare immutable blocks as mutable,
   but in this case the blocks might be empty and declaring them as Mutable
   would cause errors later.) *)
let lifted_block_mut : Lambda.mutable_flag = Immutable
let lifted_block_read_sem : Lambda.field_read_semantics = Reads_agree

let no_loc = Debuginfo.Scoped_location.Loc_unknown

let rec split_static_function lfun block_var local_idents lam :
  Lambda.lambda split_result =
  match lam with
  | Lvar v ->
    (* Eta-expand *)
    let params =
      List.map (fun (p : Lambda.lparam) ->
          { p with name = Ident.rename p.name })
        lfun.params
    in
    let ap_func =
      Lprim (Pfield (0, Pointer, lifted_block_read_sem), [Lvar block_var], no_loc)
    in
    let body =
      Lapply {
        ap_func;
        ap_args = List.map (fun p -> Lvar (p.name)) params;
        ap_loc = no_loc;
        ap_tailcall = Default_tailcall;
        ap_inlined = Default_inlined;
        ap_specialised = Default_specialise;
        ap_result_layout = lfun.return;
        ap_region_close = Rc_normal;
        ap_mode = lfun.ret_mode;
        ap_probe = None;
      }
    in
    let wrapper =
      lfunction'
        ~kind:lfun.kind
        ~params
        ~return:lfun.return
        ~body
        ~attr:default_stub_attribute
        ~loc:no_loc
        ~mode:lfun.mode
        ~ret_mode:lfun.ret_mode
    in
    let lifted = { lfun = wrapper; free_vars_block_size = 1 } in
    Reachable (lifted,
               Lprim (Pmakeblock (0, lifted_block_mut, None, Lambda.alloc_heap),
                      [Lvar v], no_loc))
  | Lfunction lfun ->
    let free_vars = Lambda.free_variables lfun.body in
    let local_free_vars = Ident.Set.inter free_vars local_idents in
    let free_vars_block_size, subst, block_fields_rev =
      Ident.Set.fold (fun var (i, subst, fields) ->
          let access =
            Lprim (Pfield (i, Pointer, lifted_block_read_sem),
                   [Lvar block_var],
                   no_loc)
          in
          (succ i, Ident.Map.add var access subst, Lvar var :: fields))
        local_free_vars (0, Ident.Map.empty, [])
    in
    (* Note: When there are no local free variables, we don't need the
       substitution and we don't need to generate code for pre-allocating
       and backpatching a block of size 0.
       However, the general scheme also works and it's unlikely to be
       noticeably worse, so we use it for simplicity. *)
    let new_fun =
      lfunction_with_body lfun
        (Lambda.subst (fun _ _ env -> env) subst lfun.body)
    in
    let lifted = { lfun = new_fun; free_vars_block_size } in
    let block =
      Lprim (Pmakeblock (0, lifted_block_mut, None, Lambda.alloc_heap),
             List.rev block_fields_rev,
             no_loc)
    in
    Reachable (lifted, block)
  | Llet (lkind, vkind, var, debug_uid, def, body) ->
    let+ body =
      split_static_function lfun block_var (Ident.Set.add var local_idents) body
    in
    Llet (lkind, vkind, var, debug_uid, def, body)
  | Lmutlet (vkind, var, debug_uid, def, body) ->
    let+ body =
      split_static_function lfun block_var (Ident.Set.add var local_idents) body
    in
    Lmutlet (vkind, var, debug_uid, def, body)
  | Lletrec (bindings, body) ->
    let local_idents =
      List.fold_left (fun ids { id } -> Ident.Set.add id ids)
        local_idents bindings
    in
    let+ body =
      split_static_function lfun block_var local_idents body
    in
    Lletrec (bindings, body)
  | Lprim (Praise _, _, _) -> Unreachable
  | Lstaticraise _ -> Unreachable
  | Lswitch (arg, sw, loc, layout) ->
    let sw_consts_res = rebuild_arms lfun block_var local_idents sw.sw_consts in
    let sw_blocks_res = rebuild_arms lfun block_var local_idents sw.sw_blocks in
    let sw_failaction_res =
      Option.map (split_static_function lfun block_var local_idents) sw.sw_failaction
    in
    begin match sw_consts_res, sw_blocks_res, sw_failaction_res with
    | Unreachable, Unreachable, (None | Some Unreachable) -> Unreachable
    | Reachable (lfun, sw_consts), Unreachable, (None | Some Unreachable) ->
      Reachable (lfun, Lswitch (arg, { sw with sw_consts }, loc, layout))
    | Unreachable, Reachable (lfun, sw_blocks), (None | Some Unreachable) ->
      Reachable (lfun, Lswitch (arg, { sw with sw_blocks }, loc, layout))
    | Unreachable, Unreachable, Some (Reachable (lfun, failaction)) ->
      let switch =
        Lswitch (arg, { sw with sw_failaction = Some failaction }, loc, layout)
      in
      Reachable (lfun, switch)
    | Reachable _, Reachable _, _ | Reachable _, _, Some (Reachable _)
    | _, Reachable _, Some (Reachable _) ->
      Misc.fatal_errorf "letrec: multiple functions:@ lfun=%a@ lam=%a"
        Printlambda.lfunction lfun
        Printlambda.lambda lam
    end
  | Lstringswitch (arg, arms, failaction, loc, layout) ->
    let arms_res = rebuild_arms lfun block_var local_idents arms in
    let failaction_res =
      Option.map (split_static_function lfun block_var local_idents) failaction
    in
    begin match arms_res, failaction_res with
    | Unreachable, (None | Some Unreachable) -> Unreachable
    | Reachable (lfun, arms), (None | Some Unreachable) ->
      Reachable (lfun, Lstringswitch (arg, arms, failaction, loc, layout))
    | Unreachable, Some (Reachable (lfun, failaction)) ->
      Reachable (lfun, Lstringswitch (arg, arms, Some failaction, loc, layout))
    | Reachable _, Some (Reachable _) ->
      Misc.fatal_errorf "letrec: multiple functions:@ lfun=%a@ lam=%a"
        Printlambda.lfunction lfun
        Printlambda.lambda lam
    end
  | Lstaticcatch (body, (nfail, params), handler, r, layout) ->
    let body_res = split_static_function lfun block_var local_idents body in
    let handler_res =
      let local_idents =
        List.fold_left (fun vars (var, _, _) -> Ident.Set.add var vars)
          local_idents params
      in
      split_static_function lfun block_var local_idents handler
    in
    begin match body_res, handler_res with
    | Unreachable, Unreachable -> Unreachable
    | Reachable (lfun, body), Unreachable ->
      Reachable (lfun, Lstaticcatch (body, (nfail, params), handler, r, layout))
    | Unreachable, Reachable (lfun, handler) ->
      Reachable (lfun, Lstaticcatch (body, (nfail, params), handler, r, layout))
    | Reachable _, Reachable _ ->
      Misc.fatal_errorf "letrec: multiple functions:@ lfun=%a@ lam=%a"
        Printlambda.lfunction lfun
        Printlambda.lambda lam
    end
  | Ltrywith (body, exn_var, debug_uid, handler, layout) ->
    let body_res = split_static_function lfun block_var local_idents body in
    let handler_res =
      split_static_function lfun block_var
        (Ident.Set.add exn_var local_idents) handler
    in
    begin match body_res, handler_res with
    | Unreachable, Unreachable -> Unreachable
    | Reachable (lfun, body), Unreachable ->
      Reachable (lfun, Ltrywith (body, exn_var, debug_uid, handler, layout))
    | Unreachable, Reachable (lfun, handler) ->
      Reachable (lfun, Ltrywith (body, exn_var, debug_uid, handler, layout))
    | Reachable _, Reachable _ ->
      Misc.fatal_errorf "letrec: multiple functions:@ lfun=%a@ lam=%a"
        Printlambda.lfunction lfun
        Printlambda.lambda lam
    end
  | Lifthenelse (cond, ifso, ifnot, layout) ->
    let ifso_res = split_static_function lfun block_var local_idents ifso in
    let ifnot_res = split_static_function lfun block_var local_idents ifnot in
    begin match ifso_res, ifnot_res with
    | Unreachable, Unreachable -> Unreachable
    | Reachable (lfun, ifso), Unreachable ->
      Reachable (lfun, Lifthenelse (cond, ifso, ifnot, layout))
    | Unreachable, Reachable (lfun, ifnot) ->
      Reachable (lfun, Lifthenelse (cond, ifso, ifnot, layout))
    | Reachable _, Reachable _ ->
      Misc.fatal_errorf "letrec: multiple functions:@ lfun=%a@ lam=%a"
        Printlambda.lfunction lfun
        Printlambda.lambda lam
    end
  | Lsequence (e1, e2) ->
    let+ e2 = split_static_function lfun block_var local_idents e2 in
    Lsequence (e1, e2)
  | Levent (lam, lev) ->
    let+ lam = split_static_function lfun block_var local_idents lam in
    Levent (lam, lev)
  | Lregion (lam, layout_fun) ->
    (* The type-checker forbids recursive values from being allocated on the
       stack, so this region is only here to collect temporary allocations.
       In particular the function itself does not capture any stack-allocated
       variables, so we can lift it out of the region. *)
    let+ lam = split_static_function lfun block_var local_idents lam in
    (* The new expression returns the closure block instead of the function *)
    ignore layout_fun;
    Lregion (lam, layout_block)
  | Lmutvar _
  | Lconst _
  | Lapply _
  | Lprim _
  | Lwhile _
  | Lfor _
  | Lassign _
  | Lsend _
  | Lifused _
  | Lexclave _ ->
    Misc.fatal_errorf
      "letrec binding is not a static function:@ lfun=%a@ lam=%a"
      Printlambda.lfunction lfun
      Printlambda.lambda lam
and rebuild_arms :
  type a. _ -> _ -> _ -> (a * Lambda.lambda) list ->
  (a * Lambda.lambda) list split_result =
  fun lfun block_var local_idents arms ->
  match arms with
  | [] -> Unreachable
  | (i, lam) :: arms ->
    let res = rebuild_arms lfun block_var local_idents arms in
    let lam_res = split_static_function lfun block_var local_idents lam in
    match lam_res, res with
    | Unreachable, Unreachable -> Unreachable
    | Reachable (lfun, lam), Unreachable ->
      Reachable (lfun, (i, lam) :: arms)
    | Unreachable, Reachable (lfun, arms) ->
      Reachable (lfun, (i, lam) :: arms)
    | Reachable _, Reachable _ ->
      Misc.fatal_errorf "letrec: multiple functions:@ lfun=%a@ lam=%a"
        Printlambda.lfunction lfun
        Printlambda.lambda lam

(** {1. Compilation} *)

(** The bindings are split into three categories.
    Static bindings are the ones that we can pre-allocate and backpatch later.
    Function bindings are syntactic functions.
    Dynamic bindings are non-recursive expressions.

    The evaluation order is as follows:
    - Evaluate all dynamic bindings
    - Pre-allocate all static bindings
    - Define all functions
    - Backpatch all static bindings

    Constants (and unreachable expressions) end up in the dynamic category,
    because we substitute all occurrences of recursive variables in their
    definition by a dummy expression, making them non-recursive.

    This is correct because:
    - [Value_rec_check] ensured that they never dereference the value of
      those recursive variables
    - their final value cannot depend on them either.

    Functions that are not already in syntactic form also generate an additional
    binding for the context. This binding fits into the static category.

    Example input:
    {[
      let rec a x =
        (* syntactic function *)
        b x
      and b =
        (* non-syntactic function *)
        let tbl = Hashtbl.make 17 in
        fun x -> ... (tbl, c, a) ...
      and c =
        (* block *)
        Some (d, default)
      and d =
        (* 'dynamic' value (not recursive *)
        Array.make 5 0
      and default =
        (* constant, with (spurious) use
           of a recursive neighbor *)
        let _ = a in
        42
    ]}

    Example output:
    {[
      (* Dynamic bindings *)
      let d = Array.make 5 0
      let default =
        let _ = *dummy_rec_value* in
        42

      (* Pre-allocations *)
      let c = caml_alloc_dummy 2
      let b_context = caml_alloc_dummy 1

      (* Functions *)
      let rec a x = b x
      and b =
        fun x -> ... (b_context.tbl, c, a) ...

      (* Backpatching *)
      let () =
        caml_update_dummy c (Some (d, default));
        caml_update_dummy b_context
          (let tbl = Hashtbl.make 17 in
           { tbl })
    ]}

    Note on performance for non-syntactic functions:
    The compiler would previously pre-allocate and backpatch function
    closures. The new approach is designed to avoid back-patching
    closures -- besides, we could not pre-allocate at this point in the
    compiler pipeline, as the closure size will only be determined later.

    For non-syntactic functions with local free variables, we now store the
    local free variables in a block, which incurs an additional indirection
    whenever a local variable is accessed by the function. On the other hand,
    we generate regular function definitions, so the rest of the compiler
    can either inline them or generate direct calls, and use the compact
    representation for mutually recursive closures.
 *)

type rec_bindings =
  { static : (Ident.t * Lambda.debug_uid * block_size * Lambda.lambda) list;
    functions : (Ident.t * Lambda.debug_uid * Lambda.lfunction) list;
    dynamic : (Ident.t * Lambda.debug_uid * Lambda.lambda) list;
  }

let empty_bindings =
  { static = [];
    functions = [];
    dynamic = [];
  }

(** Allocation and backpatching primitives *)

let alloc_prim =
  Lambda.simple_prim_on_values ~name:"caml_alloc_dummy" ~arity:1 ~alloc:true

let alloc_float_record_prim =
  Lambda.simple_prim_on_values ~name:"caml_alloc_dummy_float" ~arity:1 ~alloc:true

let alloc_mixed_record_prim =
  Lambda.simple_prim_on_values ~name:"caml_alloc_dummy_mixed" ~arity:2 ~alloc:true

let update_prim =
  (* Note: [alloc] could be false, but it probably doesn't matter *)
  Lambda.simple_prim_on_values ~name:"caml_update_dummy" ~arity:2 ~alloc:true

(** Compilation function *)

let compile_letrec input_bindings body =
  if !Clflags.dump_letreclambda then (
    Format.eprintf "Value_rec_compiler input bindings:\n";
    List.iter (fun (id, _, _, def) ->
        Format.eprintf "  %a = %a\n%!" Ident.print id Printlambda.lambda def)
      input_bindings;
    Format.eprintf "Value_rec_compiler body:@ %a\n%!" Printlambda.lambda body
  );
  let subst_for_constants =
    List.fold_left (fun subst (id, _, _, _) ->
        Ident.Map.add id Lambda.dummy_constant subst)
      Ident.Map.empty input_bindings
  in
  let all_bindings_rev =
    List.fold_left (fun rev_bindings (id, duid, rkind, def) ->
        match (rkind : Value_rec_types.recursive_binding_kind) with
        | Dynamic ->
          { rev_bindings
            with dynamic = (id, duid, def) :: rev_bindings.dynamic }
        | Static ->
          let size = compute_static_size def in
          begin match size with
          | Constant | Unreachable ->
            (* The result never escapes any recursive variables, so as we know
               it doesn't inspect them either we can just bind the recursive
               variables to dummy values and evaluate the definition normally.
            *)
            let def =
              Lambda.subst (fun _ _ env -> env) subst_for_constants def
            in
            { rev_bindings
              with dynamic = (id, duid, def) :: rev_bindings.dynamic }
          | Block size ->
            { rev_bindings with
              static = (id, duid, size, def) :: rev_bindings.static }
          | Function lfun ->
            begin match def with
            | Lfunction lfun ->
              { rev_bindings with
                functions = (id, duid, lfun) :: rev_bindings.functions
              }
            | _ ->
              let ctx_id = Ident.create_local "letrec_function_context" in
              let ctx_id_duid = Lambda.debug_uid_none in
              begin match
                split_static_function lfun ctx_id Ident.Set.empty def
              with
              | Unreachable ->
                Misc.fatal_errorf
                  "letrec: no function for binding:@ def=%a@ lfun=%a"
                  Printlambda.lambda def Printlambda.lfunction lfun
              | Reachable ({ lfun; free_vars_block_size }, lam) ->
                let functions = (id, duid, lfun) :: rev_bindings.functions in
                let static =
                  (ctx_id, ctx_id_duid,
                    Regular_block free_vars_block_size, lam)
                  :: rev_bindings.static
                in
                { rev_bindings with functions; static }
              end
            end
          end)
      empty_bindings input_bindings
  in
  let body_with_patches =
    List.fold_left (fun body (id, _, _size, lam) ->
        let update =
          Lprim (Pccall update_prim, [Lvar id; lam], no_loc)
        in
        Lsequence (update, body))
      body (all_bindings_rev.static)
  in
  let body_with_functions =
    match all_bindings_rev.functions with
    | [] -> body_with_patches
    | bindings_rev ->
      let function_bindings =
        List.rev_map (fun (id, debug_uid, lfun) ->
            { id; debug_uid; def = lfun })
          bindings_rev
      in
      Lletrec (function_bindings, body_with_patches)
  in
  let body_with_dynamic_values =
    List.fold_left (fun body (id, duid, lam) ->
        Llet(Strict, Lambda.layout_letrec, id, duid, lam, body))
      body_with_functions all_bindings_rev.dynamic
  in
  let body_with_pre_allocations =
    List.fold_left (fun body (id, duid, size, _lam) ->
        let alloc_prim, const_args =
          match size with
          | Regular_block size -> alloc_prim, [size]
          | Float_record size -> alloc_float_record_prim, [size]
          | Mixed_record shape ->
              let shape =
                Mixed_block_shape.of_mixed_block_elements
                  ~print_locality:(fun ppf () -> Format.fprintf ppf "()")
                  shape
              in
              let value_prefix_len = Mixed_block_shape.value_prefix_len shape in
              let flat_suffix_len = Mixed_block_shape.flat_suffix_len shape in
              let size = value_prefix_len + flat_suffix_len in
              alloc_mixed_record_prim, [size; value_prefix_len]
        in
        let alloc =
          Lprim (Pccall alloc_prim,
                 List.map (fun n -> Lconst (Lambda.const_int n)) const_args,
                 no_loc)
        in
        Llet(Strict, Lambda.layout_letrec, id, duid, alloc, body))
      body_with_dynamic_values all_bindings_rev.static
  in
  body_with_pre_allocations
