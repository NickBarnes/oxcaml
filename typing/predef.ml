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

(* Predefined type constructors (with special typing rules in typecore) *)

open Path
open Types
open Btype

let builtin_idents = ref []

let wrap create s =
  let id = create s in
  builtin_idents := (s, id) :: !builtin_idents;
  id

let ident_create = wrap Ident.create_predef

let ident_int = ident_create "int"
and ident_char = ident_create "char"
and ident_bytes = ident_create "bytes"
and ident_float = ident_create "float"
and ident_float32 = ident_create "float32"
and ident_bool = ident_create "bool"
and ident_unit = ident_create "unit"
and ident_exn = ident_create "exn"
and ident_array = ident_create "array"
and ident_iarray = ident_create "iarray"
and ident_list = ident_create "list"
and ident_option = ident_create "option"
and ident_nativeint = ident_create "nativeint"
and ident_int8 = ident_create "int8"
and ident_int16 = ident_create "int16"
and ident_int32 = ident_create "int32"
and ident_int64 = ident_create "int64"
and ident_lazy_t = ident_create "lazy_t"
and ident_string = ident_create "string"
and ident_extension_constructor = ident_create "extension_constructor"
and ident_floatarray = ident_create "floatarray"
and ident_lexing_position = ident_create "lexing_position"

and ident_or_null = ident_create "or_null"

and ident_int8x16 = ident_create "int8x16"
and ident_int16x8 = ident_create "int16x8"
and ident_int32x4 = ident_create "int32x4"
and ident_int64x2 = ident_create "int64x2"
and ident_float32x4 = ident_create "float32x4"
and ident_float64x2 = ident_create "float64x2"
and ident_int8x32 = ident_create "int8x32"
and ident_int16x16 = ident_create "int16x16"
and ident_int32x8 = ident_create "int32x8"
and ident_int64x4 = ident_create "int64x4"
and ident_float32x8 = ident_create "float32x8"
and ident_float64x4 = ident_create "float64x4"
and ident_int8x64 = ident_create "int8x64"
and ident_int16x32 = ident_create "int16x32"
and ident_int32x16 = ident_create "int32x16"
and ident_int64x8 = ident_create "int64x8"
and ident_float32x16 = ident_create "float32x16"
and ident_float64x8 = ident_create "float64x8"

let path_int = Pident ident_int
and path_char = Pident ident_char
and path_bytes = Pident ident_bytes
and path_float = Pident ident_float
and path_float32 = Pident ident_float32
and path_bool = Pident ident_bool
and path_unit = Pident ident_unit
and path_exn = Pident ident_exn
and path_array = Pident ident_array
and path_iarray = Pident ident_iarray
and path_list = Pident ident_list
and path_option = Pident ident_option
and path_nativeint = Pident ident_nativeint
and path_int8 = Pident ident_int8
and path_int16 = Pident ident_int16
and path_int32 = Pident ident_int32
and path_int64 = Pident ident_int64
and path_lazy_t = Pident ident_lazy_t
and path_string = Pident ident_string
and path_extension_constructor = Pident ident_extension_constructor
and path_floatarray = Pident ident_floatarray
and path_lexing_position = Pident ident_lexing_position

and path_or_null = Pident ident_or_null

and path_int8x16 = Pident ident_int8x16
and path_int16x8 = Pident ident_int16x8
and path_int32x4 = Pident ident_int32x4
and path_int64x2 = Pident ident_int64x2
and path_float32x4 = Pident ident_float32x4
and path_float64x2 = Pident ident_float64x2
and path_int8x32 = Pident ident_int8x32
and path_int16x16 = Pident ident_int16x16
and path_int32x8 = Pident ident_int32x8
and path_int64x4 = Pident ident_int64x4
and path_float32x8 = Pident ident_float32x8
and path_float64x4 = Pident ident_float64x4
and path_int8x64 = Pident ident_int8x64
and path_int16x32 = Pident ident_int16x32
and path_int32x16 = Pident ident_int32x16
and path_int64x8 = Pident ident_int64x8
and path_float32x16 = Pident ident_float32x16
and path_float64x8 = Pident ident_float64x8

let path_unboxed_float = Path.unboxed_version path_float
and path_unboxed_float32 = Path.unboxed_version path_float32
and path_unboxed_nativeint = Path.unboxed_version path_nativeint
and path_unboxed_int8 = Path.unboxed_version path_int8
and path_unboxed_int16 = Path.unboxed_version path_int16
and path_unboxed_int32 = Path.unboxed_version path_int32
and path_unboxed_int64 = Path.unboxed_version path_int64

and path_unboxed_int8x16 = Path.unboxed_version path_int8x16
and path_unboxed_int16x8 = Path.unboxed_version path_int16x8
and path_unboxed_int32x4 = Path.unboxed_version path_int32x4
and path_unboxed_int64x2 = Path.unboxed_version path_int64x2
and path_unboxed_float32x4 = Path.unboxed_version path_float32x4
and path_unboxed_float64x2 = Path.unboxed_version path_float64x2
and path_unboxed_int8x32 = Path.unboxed_version path_int8x32
and path_unboxed_int16x16 = Path.unboxed_version path_int16x16
and path_unboxed_int32x8 = Path.unboxed_version path_int32x8
and path_unboxed_int64x4 = Path.unboxed_version path_int64x4
and path_unboxed_float32x8 = Path.unboxed_version path_float32x8
and path_unboxed_float64x4 = Path.unboxed_version path_float64x4
and path_unboxed_int8x64 = Path.unboxed_version path_int8x64
and path_unboxed_int16x32 = Path.unboxed_version path_int16x32
and path_unboxed_int32x16 = Path.unboxed_version path_int32x16
and path_unboxed_int64x8 = Path.unboxed_version path_int64x8
and path_unboxed_float32x16 = Path.unboxed_version path_float32x16
and path_unboxed_float64x8 = Path.unboxed_version path_float64x8

let type_int = newgenty (Tconstr(path_int, [], ref Mnil))
and type_int8 = newgenty (Tconstr(path_int8, [], ref Mnil))
and type_int16 = newgenty (Tconstr(path_int16, [], ref Mnil))
and type_char = newgenty (Tconstr(path_char, [], ref Mnil))
and type_bytes = newgenty (Tconstr(path_bytes, [], ref Mnil))
and type_float = newgenty (Tconstr(path_float, [], ref Mnil))
and type_float32 = newgenty (Tconstr(path_float32, [], ref Mnil))
and type_bool = newgenty (Tconstr(path_bool, [], ref Mnil))
and type_unit = newgenty (Tconstr(path_unit, [], ref Mnil))
and type_exn = newgenty (Tconstr(path_exn, [], ref Mnil))
and type_array t = newgenty (Tconstr(path_array, [t], ref Mnil))
and type_iarray t = newgenty (Tconstr(path_iarray, [t], ref Mnil))
and type_list t = newgenty (Tconstr(path_list, [t], ref Mnil))
and type_option t = newgenty (Tconstr(path_option, [t], ref Mnil))
and type_nativeint = newgenty (Tconstr(path_nativeint, [], ref Mnil))
and type_int32 = newgenty (Tconstr(path_int32, [], ref Mnil))
and type_int64 = newgenty (Tconstr(path_int64, [], ref Mnil))
and type_lazy_t t = newgenty (Tconstr(path_lazy_t, [t], ref Mnil))
and type_string = newgenty (Tconstr(path_string, [], ref Mnil))
and type_extension_constructor =
      newgenty (Tconstr(path_extension_constructor, [], ref Mnil))
and type_floatarray = newgenty (Tconstr(path_floatarray, [], ref Mnil))
and type_lexing_position = newgenty (Tconstr(path_lexing_position, [], ref Mnil))

and type_unboxed_float = newgenty (Tconstr(path_unboxed_float, [], ref Mnil))
and type_unboxed_float32 = newgenty (Tconstr(path_unboxed_float32, [], ref Mnil))
and type_unboxed_nativeint =
      newgenty (Tconstr(path_unboxed_nativeint, [], ref Mnil))
and type_unboxed_int32 = newgenty (Tconstr(path_unboxed_int32, [], ref Mnil))
and type_unboxed_int64 = newgenty (Tconstr(path_unboxed_int64, [], ref Mnil))
and type_unboxed_int8 = newgenty (Tconstr(path_unboxed_int8, [], ref Mnil))
and type_unboxed_int16 = newgenty (Tconstr(path_unboxed_int16, [], ref Mnil))
and type_or_null t = newgenty (Tconstr(path_or_null, [t], ref Mnil))

and type_int8x16 = newgenty (Tconstr(path_int8x16, [], ref Mnil))
and type_int16x8 = newgenty (Tconstr(path_int16x8, [], ref Mnil))
and type_int32x4 = newgenty (Tconstr(path_int32x4, [], ref Mnil))
and type_int64x2 = newgenty (Tconstr(path_int64x2, [], ref Mnil))
and type_float32x4 = newgenty (Tconstr(path_float32x4, [], ref Mnil))
and type_float64x2 = newgenty (Tconstr(path_float64x2, [], ref Mnil))
and type_int8x32 = newgenty (Tconstr(path_int8x32, [], ref Mnil))
and type_int16x16 = newgenty (Tconstr(path_int16x16, [], ref Mnil))
and type_int32x8 = newgenty (Tconstr(path_int32x8, [], ref Mnil))
and type_int64x4 = newgenty (Tconstr(path_int64x4, [], ref Mnil))
and type_float32x8 = newgenty (Tconstr(path_float32x8, [], ref Mnil))
and type_float64x4 = newgenty (Tconstr(path_float64x4, [], ref Mnil))
and type_int8x64 = newgenty (Tconstr(path_int8x64, [], ref Mnil))
and type_int16x32 = newgenty (Tconstr(path_int16x32, [], ref Mnil))
and type_int32x16 = newgenty (Tconstr(path_int32x16, [], ref Mnil))
and type_int64x8 = newgenty (Tconstr(path_int64x8, [], ref Mnil))
and type_float32x16 = newgenty (Tconstr(path_float32x16, [], ref Mnil))
and type_float64x8 = newgenty (Tconstr(path_float64x8, [], ref Mnil))

and type_unboxed_int8x16 =
  newgenty (Tconstr(path_unboxed_int8x16, [], ref Mnil))
and type_unboxed_int16x8 =
  newgenty (Tconstr(path_unboxed_int16x8, [], ref Mnil))
and type_unboxed_int32x4 =
  newgenty (Tconstr(path_unboxed_int32x4, [], ref Mnil))
and type_unboxed_int64x2 =
  newgenty (Tconstr(path_unboxed_int64x2, [], ref Mnil))
and type_unboxed_float32x4 =
  newgenty (Tconstr(path_unboxed_float32x4, [], ref Mnil))
and type_unboxed_float64x2 =
  newgenty (Tconstr(path_unboxed_float64x2, [], ref Mnil))
and type_unboxed_int8x32 =
  newgenty (Tconstr(path_unboxed_int8x32, [], ref Mnil))
and type_unboxed_int16x16 =
  newgenty (Tconstr(path_unboxed_int16x16, [], ref Mnil))
and type_unboxed_int32x8 =
  newgenty (Tconstr(path_unboxed_int32x8, [], ref Mnil))
and type_unboxed_int64x4 =
  newgenty (Tconstr(path_unboxed_int64x4, [], ref Mnil))
and type_unboxed_float32x8 =
  newgenty (Tconstr(path_unboxed_float32x8, [], ref Mnil))
and type_unboxed_float64x4 =
  newgenty (Tconstr(path_unboxed_float64x4, [], ref Mnil))
and type_unboxed_int8x64 =
  newgenty (Tconstr(path_unboxed_int8x64, [], ref Mnil))
and type_unboxed_int16x32 =
  newgenty (Tconstr(path_unboxed_int16x32, [], ref Mnil))
and type_unboxed_int32x16 =
  newgenty (Tconstr(path_unboxed_int32x16, [], ref Mnil))
and type_unboxed_int64x8 =
  newgenty (Tconstr(path_unboxed_int64x8, [], ref Mnil))
and type_unboxed_float32x16 =
  newgenty (Tconstr(path_unboxed_float32x16, [], ref Mnil))
and type_unboxed_float64x8 =
  newgenty (Tconstr(path_unboxed_float64x8, [], ref Mnil))

let ident_match_failure = ident_create "Match_failure"
and ident_out_of_memory = ident_create "Out_of_memory"
and ident_invalid_argument = ident_create "Invalid_argument"
and ident_failure = ident_create "Failure"
and ident_not_found = ident_create "Not_found"
and ident_sys_error = ident_create "Sys_error"
and ident_end_of_file = ident_create "End_of_file"
and ident_division_by_zero = ident_create "Division_by_zero"
and ident_stack_overflow = ident_create "Stack_overflow"
and ident_sys_blocked_io = ident_create "Sys_blocked_io"
and ident_assert_failure = ident_create "Assert_failure"
and ident_undefined_recursive_module =
        ident_create "Undefined_recursive_module"

let all_predef_exns = [
  ident_match_failure;
  ident_out_of_memory;
  ident_invalid_argument;
  ident_failure;
  ident_not_found;
  ident_sys_error;
  ident_end_of_file;
  ident_division_by_zero;
  ident_stack_overflow;
  ident_sys_blocked_io;
  ident_assert_failure;
  ident_undefined_recursive_module;
]

let path_match_failure = Pident ident_match_failure
and path_invalid_argument = Pident ident_invalid_argument
and path_assert_failure = Pident ident_assert_failure
and path_undefined_recursive_module = Pident ident_undefined_recursive_module

let cstr id args =
  {
    cd_id = id;
    cd_args = Cstr_tuple args;
    cd_res = None;
    cd_loc = Location.none;
    cd_attributes = [];
    cd_uid = Uid.of_predef_id id;
  }

let ident_false = ident_create "false"
and ident_true = ident_create "true"
and ident_void = ident_create "()"
and ident_nil = ident_create "[]"
and ident_cons = ident_create "::"
and ident_none = ident_create "None"
and ident_some = ident_create "Some"

and ident_null = ident_create "Null"
and ident_this = ident_create "This"

let option_argument_sort = Jkind.Sort.Const.value
let option_argument_jkind = Jkind.Builtin.value_or_null ~why:(
  Type_argument {parent_path = path_option; position = 1; arity = 1})

let list_jkind param =
  Jkind.Builtin.immutable_data ~why:Boxed_variant |>
  Jkind.add_with_bounds
    ~modality:Mode.Modality.Value.Const.id
    ~type_expr:param |>
  Jkind.mark_best

let list_sort = Jkind.Sort.Const.value
let list_argument_sort = Jkind.Sort.Const.value
let list_argument_jkind = Jkind.Builtin.value_or_null ~why:(
  Type_argument {parent_path = path_list; position = 1; arity = 1})

let mk_add_type add_type =
  let add_type_with_jkind
      ?manifest type_ident
      ?(kind=Type_abstract Definition)
      ~jkind
      ?unboxed_jkind
      env =
    let type_uid = Uid.of_predef_id type_ident in
    let type_unboxed_version = match unboxed_jkind with
      | None -> None
      | Some unboxed_jkind ->
        let type_jkind =
          Jkind.of_builtin ~why:(Unboxed_primitive type_ident) unboxed_jkind
        in
        let type_kind =
          match kind with
            | Type_abstract Definition -> Type_abstract Definition
            | _ ->
              Misc.fatal_error "Predef.mk_add_type: non-abstract unboxed kind"
        in
        let type_manifest =
          match manifest with
          | None -> None
          | Some _ ->
            Misc.fatal_error "Predef.mk_add_type: non-[None] unboxed manifest"
        in
        Some {
          type_params = [];
          type_arity = 0;
          type_kind;
          type_jkind = Jkind.mark_best type_jkind;
          type_loc = Location.none;
          type_private = Asttypes.Public;
          type_manifest;
          type_variance = [];
          type_separability = [];
          type_is_newtype = false;
          type_expansion_scope = lowest_level;
          type_attributes = [];
          type_unboxed_default = false;
          type_uid = Uid.unboxed_version type_uid;
          type_unboxed_version = None;
        }
    in
    let decl =
      {type_params = [];
      type_arity = 0;
      type_kind = kind;
      type_jkind = Jkind.mark_best jkind;
      type_loc = Location.none;
      type_private = Asttypes.Public;
      type_manifest = manifest;
      type_variance = [];
      type_separability = [];
      type_is_newtype = false;
      type_expansion_scope = lowest_level;
      type_attributes = [];
      type_unboxed_default = false;
      type_uid;
      type_unboxed_version;
      }
    in
    add_type type_ident decl env
  in
  let add_type ?manifest type_ident ?kind ~jkind ?unboxed_jkind env =
    let jkind = Jkind.of_builtin ~why:(Primitive type_ident) jkind in
    add_type_with_jkind ?manifest type_ident ?kind ~jkind ?unboxed_jkind env
  in
  add_type_with_jkind, add_type

let mk_add_type1 add_type type_ident
      ?(kind=fun _ -> Type_abstract Definition)
      ~jkind
      ?(param_jkind=Jkind.Builtin.value ~why:(
        Type_argument {
          parent_path = Path.Pident type_ident;
          position = 1;
          arity = 1}
      ))
    ~variance ~separability env =
  let param = newgenvar param_jkind in
  let decl =
    {type_params = [param];
      type_arity = 1;
      type_kind = kind param;
      type_jkind = Jkind.mark_best (jkind param);
      type_loc = Location.none;
      type_private = Asttypes.Public;
      type_manifest = None;
      type_variance = [variance];
      type_separability = [separability];
      type_is_newtype = false;
      type_expansion_scope = lowest_level;
      type_attributes = [];
      type_unboxed_default = false;
      type_uid = Uid.of_predef_id type_ident;
      type_unboxed_version = None;
    }
  in
  add_type type_ident decl env

let mk_add_extension add_extension id args =
  List.iter (fun (_, sort) ->
      let raise_error () = Misc.fatal_error
          "sanity check failed: non-value jkind in predef extension \
            constructor; should this have Constructor_mixed shape?" in
      match (sort : Jkind.Sort.Const.t) with
      | Base Value -> ()
      | Base (Void | Float32 | Float64 | Word | Bits8 | Bits16 | Bits32
             | Bits64 | Vec128 | Vec256 | Vec512)
      | Product _ -> raise_error ())
    args;
  add_extension id
    { ext_type_path = path_exn;
      ext_type_params = [];
      ext_args =
        Cstr_tuple
          (List.map
            (fun (ca_type, ca_sort) ->
              {
                ca_type;
                ca_sort;
                ca_modalities=Mode.Modality.Value.Const.id;
                ca_loc=Location.none
              })
            args);
      ext_shape = Constructor_uniform_value;
      ext_constant = args = [];
      ext_ret_type = None;
      ext_private = Asttypes.Public;
      ext_loc = Location.none;
      ext_attributes = [Ast_helper.Attr.mk
                          (Location.mknoloc "ocaml.warn_on_literal_pattern")
                          (Parsetree.PStr [])];
      ext_uid = Uid.of_predef_id id;
    }

let variant constrs =
  let mk_elt { cd_args } =
    let sorts = match cd_args with
      | Cstr_tuple args ->
        Misc.Stdlib.Array.of_list_map (fun { ca_sort } -> ca_sort) args
      | Cstr_record lbls ->
        Misc.Stdlib.Array.of_list_map (fun { ld_sort } -> ld_sort) lbls
    in
    Constructor_uniform_value, sorts
  in
  Type_variant (
    constrs,
    Variant_boxed (Misc.Stdlib.Array.of_list_map mk_elt constrs),
    None)

let unrestricted tvar ca_sort =
  {ca_type=tvar;
   ca_sort;
   ca_modalities=Mode.Modality.Value.Const.id;
   ca_loc=Location.none}

(* CR layouts: Changes will be needed here as we add support for the built-ins
   to work with non-values, and as we relax the mixed block restriction. *)
let build_initial_env add_type add_extension empty_env =
  let add_type_with_jkind, add_type = mk_add_type add_type
  and add_type1 = mk_add_type1 add_type
  and add_extension = mk_add_extension add_extension in
  empty_env
  (* Predefined types *)
  |> add_type1 ident_array
       ~variance:Variance.full
       ~separability:Separability.Ind
       ~param_jkind:(Jkind.Builtin.any_non_null ~why:Array_type_argument)
       ~jkind:(fun param ->
         Jkind.Builtin.mutable_data ~why:(Primitive ident_array) |>
         Jkind.add_with_bounds
           ~modality:Mode.Modality.Value.Const.id
           ~type_expr:param)
  |> add_type1 ident_iarray
       ~variance:Variance.covariant
       ~separability:Separability.Ind
       ~param_jkind:(Jkind.Builtin.any_non_null ~why:Array_type_argument)
       ~jkind:(fun param ->
         Jkind.Builtin.immutable_data ~why:(Primitive ident_iarray) |>
         Jkind.add_with_bounds
           ~modality:Mode.Modality.Value.Const.id
           ~type_expr:param)
  |> add_type ident_bool
       ~kind:(variant [ cstr ident_false []; cstr ident_true []])
       ~jkind:Jkind.Const.Builtin.immediate
  |> add_type ident_char ~jkind:Jkind.Const.Builtin.immediate
  |> add_type_with_jkind ident_exn ~kind:Type_open
    ~jkind:(Jkind.for_non_float ~why:(Primitive ident_exn))
  |> add_type ident_extension_constructor ~jkind:Jkind.Const.Builtin.immutable_data
  |> add_type_with_jkind ident_float ~jkind:(Jkind.for_float ident_float)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_float
  |> add_type ident_floatarray ~jkind:Jkind.Const.Builtin.mutable_data
  |> add_type ident_int ~jkind:Jkind.Const.Builtin.immediate
  |> add_type ident_int32 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_int32
  |> add_type ident_int64 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_int64
  |> add_type1 ident_lazy_t
       ~variance:Variance.covariant
       ~separability:Separability.Ind
       (* CR layouts v2.8: Can [lazy_t] mode-cross at all? According to Zesen:
          It can at least cross locality, because it's always heap-allocated.
          It might also cross portability, linearity, uniqueness subject to its
          parameter. But I'm also fine not doing that for now (and wait until
          users complains).  *)
       ~jkind:(fun _ -> Jkind.for_non_float ~why:(Primitive ident_lazy_t))
  |> add_type1 ident_list
       ~variance:Variance.covariant
       ~separability:Separability.Ind
       ~kind:(fun tvar ->
         variant [cstr ident_nil [];
                  cstr ident_cons [unrestricted tvar list_argument_sort;
                                   unrestricted (type_list tvar) list_sort]])
       ~param_jkind:list_argument_jkind
       ~jkind:list_jkind
  |> add_type ident_nativeint
      ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_nativeint
  |> add_type1 ident_option
       ~variance:Variance.covariant
       ~separability:Separability.Ind
       ~kind:(fun tvar ->
         variant [cstr ident_none [];
                  cstr ident_some [unrestricted tvar option_argument_sort]])
       ~param_jkind:option_argument_jkind
       ~jkind:(fun param ->
         Jkind.Builtin.immutable_data ~why:Boxed_variant |>
         Jkind.add_with_bounds
           ~modality:Mode.Modality.Value.Const.id
           ~type_expr:param)
  |> add_type_with_jkind ident_lexing_position
       ~kind:(
         let lbl (field, field_type) =
           let id = Ident.create_predef field in
             {
               ld_id=id;
               ld_mutable=Immutable;
               ld_modalities=Mode.Modality.Value.Const.id;
               ld_type=field_type;
               ld_sort=Jkind.Sort.Const.value;
               ld_loc=Location.none;
               ld_attributes=[];
               ld_uid=Uid.of_predef_id id;
             }
         in
         let labels = List.map lbl [
           ("pos_fname", type_string);
           ("pos_lnum", type_int);
           ("pos_bol", type_int);
           ("pos_cnum", type_int) ]
         in
         Type_record (
           labels,
           (Record_boxed (List.map (fun label -> label.ld_sort) labels |> Array.of_list)),
           None
         )
       )
       (* CR layouts v2.8: Possibly remove this -- and simplify [mk_add_type] --
          when we have a better jkind subsumption check. *)
       ~jkind:Jkind.(
         of_builtin Const.Builtin.immutable_data
           ~why:(Primitive ident_lexing_position) |>
         add_with_bounds ~modality:Mode.Modality.Value.Const.id ~type_expr:type_int |>
         add_with_bounds ~modality:Mode.Modality.Value.Const.id ~type_expr:type_int |>
         add_with_bounds ~modality:Mode.Modality.Value.Const.id ~type_expr:type_int |>
         add_with_bounds ~modality:Mode.Modality.Value.Const.id ~type_expr:type_string)
  |> add_type ident_string ~jkind:Jkind.Const.Builtin.immutable_data
  |> add_type ident_bytes ~jkind:Jkind.Const.Builtin.mutable_data
  |> add_type ident_unit
       ~kind:(variant [cstr ident_void []])
       ~jkind:Jkind.Const.Builtin.immediate
  (* Predefined exceptions - alphabetical order *)
  |> add_extension ident_assert_failure
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int]),
        Jkind.Sort.Const.value]
  |> add_extension ident_division_by_zero []
  |> add_extension ident_end_of_file []
  |> add_extension ident_failure [type_string,
       Jkind.Sort.Const.value]
  |> add_extension ident_invalid_argument [type_string,
       Jkind.Sort.Const.value]
  |> add_extension ident_match_failure
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int]),
       Jkind.Sort.Const.value]
  |> add_extension ident_not_found []
  |> add_extension ident_out_of_memory []
  |> add_extension ident_stack_overflow []
  |> add_extension ident_sys_blocked_io []
  |> add_extension ident_sys_error [type_string,
       Jkind.Sort.Const.value]
  |> add_extension ident_undefined_recursive_module
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int]),
       Jkind.Sort.Const.value]

let add_simd_stable_extension_types add_type env =
  let _, add_type = mk_add_type add_type in
  env
  |> add_type ident_int8x16 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors
  |> add_type ident_int16x8 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors
  |> add_type ident_int32x4 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors
  |> add_type ident_int64x2 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors
  |> add_type ident_float32x4 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors
  |> add_type ident_float64x2 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors
  |> add_type ident_int8x32 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors
  |> add_type ident_int16x16 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors
  |> add_type ident_int32x8 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors
  |> add_type ident_int64x4 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors
  |> add_type ident_float32x8 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors
  |> add_type ident_float64x4 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors

let add_simd_beta_extension_types _add_type env = env

let add_simd_alpha_extension_types add_type env =
  let _, add_type = mk_add_type add_type in
  env
  |> add_type ident_int8x64 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors
  |> add_type ident_int16x32 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors
  |> add_type ident_int32x16 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors
  |> add_type ident_int64x8 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors
  |> add_type ident_float32x16 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors
  |> add_type ident_float64x8 ~jkind:Jkind.Const.Builtin.immutable_data
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors

let add_small_number_extension_types add_type env =
  let _, add_type = mk_add_type add_type in
  env
  |> add_type ident_float32 ~jkind:Jkind.Const.Builtin.immutable_data
       ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_float32

let add_small_number_beta_extension_types add_type env =
  let _, add_type = mk_add_type add_type in
  env
  |> add_type ident_int8 ~jkind:Jkind.Const.Builtin.immediate
       ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_int8
  |> add_type ident_int16 ~jkind:Jkind.Const.Builtin.immediate
       ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_int16



let or_null_argument_sort = Jkind.Sort.Const.value

let or_null_kind tvar =
  let cstrs =
    [ cstr ident_null [];
      cstr ident_this [unrestricted tvar or_null_argument_sort]]
  in
  Type_variant (cstrs, Variant_with_null, None)

let or_null_jkind param =
  Jkind.Const.Builtin.value_or_null_mod_everything
  |> Jkind.of_builtin ~why:(Primitive ident_or_null)
  |> Jkind.add_with_bounds
    ~modality:Mode.Modality.Value.Const.id
    ~type_expr:param
  |> Jkind.mark_best

let add_or_null add_type env =
  let add_type1 = mk_add_type1 add_type in
  env
  |> add_type1 ident_or_null
  ~variance:Variance.covariant
  ~separability:Separability.Ind
  (* CR layouts v3: [or_null] is separable only if the argument type
     is non-float. The current separability system can't track that.
     We also want to allow [float or_null] despite it being non-separable.

     For now, we mark the type argument as [Separability.Ind] to permit
     the most argument types, and forbid arrays from accepting [or_null]s.
     In the future, we will track separability in the jkind system. *)
  ~kind:or_null_kind
  ~param_jkind:(Jkind.for_or_null_argument ident_or_null)
  ~jkind:or_null_jkind

let builtin_values =
  List.map (fun id -> (Ident.name id, id)) all_predef_exns

let builtin_idents = List.rev !builtin_idents
