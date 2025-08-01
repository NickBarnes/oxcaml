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

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@warning "-66"]

type machtype_component = Cmx_format.machtype_component =
  | Val
  | Addr
  | Int
  | Float
  | Vec128
  | Vec256
  | Vec512
  | Float32
  | Valx2

type machtype = machtype_component array

(* Note: To_cmm_expr.translate_apply0 relies on non-void [machtype_component]s
   being singleton arrays. *)
(* CR mshinwell/xclerc: Maybe this should be a variant type instead, or an
   option. *)
let typ_void = ([||] : machtype_component array)

let typ_val = [| Val |]

let typ_addr = [| Addr |]

let typ_int = [| Int |]

let typ_float = [| Float |]

let typ_float32 = [| Float32 |]

let typ_vec128 = [| Vec128 |]

let typ_vec256 = [| Vec256 |]

let typ_vec512 = [| Vec512 |]

(** [machtype_component]s are partially ordered as follows:

      Addr     Float32     Float     Vec128     Vec256     Vec512   Valx2
       ^
       |
      Val
       ^
       |
      Int

  In particular, [Addr] must be above [Val], to ensure that if there is
  a join point between a code path yielding [Addr] and one yielding [Val]
  then the result is treated as a derived pointer into the heap (i.e. [Addr]).
  (Such a result may not be live across any call site or a fatal compiler
  error will result.)
  The order is used only in selection, Valx2 is generated after selection.
*)

let lub_component comp1 comp2 =
  match comp1, comp2 with
  | Int, Int -> Int
  | Int, Val -> Val
  | Int, Addr -> Addr
  | Val, Int -> Val
  | Val, Val -> Val
  | Val, Addr -> Addr
  | Addr, Int -> Addr
  | Addr, Addr -> Addr
  | Addr, Val -> Addr
  | Float, Float -> Float
  | Float32, Float32 -> Float32
  | Vec128, Vec128 -> Vec128
  | Vec256, Vec256 -> Vec256
  | Vec512, Vec512 -> Vec512
  | (Int | Addr | Val), (Float | Float32 | Vec128 | Vec256 | Vec512)
  | (Float | Float32 | Vec128 | Vec256 | Vec512), (Int | Addr | Val)
  | (Float | Float32 | Vec256 | Vec512), (Vec128 | Vec256 | Vec512)
  | (Vec128 | Vec256 | Vec512), (Float | Float32 | Vec256 | Vec512)
  | Float32, Float
  | Float, Float32 ->
    Printf.eprintf "%d %d\n%!" (Obj.magic comp1) (Obj.magic comp2);
    (* Float unboxing code must be sure to avoid this case. *)
    assert false
  | Valx2, _ | _, Valx2 ->
    Misc.fatal_errorf "Unexpected machtype_component Valx2"

let ge_component comp1 comp2 =
  match comp1, comp2 with
  | Int, Int -> true
  | Int, Addr -> false
  | Int, Val -> false
  | Val, Int -> true
  | Val, Val -> true
  | Val, Addr -> false
  | Addr, Int -> true
  | Addr, Addr -> true
  | Addr, Val -> true
  | Float, Float -> true
  | Float32, Float32 -> true
  | Vec128, Vec128 -> true
  | Vec256, Vec256 -> true
  | Vec512, Vec512 -> true
  | (Int | Addr | Val), (Float | Float32 | Vec128 | Vec256 | Vec512)
  | (Float | Float32 | Vec128 | Vec256 | Vec512), (Int | Addr | Val)
  | (Float | Float32 | Vec256 | Vec512), (Vec128 | Vec256 | Vec512)
  | (Vec128 | Vec256 | Vec512), (Float | Float32 | Vec256 | Vec512)
  | Float32, Float
  | Float, Float32 ->
    Printf.eprintf "GE: %d %d\n%!" (Obj.magic comp1) (Obj.magic comp2);
    assert false
  | Valx2, _ | _, Valx2 ->
    Misc.fatal_error "Unexpected machtype_component Valx2"

type exttype =
  | XInt
  | XInt8
  | XInt16
  | XInt32
  | XInt64
  | XFloat32
  | XFloat
  | XVec128
  | XVec256
  | XVec512

let machtype_of_exttype = function
  | XInt -> typ_int
  | XInt8 -> typ_int
  | XInt16 -> typ_int
  | XInt32 -> typ_int
  | XInt64 -> typ_int
  | XFloat -> typ_float
  | XFloat32 -> typ_float32
  | XVec128 -> typ_vec128
  | XVec256 -> typ_vec256
  | XVec512 -> typ_vec512

let machtype_of_exttype_list xtl =
  Array.concat (List.map machtype_of_exttype xtl)

type stack_align =
  | Align_16
  | Align_32
  | Align_64

let equal_stack_align left right =
  match left, right with
  | Align_16, Align_16 | Align_32, Align_32 | Align_64, Align_64 -> true
  | (Align_16 | Align_32 | Align_64), _ -> false

type integer_comparison = Lambda.integer_comparison =
  | Ceq
  | Cne
  | Clt
  | Cgt
  | Cle
  | Cge

let negate_integer_comparison = Lambda.negate_integer_comparison

let swap_integer_comparison = Lambda.swap_integer_comparison

(* With floats [not (x < y)] is not the same as [x >= y] due to NaNs, so we
   provide additional comparisons to represent the negations.*)
type float_comparison = Lambda.float_comparison =
  | CFeq
  | CFneq
  | CFlt
  | CFnlt
  | CFgt
  | CFngt
  | CFle
  | CFnle
  | CFge
  | CFnge

let negate_float_comparison = Lambda.negate_float_comparison

let swap_float_comparison = Lambda.swap_float_comparison

type label = Label.t

let new_label = Label.new_label

let set_label = Label.set_label

let cur_label = Label.cur_label

type static_label = Lambda.static_label

type exit_label =
  | Return_lbl
  | Lbl of static_label

type prefetch_temporal_locality_hint =
  | Nonlocal
  | Low
  | Moderate
  | High

type atomic_op =
  | Fetch_and_add
  | Add
  | Sub
  | Land
  | Lor
  | Lxor
  | Exchange
  | Compare_set
  | Compare_exchange

type atomic_bitwidth =
  | Thirtytwo
  | Sixtyfour
  | Word

type effects =
  | No_effects
  | Arbitrary_effects

type coeffects =
  | No_coeffects
  | Has_coeffects

type phantom_defining_expr =
  | Cphantom_const_int of Targetint.t
  | Cphantom_const_symbol of string
  | Cphantom_var of Backend_var.t
  | Cphantom_offset_var of
      { var : Backend_var.t;
        offset_in_words : int
      }
  | Cphantom_read_field of
      { var : Backend_var.t;
        field : int
      }
  | Cphantom_read_symbol_field of
      { sym : string;
        field : int
      }
  | Cphantom_block of
      { tag : int;
        fields : Backend_var.t list
      }

type trywith_shared_label = int

type trap_action =
  | Push of trywith_shared_label
  | Pop of trywith_shared_label

type bswap_bitwidth =
  | Sixteen
  | Thirtytwo
  | Sixtyfour

type initialization_or_assignment =
  | Initialization
  | Assignment

type vec128_type =
  | Int8x16
  | Int16x8
  | Int32x4
  | Int64x2
  | Float32x4
  | Float64x2

type vec256_type =
  | Int8x32
  | Int16x16
  | Int32x8
  | Int64x4
  | Float32x8
  | Float64x4

type vec512_type =
  | Int8x64
  | Int16x32
  | Int32x16
  | Int64x8
  | Float32x16
  | Float64x8

type float_width =
  | Float64
  | Float32

type vector_width =
  | Vec128
  | Vec256
  | Vec512

type memory_chunk =
  | Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word_int
  | Word_val
  | Single of { reg : float_width }
  | Double
  | Onetwentyeight_unaligned
  | Onetwentyeight_aligned
  | Twofiftysix_unaligned
  | Twofiftysix_aligned
  | Fivetwelve_unaligned
  | Fivetwelve_aligned

type reinterpret_cast =
  | Int_of_value
  | Value_of_int
  | Float_of_float32
  | Float32_of_float
  | Float_of_int64
  | Int64_of_float
  | Float32_of_int32
  | Int32_of_float32
  | V128_of_vec of vector_width
  | V256_of_vec of vector_width
  | V512_of_vec of vector_width

type static_cast =
  | Float_of_int of float_width
  | Int_of_float of float_width
  | Float_of_float32
  | Float32_of_float
  | V128_of_scalar of vec128_type
  | Scalar_of_v128 of vec128_type
  | V256_of_scalar of vec256_type
  | Scalar_of_v256 of vec256_type
  | V512_of_scalar of vec512_type
  | Scalar_of_v512 of vec512_type

module Alloc_mode = struct
  type t =
    | Heap
    | Local

  let equal t1 t2 =
    match t1, t2 with
    | Heap, Heap -> true
    | Local, Local -> true
    | Heap, Local -> false
    | Local, Heap -> false

  let print ppf t =
    match t with
    | Heap -> Format.fprintf ppf "Heap"
    | Local -> Format.fprintf ppf "Local"

  let is_local = function Heap -> false | Local -> true

  let is_heap = function Heap -> true | Local -> false
end

type alloc_block_kind =
  | Alloc_block_kind_other
  | Alloc_block_kind_closure
  | Alloc_block_kind_float
  | Alloc_block_kind_float32
  | Alloc_block_kind_vec128
  | Alloc_block_kind_vec256
  | Alloc_block_kind_vec512
  | Alloc_block_kind_boxed_int of Primitive.boxed_integer
  | Alloc_block_kind_float_array
  | Alloc_block_kind_float32_u_array
  | Alloc_block_kind_int32_u_array
  | Alloc_block_kind_int64_u_array
  | Alloc_block_kind_vec128_u_array
  | Alloc_block_kind_vec256_u_array
  | Alloc_block_kind_vec512_u_array

type alloc_dbginfo_item =
  { alloc_words : int;
    alloc_block_kind : alloc_block_kind;
    alloc_dbg : Debuginfo.t
  }

type alloc_dbginfo = alloc_dbginfo_item list

type operation =
  | Capply of machtype * Lambda.region_close
  | Cextcall of
      { func : string;
        ty : machtype;
        ty_args : exttype list;
        alloc : bool;
        builtin : bool;
        returns : bool;
        effects : effects;
        coeffects : coeffects
      }
  | Cload of
      { memory_chunk : memory_chunk;
        mutability : Asttypes.mutable_flag;
        is_atomic : bool
      }
  | Calloc of Alloc_mode.t * alloc_block_kind
  | Cstore of memory_chunk * initialization_or_assignment
  | Caddi
  | Csubi
  | Cmuli
  | Cmulhi of { signed : bool }
  | Cdivi
  | Cmodi
  | Cand
  | Cor
  | Cxor
  | Clsl
  | Clsr
  | Casr
  | Cbswap of { bitwidth : bswap_bitwidth }
  | Ccsel of machtype
  | Cclz of { arg_is_non_zero : bool }
  | Cctz of { arg_is_non_zero : bool }
  | Cpopcnt
  | Cprefetch of
      { is_write : bool;
        locality : prefetch_temporal_locality_hint
      }
  | Catomic of
      { op : atomic_op;
        size : atomic_bitwidth
      }
  | Ccmpi of integer_comparison
  | Caddv
  | Cadda
  | Ccmpa of integer_comparison
  | Cnegf of float_width
  | Cabsf of float_width
  | Caddf of float_width
  | Csubf of float_width
  | Cmulf of float_width
  | Cdivf of float_width
  | Cpackf32
  | Creinterpret_cast of reinterpret_cast
  | Cstatic_cast of static_cast
  | Ccmpf of float_width * float_comparison
  | Craise of Lambda.raise_kind
  | Cprobe of
      { name : string;
        handler_code_sym : string;
        enabled_at_init : bool
      }
  | Cprobe_is_enabled of { name : string }
  | Copaque
  | Cbeginregion
  | Cendregion
  | Ctuple_field of int * machtype array
  | Cdls_get
  | Cpoll
  | Cpause

type is_global =
  | Global
  | Local

let equal_is_global g g' =
  match g, g' with
  | Local, Local | Global, Global -> true
  | Local, Global | Global, Local -> false

type symbol =
  { sym_name : string;
    sym_global : is_global
  }

type vec128_bits =
  { word0 : int64; (* Least significant *)
    word1 : int64
  }

type vec256_bits =
  { word0 : int64; (* Least significant *)
    word1 : int64;
    word2 : int64;
    word3 : int64
  }

type vec512_bits =
  { word0 : int64; (* Least significant *)
    word1 : int64;
    word2 : int64;
    word3 : int64;
    word4 : int64;
    word5 : int64;
    word6 : int64;
    word7 : int64
  }

let global_symbol sym_name = { sym_name; sym_global = Global }

type ccatch_flag =
  | Normal
  | Recursive
  | Exn_handler

type expression =
  | Cconst_int of int * Debuginfo.t
  | Cconst_natint of nativeint * Debuginfo.t
  | Cconst_float32 of float * Debuginfo.t
  | Cconst_float of float * Debuginfo.t
  | Cconst_vec128 of vec128_bits * Debuginfo.t
  | Cconst_vec256 of vec256_bits * Debuginfo.t
  | Cconst_vec512 of vec512_bits * Debuginfo.t
  | Cconst_symbol of symbol * Debuginfo.t
  | Cvar of Backend_var.t
  | Clet of Backend_var.With_provenance.t * expression * expression
  | Cphantom_let of
      Backend_var.With_provenance.t * phantom_defining_expr option * expression
  | Ctuple of expression list
  | Cop of operation * expression list * Debuginfo.t
  | Csequence of expression * expression
  | Cifthenelse of
      expression
      * Debuginfo.t
      * expression
      * Debuginfo.t
      * expression
      * Debuginfo.t
  | Cswitch of
      expression * int array * (expression * Debuginfo.t) array * Debuginfo.t
  | Ccatch of
      ccatch_flag
      * (static_label
        * (Backend_var.With_provenance.t * machtype) list
        * expression
        * Debuginfo.t
        * bool (* is_cold *))
        list
      * expression
  | Cexit of exit_label * expression list * trap_action list

type codegen_option =
  | Reduce_code_size
  | No_CSE
  | Use_linscan_regalloc
  | Assume_zero_alloc of
      { strict : bool;
        never_returns_normally : bool;
        never_raises : bool;
        loc : Location.t
      }
  | Check_zero_alloc of
      { strict : bool;
        loc : Location.t;
        custom_error_msg : string option
      }

type fundecl =
  { fun_name : symbol;
    fun_args : (Backend_var.With_provenance.t * machtype) list;
    fun_body : expression;
    fun_codegen_options : codegen_option list;
    fun_poll : Lambda.poll_attribute;
    fun_dbg : Debuginfo.t;
    fun_ret_type : machtype
  }

type data_item =
  | Cdefine_symbol of symbol
  | Cint8 of int
  | Cint16 of int
  | Cint32 of nativeint
  | Cint of nativeint
  | Csingle of float
  | Cdouble of float
  | Cvec128 of vec128_bits
  | Cvec256 of vec256_bits
  | Cvec512 of vec512_bits
  | Csymbol_address of symbol
  | Csymbol_offset of symbol * int
  | Cstring of string
  | Cskip of int
  | Calign of int

type phrase =
  | Cfunction of fundecl
  | Cdata of data_item list

let ccatch (i, ids, e1, e2, dbg, is_cold) =
  Ccatch (Normal, [i, ids, e2, dbg, is_cold], e1)

let ctrywith (body, lbl, id, extra_args, handler, dbg) =
  Ccatch
    (Exn_handler, [lbl, (id, typ_val) :: extra_args, handler, dbg, false], body)

let reset () = Label.reset ()

let iter_shallow_tail f = function
  | Clet (_, _, body) | Cphantom_let (_, _, body) ->
    f body;
    true
  | Cifthenelse (_cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg) ->
    f ifso;
    f ifnot;
    true
  | Csequence (_e1, e2) ->
    f e2;
    true
  | Cswitch (_e, _tbl, el, _dbg') ->
    Array.iter (fun (e, _dbg) -> f e) el;
    true
  | Ccatch (_flag, handlers, body) ->
    List.iter (fun (_, _, h, _dbg, _) -> f h) handlers;
    f body;
    true
  | Cexit _ | Cop (Craise _, _, _) -> true
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _ | Cconst_symbol _
  | Cvar _ | Ctuple _
  | Cop
      ( ( Calloc _ | Caddi | Csubi | Cmuli | Cdivi | Cmodi | Cand | Cor | Cxor
        | Clsl | Clsr | Casr | Cpopcnt | Caddv | Cadda | Cpackf32 | Copaque
        | Cbeginregion | Cendregion | Cdls_get | Cpoll | Cpause
        | Capply (_, _)
        | Cextcall _ | Cload _
        | Cstore (_, _)
        | Cmulhi _ | Cbswap _ | Ccsel _ | Cclz _ | Cctz _ | Cprefetch _
        | Catomic _ | Ccmpi _ | Ccmpa _ | Cnegf _ | Cabsf _ | Caddf _ | Csubf _
        | Cmulf _ | Cdivf _ | Creinterpret_cast _ | Cstatic_cast _
        | Ccmpf (_, _)
        | Cprobe _ | Cprobe_is_enabled _
        | Ctuple_field (_, _) ),
        _,
        _ ) ->
    false

let map_shallow_tail f = function
  | Clet (id, exp, body) -> Clet (id, exp, f body)
  | Cphantom_let (id, exp, body) -> Cphantom_let (id, exp, f body)
  | Cifthenelse (cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg) ->
    Cifthenelse (cond, ifso_dbg, f ifso, ifnot_dbg, f ifnot, dbg)
  | Csequence (e1, e2) -> Csequence (e1, f e2)
  | Cswitch (e, tbl, el, dbg') ->
    Cswitch (e, tbl, Array.map (fun (e, dbg) -> f e, dbg) el, dbg')
  | Ccatch (flag, handlers, body) ->
    let map_h (n, ids, handler, dbg, is_cold) =
      n, ids, f handler, dbg, is_cold
    in
    Ccatch (flag, List.map map_h handlers, f body)
  | (Cexit _ | Cop (Craise _, _, _)) as cmm -> cmm
  | ( Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _ | Cconst_symbol _
    | Cvar _ | Ctuple _
    | Cop
        ( ( Calloc _ | Caddi | Csubi | Cmuli | Cdivi | Cmodi | Cand | Cor | Cxor
          | Clsl | Clsr | Casr | Cpopcnt | Caddv | Cadda | Cpackf32 | Copaque
          | Cbeginregion | Cendregion | Cdls_get | Cpoll | Cpause
          | Capply (_, _)
          | Cextcall _ | Cload _
          | Cstore (_, _)
          | Cmulhi _ | Cbswap _ | Ccsel _ | Cclz _ | Cctz _ | Cprefetch _
          | Catomic _ | Ccmpi _ | Ccmpa _ | Cnegf _ | Cabsf _ | Caddf _
          | Csubf _ | Cmulf _ | Cdivf _ | Creinterpret_cast _ | Cstatic_cast _
          | Ccmpf (_, _)
          | Cprobe _ | Cprobe_is_enabled _
          | Ctuple_field (_, _) ),
          _,
          _ ) ) as cmm ->
    cmm

let map_tail f =
  let rec loop = function
    | ( Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
      | Cconst_symbol _ | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _
      | Cvar _ | Ctuple _ | Cop _ ) as c ->
      f c
    | ( Cexit _
      | Clet (_, _, _)
      | Cphantom_let (_, _, _)
      | Csequence (_, _)
      | Cifthenelse (_, _, _, _, _, _)
      | Cswitch (_, _, _, _)
      | Ccatch (_, _, _) ) as cmm ->
      map_shallow_tail loop cmm
  in
  loop

let iter_shallow f = function
  | Clet (_id, e1, e2) ->
    f e1;
    f e2
  | Cphantom_let (_id, _de, e) -> f e
  | Ctuple el -> List.iter f el
  | Cop (_op, el, _dbg) -> List.iter f el
  | Csequence (e1, e2) ->
    f e1;
    f e2
  | Cifthenelse (cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg) ->
    f cond;
    f ifso;
    f ifnot
  | Cswitch (_e, _ia, ea, _dbg) -> Array.iter (fun (e, _) -> f e) ea
  | Ccatch (_f, hl, body) ->
    let iter_h (_n, _ids, handler, _dbg, _is_cold) = f handler in
    List.iter iter_h hl;
    f body
  | Cexit (_n, el, _traps) -> List.iter f el
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _ | Cconst_symbol _
  | Cvar _ ->
    ()

let map_shallow f = function
  | Clet (id, e1, e2) -> Clet (id, f e1, f e2)
  | Cphantom_let (id, de, e) -> Cphantom_let (id, de, f e)
  | Ctuple el -> Ctuple (List.map f el)
  | Cop (op, el, dbg) -> Cop (op, List.map f el, dbg)
  | Csequence (e1, e2) -> Csequence (f e1, f e2)
  | Cifthenelse (cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg) ->
    Cifthenelse (f cond, ifso_dbg, f ifso, ifnot_dbg, f ifnot, dbg)
  | Cswitch (e, ia, ea, dbg) ->
    Cswitch (e, ia, Array.map (fun (e, dbg) -> f e, dbg) ea, dbg)
  | Ccatch (flag, hl, body) ->
    let map_h (n, ids, handler, dbg, is_cold) =
      n, ids, f handler, dbg, is_cold
    in
    Ccatch (flag, List.map map_h hl, f body)
  | Cexit (n, el, traps) -> Cexit (n, List.map f el, traps)
  | ( Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _ | Cconst_symbol _
    | Cvar _ ) as c ->
    c

let rank_machtype_component : machtype_component -> int = function
  | Val -> 0
  | Addr -> 1
  | Int -> 2
  | Float -> 3
  | Vec128 -> 4
  | Vec256 -> 5
  | Vec512 -> 6
  | Float32 -> 7
  | Valx2 -> 8

let compare_machtype_component
    ((Val | Addr | Int | Float | Vec128 | Vec256 | Vec512 | Float32 | Valx2) as
     left :
      machtype_component) (right : machtype_component) =
  rank_machtype_component left - rank_machtype_component right

let equal_machtype_component
    ((Val | Addr | Int | Float | Vec128 | Vec256 | Vec512 | Float32 | Valx2) as
     left :
      machtype_component) (right : machtype_component) =
  rank_machtype_component left = rank_machtype_component right

let equal_exttype
    (( XInt | XInt8 | XInt16 | XInt32 | XInt64 | XFloat32 | XFloat | XVec128
     | XVec256 | XVec512 ) as left) right =
  (* we can use polymorphic compare as long as exttype is all constant
     constructors *)
  Stdlib.( = ) left right

let equal_vec128_type v1 v2 =
  match v1, v2 with
  | Int8x16, Int8x16 -> true
  | Int16x8, Int16x8 -> true
  | Int32x4, Int32x4 -> true
  | Int64x2, Int64x2 -> true
  | Float32x4, Float32x4 -> true
  | Float64x2, Float64x2 -> true
  | (Int8x16 | Int16x8 | Int32x4 | Int64x2 | Float32x4 | Float64x2), _ -> false

let equal_vec256_type v1 v2 =
  match v1, v2 with
  | Int8x32, Int8x32 -> true
  | Int16x16, Int16x16 -> true
  | Int32x8, Int32x8 -> true
  | Int64x4, Int64x4 -> true
  | Float32x8, Float32x8 -> true
  | Float64x4, Float64x4 -> true
  | (Int8x32 | Int16x16 | Int32x8 | Int64x4 | Float32x8 | Float64x4), _ -> false

let equal_vec512_type v1 v2 =
  match v1, v2 with
  | Int8x64, Int8x64 -> true
  | Int16x32, Int16x32 -> true
  | Int32x16, Int32x16 -> true
  | Int64x8, Int64x8 -> true
  | Float32x16, Float32x16 -> true
  | Float64x8, Float64x8 -> true
  | (Int8x64 | Int16x32 | Int32x16 | Int64x8 | Float32x16 | Float64x8), _ ->
    false

let equal_float_width left right =
  match left, right with
  | Float64, Float64 -> true
  | Float32, Float32 -> true
  | (Float32 | Float64), _ -> false

let equal_vector_width left right =
  match left, right with
  | Vec128, Vec128 -> true
  | Vec256, Vec256 -> true
  | Vec512, Vec512 -> true
  | (Vec128 | Vec256 | Vec512), _ -> false

let equal_reinterpret_cast (left : reinterpret_cast) (right : reinterpret_cast)
    =
  match left, right with
  | Int_of_value, Int_of_value -> true
  | Value_of_int, Value_of_int -> true
  | Float_of_float32, Float_of_float32 -> true
  | Float32_of_float, Float32_of_float -> true
  | Float_of_int64, Float_of_int64 -> true
  | Int64_of_float, Int64_of_float -> true
  | Float32_of_int32, Float32_of_int32 -> true
  | Int32_of_float32, Int32_of_float32 -> true
  | V128_of_vec w1, V128_of_vec w2
  | V256_of_vec w1, V256_of_vec w2
  | V512_of_vec w1, V512_of_vec w2 ->
    equal_vector_width w1 w2
  | ( ( Int_of_value | Value_of_int | Float_of_float32 | Float32_of_float
      | Float_of_int64 | Int64_of_float | Float32_of_int32 | Int32_of_float32
      | V128_of_vec _ | V256_of_vec _ | V512_of_vec _ ),
      _ ) ->
    false

let equal_static_cast (left : static_cast) (right : static_cast) =
  match left, right with
  | Float32_of_float, Float32_of_float -> true
  | Float_of_float32, Float_of_float32 -> true
  | Float_of_int f1, Float_of_int f2 -> equal_float_width f1 f2
  | Int_of_float f1, Int_of_float f2 -> equal_float_width f1 f2
  | Scalar_of_v128 v1, Scalar_of_v128 v2 -> equal_vec128_type v1 v2
  | V128_of_scalar v1, V128_of_scalar v2 -> equal_vec128_type v1 v2
  | Scalar_of_v256 v1, Scalar_of_v256 v2 -> equal_vec256_type v1 v2
  | V256_of_scalar v1, V256_of_scalar v2 -> equal_vec256_type v1 v2
  | Scalar_of_v512 v1, Scalar_of_v512 v2 -> equal_vec512_type v1 v2
  | V512_of_scalar v1, V512_of_scalar v2 -> equal_vec512_type v1 v2
  | ( ( Float32_of_float | Float_of_float32 | Float_of_int _ | Int_of_float _
      | Scalar_of_v128 _ | V128_of_scalar _ | Scalar_of_v256 _
      | V256_of_scalar _ | Scalar_of_v512 _ | V512_of_scalar _ ),
      _ ) ->
    false

let equal_float_comparison left right =
  match left, right with
  | CFeq, CFeq -> true
  | CFneq, CFneq -> true
  | CFlt, CFlt -> true
  | CFnlt, CFnlt -> true
  | CFgt, CFgt -> true
  | CFngt, CFngt -> true
  | CFle, CFle -> true
  | CFnle, CFnle -> true
  | CFge, CFge -> true
  | CFnge, CFnge -> true
  | CFeq, (CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge)
  | CFneq, (CFeq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge)
  | CFlt, (CFeq | CFneq | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge)
  | CFnlt, (CFeq | CFneq | CFlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge)
  | CFgt, (CFeq | CFneq | CFlt | CFnlt | CFngt | CFle | CFnle | CFge | CFnge)
  | CFngt, (CFeq | CFneq | CFlt | CFnlt | CFgt | CFle | CFnle | CFge | CFnge)
  | CFle, (CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFnle | CFge | CFnge)
  | CFnle, (CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFge | CFnge)
  | CFge, (CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFnge)
  | CFnge, (CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge) ->
    false

let equal_memory_chunk left right =
  match left, right with
  | Byte_unsigned, Byte_unsigned -> true
  | Byte_signed, Byte_signed -> true
  | Sixteen_unsigned, Sixteen_unsigned -> true
  | Sixteen_signed, Sixteen_signed -> true
  | Thirtytwo_unsigned, Thirtytwo_unsigned -> true
  | Thirtytwo_signed, Thirtytwo_signed -> true
  | Word_int, Word_int -> true
  | Word_val, Word_val -> true
  | Single { reg = regl }, Single { reg = regr } -> equal_float_width regl regr
  | Double, Double -> true
  | Onetwentyeight_unaligned, Onetwentyeight_unaligned -> true
  | Onetwentyeight_aligned, Onetwentyeight_aligned -> true
  | Twofiftysix_unaligned, Twofiftysix_unaligned -> true
  | Twofiftysix_aligned, Twofiftysix_aligned -> true
  | Fivetwelve_unaligned, Fivetwelve_unaligned -> true
  | Fivetwelve_aligned, Fivetwelve_aligned -> true
  | ( Byte_unsigned,
      ( Byte_signed | Sixteen_unsigned | Sixteen_signed | Thirtytwo_unsigned
      | Thirtytwo_signed | Word_int | Word_val | Single _ | Double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ) )
  | ( Byte_signed,
      ( Byte_unsigned | Sixteen_unsigned | Sixteen_signed | Thirtytwo_unsigned
      | Thirtytwo_signed | Word_int | Word_val | Single _ | Double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ) )
  | ( Sixteen_unsigned,
      ( Byte_unsigned | Byte_signed | Sixteen_signed | Thirtytwo_unsigned
      | Thirtytwo_signed | Word_int | Word_val | Single _ | Double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ) )
  | ( Sixteen_signed,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Thirtytwo_unsigned
      | Thirtytwo_signed | Word_int | Word_val | Single _ | Double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ) )
  | ( Thirtytwo_unsigned,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_signed | Word_int | Word_val | Single _ | Double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ) )
  | ( Thirtytwo_signed,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Word_int | Word_val | Single _ | Double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ) )
  | ( Word_int,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Word_val | Single _ | Double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ) )
  | ( Word_val,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Single _ | Double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ) )
  | ( Double,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Single _
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ) )
  | ( Onetwentyeight_unaligned,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Single _
      | Double | Onetwentyeight_aligned | Twofiftysix_unaligned
      | Twofiftysix_aligned | Fivetwelve_unaligned | Fivetwelve_aligned ) )
  | ( Onetwentyeight_aligned,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Single _
      | Double | Onetwentyeight_unaligned | Twofiftysix_unaligned
      | Twofiftysix_aligned | Fivetwelve_unaligned | Fivetwelve_aligned ) )
  | ( Twofiftysix_unaligned,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Single _
      | Double | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_aligned | Fivetwelve_unaligned | Fivetwelve_aligned ) )
  | ( Twofiftysix_aligned,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Single _
      | Double | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Fivetwelve_unaligned | Fivetwelve_aligned ) )
  | ( Fivetwelve_unaligned,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Single _
      | Double | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_aligned ) )
  | ( Fivetwelve_aligned,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Single _
      | Double | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned ) )
  | ( Single _,
      ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ) ) ->
    false

let equal_integer_comparison left right =
  match left, right with
  | Ceq, Ceq -> true
  | Cne, Cne -> true
  | Clt, Clt -> true
  | Cgt, Cgt -> true
  | Cle, Cle -> true
  | Cge, Cge -> true
  | Ceq, (Cne | Clt | Cgt | Cle | Cge)
  | Cne, (Ceq | Clt | Cgt | Cle | Cge)
  | Clt, (Ceq | Cne | Cgt | Cle | Cge)
  | Cgt, (Ceq | Cne | Clt | Cle | Cge)
  | Cle, (Ceq | Cne | Clt | Cgt | Cge)
  | Cge, (Ceq | Cne | Clt | Cgt | Cle) ->
    false

let caml_flambda2_invalid = "caml_flambda2_invalid"

let is_val (m : machtype_component) =
  match m with
  | Val -> true
  | Addr | Int | Float | Vec128 | Vec256 | Vec512 | Float32 | Valx2 -> false

let is_exn_handler (flag : ccatch_flag) =
  match flag with Exn_handler -> true | Normal | Recursive -> false
