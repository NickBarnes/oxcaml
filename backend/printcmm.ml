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

(* Pretty-printing of C-- code *)
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
open Format
open Cmm
module V = Backend_var
module VP = Backend_var.With_provenance
open Location_tracker_formatter

let with_location_mapping ?label ~dbg ppf f =
  with_location_mapping ?label ~loc:(Debuginfo.to_location dbg) ppf f

let ccatch_flag ppf = function
  | Normal -> ()
  | Recursive -> fprintf ppf " rec"
  | Exn_handler -> fprintf ppf " exn"

let machtype_component ppf (ty : machtype_component) =
  match ty with
  | Val -> fprintf ppf "val"
  | Addr -> fprintf ppf "addr"
  | Int -> fprintf ppf "int"
  | Float -> fprintf ppf "float"
  | Vec128 -> fprintf ppf "vec128"
  | Vec256 -> fprintf ppf "vec256"
  | Vec512 -> fprintf ppf "vec512"
  | Float32 -> fprintf ppf "float32"
  | Valx2 -> fprintf ppf "valx2"

let machtype ppf mty =
  match Array.length mty with
  | 0 -> fprintf ppf "unit"
  | n ->
    machtype_component ppf mty.(0);
    for i = 1 to n - 1 do
      fprintf ppf "*%a" machtype_component mty.(i)
    done

let exttype ppf = function
  | XInt -> fprintf ppf "int"
  | XInt8 -> fprintf ppf "int8"
  | XInt16 -> fprintf ppf "int16"
  | XInt32 -> fprintf ppf "int32"
  | XInt64 -> fprintf ppf "int64"
  | XFloat -> fprintf ppf "float"
  | XFloat32 -> fprintf ppf "float32"
  | XVec128 -> fprintf ppf "vec128"
  | XVec256 -> fprintf ppf "vec256"
  | XVec512 -> fprintf ppf "vec512"

let extcall_signature ppf (ty_res, ty_args) =
  (match ty_args with
  | [] -> ()
  | ty_arg1 :: ty_args ->
    exttype ppf ty_arg1;
    List.iter (fun ty -> fprintf ppf ",%a" exttype ty) ty_args);
  match ty_res with
  | None -> fprintf ppf "->."
  | Some ty_res -> fprintf ppf "->%a" machtype ty_res

let is_global ppf = function
  | Global -> fprintf ppf "G"
  | Local -> fprintf ppf "L"

let symbol ppf s = fprintf ppf "%a:\"%s\"" is_global s.sym_global s.sym_name

let integer_comparison = function
  | Ceq -> "=="
  | Cne -> "!="
  | Clt -> "<"
  | Cle -> "<="
  | Cgt -> ">"
  | Cge -> ">="

let float_comparison = function
  | CFeq -> "=="
  | CFneq -> "!="
  | CFlt -> "<"
  | CFnlt -> "!<"
  | CFle -> "<="
  | CFnle -> "!<="
  | CFgt -> ">"
  | CFngt -> "!>"
  | CFge -> ">="
  | CFnge -> "!>="

let vector_width = function
  | Vec128 -> "vec128"
  | Vec256 -> "vec256"
  | Vec512 -> "vec512"

let vec128_name = function
  | Int8x16 -> "int8x16"
  | Int16x8 -> "int16x8"
  | Int32x4 -> "int32x4"
  | Int64x2 -> "int64x2"
  | Float32x4 -> "float32x4"
  | Float64x2 -> "float64x2"

let vec256_name = function
  | Int8x32 -> "int8x32"
  | Int16x16 -> "int16x16"
  | Int32x8 -> "int32x8"
  | Int64x4 -> "int64x4"
  | Float32x8 -> "float32x8"
  | Float64x4 -> "float64x4"

let vec512_name = function
  | Int8x64 -> "int8x64"
  | Int16x32 -> "int16x32"
  | Int32x16 -> "int32x16"
  | Int64x8 -> "int64x8"
  | Float32x16 -> "float32x16"
  | Float64x8 -> "float64x8"

let chunk = function
  | Byte_unsigned -> "unsigned int8"
  | Byte_signed -> "signed int8"
  | Sixteen_unsigned -> "unsigned int16"
  | Sixteen_signed -> "signed int16"
  | Thirtytwo_unsigned -> "unsigned int32"
  | Thirtytwo_signed -> "signed int32"
  | Onetwentyeight_unaligned -> "unaligned vec128"
  | Onetwentyeight_aligned -> "aligned vec128"
  | Twofiftysix_unaligned -> "unaligned vec256"
  | Twofiftysix_aligned -> "aligned vec256"
  | Fivetwelve_unaligned -> "unaligned vec512"
  | Fivetwelve_aligned -> "aligned vec512"
  | Word_int -> "int"
  | Word_val -> "val"
  | Single { reg = Float64 } -> "float32_as_float64"
  | Single { reg = Float32 } -> "float32"
  | Double -> "float64"

let atomic_bitwidth : Cmm.atomic_bitwidth -> string = function
  | Word -> "int"
  | Thirtytwo -> "int32"
  | Sixtyfour -> "int64"

let temporal_locality = function
  | Nonlocal -> "nonlocal"
  | Low -> "low"
  | Moderate -> "moderate"
  | High -> "high"

let atomic_op = function
  | Fetch_and_add -> "xadd"
  | Add -> "+="
  | Sub -> "-="
  | Land -> "&="
  | Lor -> "|="
  | Lxor -> "^="
  | Exchange -> "exchange"
  | Compare_set -> "compare_set"
  | Compare_exchange -> "compare_exchange"

let phantom_defining_expr ppf defining_expr =
  match defining_expr with
  | Cphantom_const_int i -> Targetint.print ppf i
  | Cphantom_const_symbol sym -> Format.pp_print_string ppf sym
  | Cphantom_var var -> V.print ppf var
  | Cphantom_offset_var { var; offset_in_words } ->
    Format.fprintf ppf "%a+(%d)" V.print var offset_in_words
  | Cphantom_read_field { var; field } ->
    Format.fprintf ppf "%a[%d]" V.print var field
  | Cphantom_read_symbol_field { sym; field } ->
    Format.fprintf ppf "%s[%d]" sym field
  | Cphantom_block { tag; fields } ->
    Format.fprintf ppf "[%d: " tag;
    List.iter (fun field -> Format.fprintf ppf "%a; " V.print field) fields;
    Format.fprintf ppf "]"

let phantom_defining_expr_opt ppf defining_expr =
  match defining_expr with
  | None -> Format.pp_print_string ppf "()"
  | Some defining_expr -> phantom_defining_expr ppf defining_expr

let location d = if not !Clflags.locations then "" else Debuginfo.to_string d

let exit_label ppf = function
  | Return_lbl -> fprintf ppf "*return*"
  | Lbl lbl -> fprintf ppf "%d" lbl

let trap_action ppf ta =
  match ta with
  | Push i -> fprintf ppf "push(%d)" i
  | Pop i -> fprintf ppf "pop(%d)" i

let trap_action_list ppf traps =
  match traps with
  | [] -> ()
  | t :: rest ->
    fprintf ppf "<%a" trap_action t;
    List.iter (fun t -> fprintf ppf " %a" trap_action t) rest;
    fprintf ppf ">"

let to_string msg =
  let b = Buffer.create 17 in
  let ppf = Format.formatter_of_buffer b in
  Format.kfprintf
    (fun ppf ->
      Format.pp_print_flush ppf ();
      Buffer.contents b)
    ppf msg

let reinterpret_cast : Cmm.reinterpret_cast -> string = function
  | V128_of_vec w -> Printf.sprintf "%s as vec128" (vector_width w)
  | V256_of_vec w -> Printf.sprintf "%s as vec256" (vector_width w)
  | V512_of_vec w -> Printf.sprintf "%s as vec512" (vector_width w)
  | Value_of_int -> "int as value"
  | Int_of_value -> "value as int"
  | Float32_of_float -> "float as float32"
  | Float_of_float32 -> "float32 as float"
  | Float_of_int64 -> "int64 as float"
  | Int64_of_float -> "float as int64"
  | Float32_of_int32 -> "int32 as float32"
  | Int32_of_float32 -> "float32 as int32"

let static_cast : Cmm.static_cast -> string = function
  | Int_of_float Float64 -> "float->int"
  | Float_of_int Float64 -> "int->float"
  | Int_of_float Float32 -> "float32->int"
  | Float_of_int Float32 -> "int->float32"
  | Float32_of_float -> "float->float32"
  | Float_of_float32 -> "float32->float"
  | Scalar_of_v128 ty -> Printf.sprintf "%s->scalar" (vec128_name ty)
  | V128_of_scalar ty -> Printf.sprintf "scalar->%s" (vec128_name ty)
  | Scalar_of_v256 ty -> Printf.sprintf "%s->scalar" (vec256_name ty)
  | V256_of_scalar ty -> Printf.sprintf "scalar->%s" (vec256_name ty)
  | Scalar_of_v512 ty -> Printf.sprintf "%s->scalar" (vec512_name ty)
  | V512_of_scalar ty -> Printf.sprintf "scalar->%s" (vec512_name ty)

let operation d = function
  | Capply (_ty, _) -> "app" ^ location d
  | Cextcall { func = lbl; _ } ->
    Printf.sprintf "extcall \"%s\"%s" lbl (location d)
  | Cload { memory_chunk; mutability; is_atomic } -> (
    let atomic = if is_atomic then "_atomic" else "" in
    match mutability with
    | Asttypes.Immutable ->
      Printf.sprintf "load%s %s" atomic (chunk memory_chunk)
    | Asttypes.Mutable ->
      Printf.sprintf "load_mut%s %s" atomic (chunk memory_chunk))
  | Calloc (Alloc_mode.Heap, _) -> "alloc" ^ location d
  | Calloc (Alloc_mode.Local, _) -> "alloc_local" ^ location d
  | Cstore (c, init) ->
    let init =
      match init with Initialization -> "(init)" | Assignment -> ""
    in
    Printf.sprintf "store %s%s" (chunk c) init
  | Caddi -> "+"
  | Csubi -> "-"
  | Cmuli -> "*"
  | Cmulhi { signed } -> "*h" ^ if signed then "" else "u"
  | Cdivi -> "/"
  | Cmodi -> "mod"
  | Cand -> "and"
  | Cor -> "or"
  | Cxor -> "xor"
  | Clsl -> "<<"
  | Clsr -> ">>u"
  | Casr -> ">>s"
  | Cbswap { bitwidth = Sixteen } -> "bswap_16"
  | Cbswap { bitwidth = Thirtytwo } -> "bswap_32"
  | Cbswap { bitwidth = Sixtyfour } -> "bswap_64"
  | Cclz { arg_is_non_zero } -> Printf.sprintf "clz %B" arg_is_non_zero
  | Cctz { arg_is_non_zero } -> Printf.sprintf "ctz %B" arg_is_non_zero
  | Cpopcnt -> "popcnt"
  | Ccmpi c -> integer_comparison c
  | Caddv -> "+v"
  | Cadda -> "+a"
  | Ccmpa c -> Printf.sprintf "%sa" (integer_comparison c)
  | Cnegf Float64 -> "~f"
  | Cabsf Float64 -> "absf"
  | Caddf Float64 -> "+f"
  | Csubf Float64 -> "-f"
  | Cmulf Float64 -> "*f"
  | Cdivf Float64 -> "/f"
  | Cnegf Float32 -> "~f32"
  | Cabsf Float32 -> "absf32"
  | Caddf Float32 -> "+f32"
  | Csubf Float32 -> "-f32"
  | Cmulf Float32 -> "*f32"
  | Cdivf Float32 -> "/f32"
  | Cpackf32 -> "packf32"
  | Ccsel ret_typ -> to_string "csel %a" machtype ret_typ
  | Creinterpret_cast cast -> reinterpret_cast cast
  | Cstatic_cast cast -> static_cast cast
  | Ccmpf (Float64, c) -> Printf.sprintf "%sf" (float_comparison c)
  | Ccmpf (Float32, c) -> Printf.sprintf "%sf32" (float_comparison c)
  | Craise k -> Lambda.raise_kind k ^ location d
  | Cprobe { name; handler_code_sym; enabled_at_init } ->
    Printf.sprintf "probe[%s %s%s]" name handler_code_sym
      (if enabled_at_init then " enabled_at_init" else "")
  | Cprobe_is_enabled { name } -> Printf.sprintf "probe_is_enabled[%s]" name
  | Cprefetch { is_write; locality } ->
    Printf.sprintf "prefetch is_write=%b prefetch_temporal_locality_hint=%s"
      is_write
      (temporal_locality locality)
  | Catomic { op; size = _ } -> Printf.sprintf "atomic %s" (atomic_op op)
  | Copaque -> "opaque"
  | Cbeginregion -> "beginregion"
  | Cendregion -> "endregion"
  | Ctuple_field (field, _ty) -> to_string "tuple_field %i" field
  | Cdls_get -> "dls_get"
  | Cpoll -> "poll"
  | Cpause -> "pause"

let rec expr ppf = function
  | Cconst_int (n, _dbg) -> fprintf ppf "%i" n
  | Cconst_natint (n, _dbg) -> fprintf ppf "%s" (Nativeint.to_string n)
  | Cconst_vec128 ({ word0; word1 }, _dbg) ->
    fprintf ppf "%016Lx:%016Lx" word1 word0
  | Cconst_vec256 ({ word0; word1; word2; word3 }, _dbg) ->
    fprintf ppf "%016Lx:%016Lx:%016Lx:%016Lx" word3 word2 word1 word0
  | Cconst_vec512
      ({ word0; word1; word2; word3; word4; word5; word6; word7 }, _dbg) ->
    fprintf ppf "%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx" word7
      word6 word5 word4 word3 word2 word1 word0
  | Cconst_float32 (n, _dbg) -> fprintf ppf "%Fs" n
  | Cconst_float (n, _dbg) -> fprintf ppf "%F" n
  | Cconst_symbol (s, _dbg) ->
    fprintf ppf "%a:\"%s\"" is_global s.sym_global s.sym_name
  | Cvar id -> V.print ppf id
  | Clet (id, def, (Clet (_, _, _) as body)) ->
    let print_binding id ppf def =
      fprintf ppf "@[<2>%a@ %a@]" VP.print id expr def
    in
    let rec in_part ppf = function[@warning "-4"]
      | Clet (id, def, body) ->
        fprintf ppf "@ %a" (print_binding id) def;
        in_part ppf body
      | exp -> exp
    in
    fprintf ppf "@[<2>(let@ @[<1>(%a" (print_binding id) def;
    let exp = in_part ppf body in
    fprintf ppf ")@]@ %a)@]" sequence exp
  | Clet (id, def, body) ->
    fprintf ppf "@[<2>(let@ @[<2>%a@ %a@]@ %a)@]" VP.print id expr def sequence
      body
  | Cphantom_let (var, def, (Cphantom_let (_, _, _) as body)) ->
    let print_binding var ppf def =
      fprintf ppf "@[<2>%a@ %a@]" VP.print var phantom_defining_expr_opt def
    in
    let rec in_part ppf = function[@warning "-4"]
      | Cphantom_let (var, def, body) ->
        fprintf ppf "@ %a" (print_binding var) def;
        in_part ppf body
      | exp -> exp
    in
    fprintf ppf "@[<2>(let?@ @[<1>(%a" (print_binding var) def;
    let exp = in_part ppf body in
    fprintf ppf ")@]@ %a)@]" sequence exp
  | Cphantom_let (var, def, body) ->
    fprintf ppf "@[<2>(let?@ @[<2>%a@ %a@]@ %a)@]" VP.print var
      phantom_defining_expr_opt def sequence body
  | Ctuple el ->
    let tuple ppf el =
      let first = ref true in
      List.iter
        (fun e ->
          if !first then first := false else fprintf ppf "@ ";
          expr ppf e)
        el
    in
    fprintf ppf "@[<1>[%a]@]" tuple el
  | Cop (op, el, dbg) ->
    with_location_mapping ~label:"Cop" ~dbg ppf (fun () ->
        fprintf ppf "@[<2>(%s" (operation dbg op);
        List.iter (fun e -> fprintf ppf "@ %a" expr e) el;
        (match[@warning "-4"] op with
        | Capply (mty, _) -> fprintf ppf "@ %a" machtype mty
        | Cextcall
            { ty;
              ty_args;
              alloc = _;
              func = _;
              returns;
              builtin = _;
              effects = _;
              coeffects = _
            } ->
          let ty = if returns then Some ty else None in
          fprintf ppf "@ %a" extcall_signature (ty, ty_args)
        | _ -> ());
        fprintf ppf ")@]")
  | Csequence (e1, e2) ->
    fprintf ppf "@[<2>(seq@ %a@ %a)@]" sequence e1 sequence e2
  | Cifthenelse (e1, e2_dbg, e2, e3_dbg, e3, dbg) ->
    with_location_mapping ~label:"Cifthenelse-e1" ~dbg ppf (fun () ->
        fprintf ppf "@[<2>(if@ %a@ " expr e1;
        with_location_mapping ~label:"Cifthenelse-e2" ~dbg:e2_dbg ppf (fun () ->
            fprintf ppf "%a@ " expr e2);
        with_location_mapping ~label:"Cifthenelse-e3" ~dbg:e3_dbg ppf (fun () ->
            fprintf ppf "%a" expr e3);
        fprintf ppf ")@]")
  | Cswitch (e1, index, cases, dbg) ->
    with_location_mapping ~label:"Cswitch" ~dbg ppf (fun () ->
        let print_case i ppf =
          for j = 0 to Array.length index - 1 do
            if index.(j) = i then fprintf ppf "case %i:" j
          done
        in
        let print_cases ppf =
          for i = 0 to Array.length cases - 1 do
            fprintf ppf "@ @[<2>%t@ %a@]" (print_case i) sequence
              (fst cases.(i))
          done
        in
        fprintf ppf "@[<v 0>@[<2>(switch@ %a@ @]%t)@]" expr e1 print_cases)
  | Ccatch (flag, handlers, e1) ->
    let print_handler ppf (i, ids, e2, dbg, is_cold) =
      with_location_mapping ~label:"Ccatch-handler" ~dbg ppf (fun () ->
          fprintf ppf "(%d%a)%s@ %a" i
            (fun ppf ids ->
              List.iter
                (fun (id, ty) -> fprintf ppf "@ %a: %a" VP.print id machtype ty)
                ids)
            ids
            (if is_cold then "(cold)" else "")
            sequence e2)
    in
    let print_handlers ppf l = List.iter (print_handler ppf) l in
    fprintf ppf "@[<2>(catch%a@ %a@;<1 -2>with%a)@]" ccatch_flag flag sequence
      e1 print_handlers handlers
  | Cexit (i, el, traps) ->
    fprintf ppf "@[<2>(exit%a %a" trap_action_list traps exit_label i;
    List.iter (fun e -> fprintf ppf "@ %a" expr e) el;
    fprintf ppf ")@]"

and sequence ppf = function[@warning "-4"]
  | Csequence (e1, e2) -> fprintf ppf "%a@ %a" sequence e1 sequence e2
  | e -> expression ppf e

and expression ppf e = fprintf ppf "%a" expr e

let codegen_option = function
  | Reduce_code_size -> "reduce_code_size"
  | No_CSE -> "no_cse"
  | Use_linscan_regalloc -> "linscan"
  | Assume_zero_alloc { strict; never_returns_normally; never_raises; loc = _ }
    ->
    Printf.sprintf "assume_zero_alloc_%s%s%s"
      (if strict then "_strict" else "")
      (if never_returns_normally then "_never_returns_normally" else "")
      (if never_raises then "_never_raises" else "")
  | Check_zero_alloc { strict; loc = _; custom_error_msg } ->
    Printf.sprintf "assert_zero_alloc%s%s"
      (if strict then "_strict" else "")
      (match custom_error_msg with
      | None -> ""
      | Some msg -> Printf.sprintf " customer_error_message %S" msg)

let print_codegen_options ppf l =
  List.iter (fun c -> fprintf ppf " %s" (codegen_option c)) l

let fundecl ppf f =
  let print_cases ppf cases =
    let first = ref true in
    List.iter
      (fun (id, ty) ->
        if !first then first := false else fprintf ppf "@ ";
        fprintf ppf "%a: %a" VP.print id machtype ty)
      cases
  in
  with_location_mapping ~label:"Function" ~dbg:f.fun_dbg ppf (fun () ->
      fprintf ppf "@[<1>(function%s%a@ %s@;<1 4>@[<1>(%a) : %a@]@ @[%a@] )@]@."
        (location f.fun_dbg) print_codegen_options f.fun_codegen_options
        f.fun_name.sym_name print_cases f.fun_args machtype f.fun_ret_type
        sequence f.fun_body)

let data_item ppf = function
  | Cdefine_symbol { sym_name; sym_global = Local } ->
    fprintf ppf "\"%s\":" sym_name
  | Cdefine_symbol { sym_name; sym_global = Global } ->
    fprintf ppf "global \"%s\":" sym_name
  | Cint8 n -> fprintf ppf "byte %i" n
  | Cint16 n -> fprintf ppf "int16 %i" n
  | Cint32 n -> fprintf ppf "int32 %s" (Nativeint.to_string n)
  | Cint n -> fprintf ppf "int %s" (Nativeint.to_string n)
  | Csingle f -> fprintf ppf "single %F" f
  | Cdouble f -> fprintf ppf "double %F" f
  | Cvec128 { word0; word1 } ->
    fprintf ppf "vec128 %s:%s" (Int64.to_string word1) (Int64.to_string word0)
  | Cvec256 { word0; word1; word2; word3 } ->
    fprintf ppf "vec256 %s:%s:%s:%s" (Int64.to_string word3)
      (Int64.to_string word2) (Int64.to_string word1) (Int64.to_string word0)
  | Cvec512 { word0; word1; word2; word3; word4; word5; word6; word7 } ->
    fprintf ppf "vec512 %s:%s:%s:%s:%s:%s:%s:%s" (Int64.to_string word7)
      (Int64.to_string word6) (Int64.to_string word5) (Int64.to_string word4)
      (Int64.to_string word3) (Int64.to_string word2) (Int64.to_string word1)
      (Int64.to_string word0)
  | Csymbol_address s ->
    fprintf ppf "addr %a:\"%s\"" is_global s.sym_global s.sym_name
  | Csymbol_offset (s, o) ->
    fprintf ppf "addr %a:\"%s+%d\"" is_global s.sym_global s.sym_name o
  | Cstring s -> fprintf ppf "string \"%s\"" s
  | Cskip n -> fprintf ppf "skip %i" n
  | Calign n -> fprintf ppf "align %i" n

let data ppf dl =
  let items ppf = List.iter (fun d -> fprintf ppf "@ %a" data_item d) dl in
  fprintf ppf "@[<hv 1>(data%t)@]" items

let phrase ppf = function
  | Cfunction f -> fundecl ppf f
  | Cdata dl -> data ppf dl
