(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]
(* Description of the ARM processor in 64-bit mode *)

open! Int_replace_polymorphic_compare

open Misc
open Reg
open Arch

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Integer register map:
    x0 - x15              general purpose (caller-save)
    x16, x17              temporaries (used by call veeners)
    x18                   platform register (reserved)
    x19 - x25             general purpose (callee-save)
    x26                   trap pointer
    x27                   alloc pointer
    x28                   domain state pointer
    x29                   frame pointer
    x30                   return address
    sp / xzr              stack pointer / zero register
   Floating-point register map:
    d0 - d7               general purpose (caller-save)
    d8 - d15              general purpose (callee-save)
    d16 - d31             general purpose (caller-save)
   Vector register map:   general purpose (caller-save)
*)

let types_are_compatible left right =
  match left.typ, right.typ with
  | (Int | Val | Addr), (Int | Val | Addr)
  | Float, Float -> true
  | Float32, Float32 -> true
  | Vec128, Vec128 -> true
  | Valx2,Valx2 -> true
  | (Vec256 | Vec512), _ | _, (Vec256 | Vec512) ->
    Misc.fatal_error "arm64: got 256/512 bit vector"
  | (Int | Val | Addr | Float | Float32 | Vec128 | Valx2), _ -> false

(* Representation of hard registers by pseudo-registers *)

let hard_reg_gen typ =
  let reg_class = Reg_class.of_machtype typ in
  let first = Reg_class.first_available_register reg_class in
  let n = Reg_class.num_registers reg_class in
  let v = Array.make n Reg.dummy in
  for i = 0 to n - 1 do
    v.(i) <- Reg.create_at_location typ (Reg(first + i))
  done;
  v

let hard_int_reg = hard_reg_gen Int
let hard_float_reg = hard_reg_gen Float

let hard_vec128_reg = Array.map (fun r -> {r with typ = Vec128}) hard_float_reg
let hard_float32_reg = Array.map (fun r -> {r with typ = Float32}) hard_float_reg

let all_phys_regs =
  Array.concat [hard_int_reg; hard_float_reg; hard_float32_reg; hard_vec128_reg; ]

let precolored_regs =
  let phys_regs = Reg.set_of_array all_phys_regs in
  fun () -> phys_regs

let phys_reg ty n =
  match (ty : Cmm.machtype_component) with
  | Int | Addr | Val -> hard_int_reg.(n)
  | Float -> hard_float_reg.(n - 100)
  | Float32 -> hard_float32_reg.(n - 100)
  | Vec128 | Valx2 -> hard_vec128_reg.(n - 100)
  | Vec256 | Vec512 -> Misc.fatal_error "arm64: got 256/512 bit vector"
let reg_x8 = phys_reg Int 8

let stack_slot slot ty =
  Reg.create_at_location ty (Stack slot)

(* Calling conventions *)

let size_domainstate_args = 64 * size_int

let loc_int last_int make_stack int ofs =
  if !int <= last_int then begin
    let l = phys_reg Int !int in
    incr int; l
  end else begin
    ofs := Misc.align !ofs size_int;
    let l = stack_slot (make_stack !ofs) Int in
    ofs := !ofs + size_int; l
  end

let loc_float_gen kind size last_float make_stack float ofs =
  if !float <= last_float then begin
    let l = phys_reg kind !float in
    incr float; l
  end else begin
    ofs := Misc.align !ofs size;
    let l = stack_slot (make_stack !ofs) kind in
    ofs := !ofs + size; l
  end

let loc_float = loc_float_gen Float Arch.size_float
(* float32 slots still take up a full word *)
let loc_float32 = loc_float_gen Float32 Arch.size_float
let loc_vec128 = loc_float_gen Vec128 Arch.size_vec128

let loc_int32 last_int make_stack int ofs =
  if !int <= last_int then begin
    let l = phys_reg Int !int in
    incr int; l
  end else begin
    let l = stack_slot (make_stack !ofs) Int in
    ofs := !ofs + (if macosx then 4 else 8);
    l
  end

let calling_conventions
    first_int last_int first_float last_float make_stack first_stack arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref first_stack in
  for i = 0 to Array.length arg - 1 do
    match (arg.(i) : Cmm.machtype_component) with
    | Val | Int | Addr ->
        loc.(i) <- loc_int last_int make_stack int ofs
    | Float ->
        loc.(i) <- loc_float last_float make_stack float ofs
    | Vec128 ->
        loc.(i) <- loc_vec128 last_float make_stack float ofs
    | Vec256 | Vec512 ->
        Misc.fatal_error "arm64: got 256/512 bit vector"
    | Float32 ->
        loc.(i) <- loc_float32 last_float make_stack float ofs
    | Valx2 ->
        Misc.fatal_error "Unexpected machtype_component Valx2"
  done;
  (* CR mslater: (SIMD) will need to be 32/64 if vec256/512 are used. *)
  (loc, Misc.align (max 0 !ofs) 16)  (* keep stack 16-aligned *)

let incoming ofs =
  if ofs >= 0
  then Incoming ofs
  else Domainstate (ofs + size_domainstate_args)
let outgoing ofs =
  if ofs >= 0
  then Outgoing ofs
  else Domainstate (ofs + size_domainstate_args)
let not_supported _ofs = fatal_error "Proc.loc_results: cannot call"

(* OCaml calling convention:
     first integer args in r0...r15
     first float args in d0...d15
     remaining args in domain area, then on stack.
   Return values in r0...r15 or d0...d15. *)

let max_arguments_for_tailcalls = 16 (* in regs *) + 64 (* in domain state *)

let last_int_register = if macosx then 7 else 15

let loc_arguments arg =
  calling_conventions 0 last_int_register 100 115
                      outgoing (- size_domainstate_args) arg
let loc_parameters arg =
  let (loc, _) =
    calling_conventions 0 last_int_register 100 115
                        incoming (- size_domainstate_args) arg
  in
  loc
let loc_results_call res =
  calling_conventions 0 last_int_register 100 115 outgoing (- size_domainstate_args) res
let loc_results_return res =
  let (loc, _) =
    calling_conventions 0 last_int_register 100 115 incoming (- size_domainstate_args) res
  in
  loc

(* C calling convention:
     first integer args in r0...r7
     first float args in d0...d7
     remaining args on stack.
   macOS/iOS peculiarity: int32 arguments passed on stack occupy 4 bytes,
   while the AAPCS64 says 8 bytes.
   Return values in r0...r1 or d0. *)

let external_calling_conventions
    first_int last_int first_float last_float make_stack ty_args =
  let loc = Array.make (List.length ty_args) [| Reg.dummy |] in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  List.iteri (fun i ty_arg ->
    begin match (ty_arg : Cmm.exttype) with
    | XInt | XInt64 ->
        loc.(i) <- [| loc_int last_int make_stack int ofs |]
    | XInt32 | XInt16 | XInt8 ->
        loc.(i) <- [| loc_int32 last_int make_stack int ofs |]
    | XFloat ->
        loc.(i) <- [| loc_float last_float make_stack float ofs |]
    | XVec128 ->
        loc.(i) <- [| loc_vec128 last_float make_stack float ofs |]
    | XVec256 | XVec512 ->
        Misc.fatal_error "XVec256 and XVec512 not supported on ARM64"
    | XFloat32 ->
        loc.(i) <- [| loc_float32 last_float make_stack float ofs |]
    end)
    ty_args;
  (loc, Misc.align !ofs 16, Cmm.Align_16) (* keep stack 16-aligned *)

let loc_external_arguments ty_args =
  external_calling_conventions 0 7 100 107 outgoing ty_args

let loc_external_results res =
  let (loc, _) = calling_conventions 0 1 100 101 not_supported 0 res in loc

let loc_exn_bucket = phys_reg Int 0

let stack_ptr_dwarf_register_number = 31

let domainstate_ptr_dwarf_register_number = 28

(* Registers destroyed by operations *)

let destroyed_at_c_noalloc_call =
  (* x19-x28, d8-d15 preserved *)
  let int_regs_destroyed_at_c_noalloc_call =
    [| 0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15 |]
  in
  let float_regs_destroyed_at_c_noalloc_call =
    [|100;101;102;103;104;105;106;107;
      116;117;118;119;120;121;122;123;
      124;125;126;127;128;129;130;131|]
  in
  let vec128_regs_destroyed_at_c_noalloc_call =
    (* Registers v8-v15 must be preserved by a callee across
       subroutine calls; the remaining registers (v0-v7, v16-v31) do
       not need to be preserved (or should be preserved by the
       caller). Additionally, only the bottom 64 bits of each value
       stored in v8-v15 need to be preserved [8]; it is the
       responsibility of the caller to preserve larger values.

       https://github.com/ARM-software/abi-aa/blob/
       3952cfbfd2404c442bb6bb6f59ff7b923ab0c148/
       aapcs64/aapcs64.rst?plain=1#L837
    *)
    hard_vec128_reg
  in
  Array.concat [
    Array.map (phys_reg Int) int_regs_destroyed_at_c_noalloc_call;
    Array.map (phys_reg Float) float_regs_destroyed_at_c_noalloc_call;
    Array.map (phys_reg Float32) float_regs_destroyed_at_c_noalloc_call;
    vec128_regs_destroyed_at_c_noalloc_call;
  ]

(* CSE needs to know that all versions of neon are destroyed. *)
let destroy_neon_reg n =
  [| phys_reg Float (100 + n); phys_reg Float32 (100 + n);
     phys_reg Vec128 (100 + n); |]

let destroy_neon_reg7 = destroy_neon_reg 7

let destroyed_at_raise = all_phys_regs

let destroyed_at_reloadretaddr = [| |]

let destroyed_at_pushtrap = [| |]

let destroyed_at_alloc_or_poll = [| reg_x8 |]

let destroyed_at_basic (basic : Cfg_intf.S.basic) =
  match basic with
  | Reloadretaddr ->
    destroyed_at_reloadretaddr
  | Pushtrap _ ->
    destroyed_at_pushtrap
  | Op Poll -> destroyed_at_alloc_or_poll
  | Op (Alloc _) ->
    destroyed_at_alloc_or_poll
  | Op(Load {memory_chunk = Single { reg = Float64 }; _ }
      | Store(Single { reg = Float64 }, _, _))
    -> destroy_neon_reg7
  | Op (Load {memory_chunk=Single {reg=Float32}; _ })
  | Op (Store (Single {reg=Float32}, _, _))
  | Op (Load
          {memory_chunk=(Byte_unsigned|Byte_signed|Sixteen_unsigned|
                         Sixteen_signed|Thirtytwo_unsigned|Thirtytwo_signed|
                         Word_int|Word_val|Double|Onetwentyeight_unaligned|
                         Onetwentyeight_aligned);
           _ })
  | Op (Store
          ((Byte_unsigned|Byte_signed|Sixteen_unsigned|Sixteen_signed|
            Thirtytwo_unsigned|Thirtytwo_signed|Word_int|Word_val|Double|
            Onetwentyeight_unaligned|Onetwentyeight_aligned),
           _, _))
    -> [||]
  | Op (Static_cast
          (Int_of_float _ | Float_of_int _
          | Float_of_float32|Float32_of_float))
    -> [||]
  | Op (Static_cast
          (V128_of_scalar _|Scalar_of_v128 _))
  | Op (Intop Ipopcnt) ->
      if !Arch.feat_cssc then
        [||]
      else
        destroy_neon_reg7
  | Op (Intop (Iadd  | Isub | Imul | Idiv|Imod|Iand|Ior|Ixor|Ilsl
              |Ilsr|Iasr|Imulh _|Iclz _|Ictz _|Icomp _))
  | Op (Specific _
        | Move | Spill | Reload
        | Floatop _
        | Csel _
        | Const_int _
        | Const_float32 _ | Const_float _
        | Const_symbol _ | Const_vec128 _
        | Stackoffset _
        | Intop_imm _ | Intop_atomic _
        | Name_for_debugger _ | Probe_is_enabled _ | Opaque | Pause
        | Begin_region | End_region | Dls_get)
  | Poptrap _ | Prologue
  | Op (Reinterpret_cast (Int_of_value | Value_of_int | Float_of_float32 |
                          Float32_of_float | Float_of_int64 | Int64_of_float |
                          Float32_of_int32 | Int32_of_float32 |
                          V128_of_vec Vec128))
    -> [||]
  | Stack_check _ -> assert false (* not supported *)
  | Op (Const_vec256 _ | Const_vec512 _)
  | Op (Load
          {memory_chunk=(Twofiftysix_aligned|Twofiftysix_unaligned|
                         Fivetwelve_aligned|Fivetwelve_unaligned);
           _ })
  | Op (Store
          ((Twofiftysix_aligned|Twofiftysix_unaligned|
            Fivetwelve_aligned|Fivetwelve_unaligned),
            _, _))
  | Op (Reinterpret_cast (V128_of_vec (Vec256 | Vec512) |
                          V256_of_vec _ | V512_of_vec _))
  | Op (Static_cast (V256_of_scalar _ | Scalar_of_v256 _ |
                     V512_of_scalar _ | Scalar_of_v512 _))
    -> Misc.fatal_error "arm64: got 256/512 bit vector"

(* note: keep this function in sync with `is_destruction_point` below. *)
let destroyed_at_terminator (terminator : Cfg_intf.S.terminator) =
  match terminator with
  | Never -> assert false
  | Call {op = Indirect | Direct _; _} ->
    all_phys_regs
  | Always _ | Parity_test _ | Truth_test _ | Float_test _
  | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
  | Tailcall_func _ | Prim {op = Probe _; _} ->
    [||]
  | Call_no_return { func_symbol = _; alloc; ty_res = _; ty_args = _; stack_ofs; _ }
  | Prim {op  = External { func_symbol = _; alloc; ty_res = _; ty_args = _; stack_ofs; _ }; _} ->
    if alloc || stack_ofs > 0 then all_phys_regs else destroyed_at_c_noalloc_call

(* CR-soon xclerc for xclerc: consider having more destruction points.
   We current return `true` when `destroyed_at_terminator` returns
   `all_phys_regs`; we could also return `true` when `destroyed_at_terminator`
   returns `destroyed_at_c_call` for instance. *)
(* note: keep this function in sync with `destroyed_at_terminator` above. *)
let is_destruction_point ~(more_destruction_points : bool) (terminator : Cfg_intf.S.terminator) =
  match terminator with
  | Never -> assert false
  | Call {op = Indirect | Direct _; _} ->
    true
  | Always _ | Parity_test _ | Truth_test _ | Float_test _
  | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
  | Tailcall_func _ | Prim {op = Probe _; _} ->
    false
  | Call_no_return { func_symbol = _; alloc; ty_res = _; ty_args = _; stack_ofs = _; _}
  | Prim {op  = External { func_symbol = _; alloc; ty_res = _; ty_args = _; stack_ofs = _; _}; _} ->
    if more_destruction_points then
      true
    else
    if alloc then true else false

(* Layout of the stack *)

let initial_stack_offset ~num_stack_slots ~contains_calls =
  Stack_class.Tbl.total_size_in_bytes num_stack_slots
  + if contains_calls then 8 else 0

let trap_frame_size_in_bytes = 16

let frame_size ~stack_offset ~contains_calls ~num_stack_slots =
  let sz =
    stack_offset + initial_stack_offset ~num_stack_slots ~contains_calls
  in
  Misc.align sz 16

let frame_required ~fun_contains_calls ~fun_num_stack_slots =
  fun_contains_calls || Stack_class.Tbl.exists fun_num_stack_slots ~f:(fun _stack_class num -> num > 0)

let prologue_required ~fun_contains_calls ~fun_num_stack_slots =
  frame_required ~fun_contains_calls ~fun_num_stack_slots

type slot_offset =
  | Bytes_relative_to_stack_pointer of int
  | Bytes_relative_to_domainstate_pointer of int
[@@ocaml.warning "-37"]

let slot_offset (loc : Reg.stack_location) ~stack_class ~stack_offset
      ~fun_contains_calls ~fun_num_stack_slots =
  match loc with
    Incoming n ->
      assert (n >= 0);
      let frame_size =
        frame_size ~stack_offset ~contains_calls:fun_contains_calls
          ~num_stack_slots:fun_num_stack_slots
      in
      Bytes_relative_to_stack_pointer (frame_size + n)
  | Local n ->
      let offset =
        stack_offset +
        Stack_class.Tbl.offset_in_bytes fun_num_stack_slots ~stack_class ~slot:n
      in
      Bytes_relative_to_stack_pointer offset
  | Outgoing n ->
      assert (n >= 0);
      Bytes_relative_to_stack_pointer n
  | Domainstate n ->
      Bytes_relative_to_domainstate_pointer (
        n + Domainstate.(idx_of_field Domain_extra_params) * 8)

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " " ^
                 (String.concat " " (Misc.debug_prefix_map_flags ())) ^
                 " -o " ^ Filename.quote outfile ^ " " ^ Filename.quote infile)

let has_three_operand_float_ops () = false

let operation_supported : Cmm.operation -> bool = function
  | Cprefetch _ | Catomic _
  | Creinterpret_cast (V128_of_vec (Vec256 | Vec512) |
                       V256_of_vec _ | V512_of_vec _)
  | Cstatic_cast (V256_of_scalar _ | Scalar_of_v256 _ |
                  V512_of_scalar _ | Scalar_of_v512 _) ->
    false
  | Cpopcnt
  | Cnegf Float32 | Cabsf Float32 | Caddf Float32
  | Csubf Float32 | Cmulf Float32 | Cdivf Float32
  | Cpackf32
  | Cclz _ | Cctz _ | Cbswap _
  | Capply _ | Cextcall _ | Cload _ | Calloc _ | Cstore _
  | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccmpi _ | Caddv | Cadda | Ccmpa _
  | Cnegf Float64 | Cabsf Float64 | Caddf Float64
  | Csubf Float64 | Cmulf Float64 | Cdivf Float64
  | Ccmpf _
  | Ccsel _
  | Craise _
  | Cprobe _ | Cprobe_is_enabled _ | Copaque | Cpause
  | Cbeginregion | Cendregion | Ctuple_field _
  | Cdls_get
  | Cpoll
  | Creinterpret_cast (Int_of_value | Value_of_int |
                       Int64_of_float | Float_of_int64 |
                       Float32_of_float | Float_of_float32 |
                       Float32_of_int32 | Int32_of_float32 |
                       V128_of_vec Vec128)
  | Cstatic_cast (Float_of_float32 | Float32_of_float |
                  Int_of_float Float32 | Float_of_int Float32 |
                  Float_of_int Float64 | Int_of_float Float64 |
                  V128_of_scalar _ | Scalar_of_v128 _) ->
    true

let expression_supported : Cmm.expression -> bool = function
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_vec128 _ | Cconst_symbol _  | Cvar _ | Clet _ | Cphantom_let _
  | Ctuple _ | Cop _ | Csequence _ | Cifthenelse _ | Cswitch _ | Ccatch _
  | Cexit _ -> true
  | Cconst_vec256 _ | Cconst_vec512 _ -> false

let trap_size_in_bytes = 16
