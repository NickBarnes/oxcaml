(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Int_replace_polymorphic_compare
open X86_ast
open X86_proc
open Amd64_simd_instrs

let bprintf = Printf.bprintf

let string_of_datatype = function
  | VEC128 -> "XMMWORD"
  | VEC256 -> "YMMWORD"
  | VEC512 -> "ZMMWORD"
  | QWORD -> "QWORD"
  | NONE -> assert false
  | REAL4 -> "REAL4"
  | REAL8 -> "REAL8"
  | BYTE -> "BYTE"
  | WORD -> "WORD"
  | DWORD -> "DWORD"
  | NEAR -> "NEAR"
  | PROC -> "PROC"

let string_of_datatype_ptr = function
  | VEC128 -> "XMMWORD PTR "
  | VEC256 -> "YMMWORD PTR "
  | VEC512 -> "ZMMWORD PTR "
  | QWORD -> "QWORD PTR "
  | NONE -> ""
  | REAL4 -> "REAL4 PTR "
  | REAL8 -> "REAL8 PTR "
  | BYTE -> "BYTE PTR "
  | WORD -> "WORD PTR "
  | DWORD -> "DWORD PTR "
  | NEAR -> "NEAR PTR "
  | PROC -> "PROC PTR "

let arg_mem b { arch; typ; idx; scale; base; sym; displ } =
  let string_of_register =
    match arch with X86 -> string_of_reg32 | X64 -> string_of_reg64
  in
  Buffer.add_string b (string_of_datatype_ptr typ);
  Buffer.add_char b '[';
  (match sym with None -> () | Some s -> Buffer.add_string b s);
  if scale <> 0
  then (
    if Option.is_some sym then Buffer.add_char b '+';
    Buffer.add_string b (string_of_register idx);
    if scale <> 1 then bprintf b "*%d" scale);
  (match base with
  | None -> ()
  | Some r ->
    assert (scale > 0);
    Buffer.add_char b '+';
    Buffer.add_string b (string_of_register r));
  if displ > 0
  then bprintf b "+%d" displ
  else if displ < 0
  then bprintf b "%d" displ;
  Buffer.add_char b ']'

let arg b = function
  | Sym s -> bprintf b "OFFSET %s" s
  | Imm n
    when Int64.compare n 0x7FFF_FFFFL <= 0
         && Int64.compare n (-0x8000_0000L) >= 0 ->
    bprintf b "%Ld" n
  | Imm int -> bprintf b "0%LxH" int (* force ml64 to use mov reg, imm64 *)
  | Reg8L x -> Buffer.add_string b (string_of_reg8l x)
  | Reg8H x -> Buffer.add_string b (string_of_reg8h x)
  | Reg16 x -> Buffer.add_string b (string_of_reg16 x)
  | Reg32 x -> Buffer.add_string b (string_of_reg32 x)
  | Reg64 x -> Buffer.add_string b (string_of_reg64 x)
  | Regf x -> Buffer.add_string b (string_of_regf x)
  (* We don't need to specify RIP on Win64, since EXTERN will provide the list
     of external symbols that need this addressing mode, and MASM will
     automatically use RIP addressing when needed. *)
  | Mem64_RIP (typ, s, displ) ->
    bprintf b "%s%s" (string_of_datatype_ptr typ) s;
    if displ > 0
    then bprintf b "+%d" displ
    else if displ < 0
    then bprintf b "%d" displ
  | Mem addr -> arg_mem b addr

let rec cst b = function
  | (ConstLabel _ | ConstLabelOffset _ | Const _ | ConstThis) as c -> scst b c
  | ConstAdd (c1, c2) -> bprintf b "%a + %a" scst c1 scst c2
  | ConstSub (c1, c2) -> bprintf b "%a - %a" scst c1 scst c2

and scst b = function
  | ConstThis -> Buffer.add_string b "THIS BYTE"
  | ConstLabel l -> Buffer.add_string b l
  | ConstLabelOffset (l, o) ->
    Buffer.add_string b l;
    if o > 0 then bprintf b "+%d" o else if o < 0 then bprintf b "%d" o
  | Const n
    when Int64.compare n 0x7FFF_FFFFL <= 0
         && Int64.compare n (-0x8000_0000L) >= 0 ->
    Buffer.add_string b (Int64.to_string n)
  | Const n -> bprintf b "0%LxH" n
  | ConstAdd (c1, c2) -> bprintf b "(%a + %a)" scst c1 scst c2
  | ConstSub (c1, c2) -> bprintf b "(%a - %a)" scst c1 scst c2

let i0 b s = bprintf b "\t%s" s

let i1 b s x = bprintf b "\t%s\t%a" s arg x

let i2 b s x y = bprintf b "\t%s\t%a, %a" s arg y arg x

let i3 b s x y z = bprintf b "\t%s\t%a, %a, %a" s arg x arg y arg z

let i4 b s x y z w = bprintf b "\t%s\t%a, %a, %a, %a" s arg x arg y arg z arg w

let i1_call_jmp b s = function
  | Sym x -> bprintf b "\t%s\t%s" s x
  | x -> i1 b s x

let print_instr b = function
  | ADD (arg1, arg2) -> i2 b "add" arg1 arg2
  | AND (arg1, arg2) -> i2 b "and" arg1 arg2
  | BSF (arg1, arg2) -> i2 b "bsf" arg1 arg2
  | BSR (arg1, arg2) -> i2 b "bsr" arg1 arg2
  | BSWAP arg -> i1 b "bswap" arg
  | CALL arg -> i1_call_jmp b "call" arg
  | CDQ -> i0 b "cdq"
  | CLDEMOTE arg -> i1 b "cldemote" arg
  | CMOV (c, arg1, arg2) -> i2 b ("cmov" ^ string_of_condition c) arg1 arg2
  | CMP (arg1, arg2) -> i2 b "cmp" arg1 arg2
  | CQO -> i0 b "cqo"
  | DEC arg -> i1 b "dec" arg
  | HLT -> assert false
  | IDIV arg -> i1 b "idiv" arg
  | IMUL (arg, None) -> i1 b "imul" arg
  | IMUL (arg1, Some arg2) -> i2 b "imul" arg1 arg2
  | MUL arg -> i1 b "mul" arg
  | INC arg -> i1 b "inc" arg
  | J (c, arg) -> i1_call_jmp b ("j" ^ string_of_condition c) arg
  | JMP arg -> i1_call_jmp b "jmp" arg
  | LEA (arg1, arg2) -> i2 b "lea" arg1 arg2
  | LOCK_CMPXCHG (arg1, arg2) -> i2 b "lock cmpxchg" arg1 arg2
  | LOCK_XADD (arg1, arg2) -> i2 b "lock xadd" arg1 arg2
  | LOCK_ADD (arg1, arg2) -> i2 b "lock add" arg1 arg2
  | LOCK_SUB (arg1, arg2) -> i2 b "lock sub" arg1 arg2
  | LOCK_AND (arg1, arg2) -> i2 b "lock and" arg1 arg2
  | LOCK_OR (arg1, arg2) -> i2 b "lock or" arg1 arg2
  | LOCK_XOR (arg1, arg2) -> i2 b "lock xor" arg1 arg2
  | LEAVE -> i0 b "leave"
  | MOV ((Imm n as arg1), Reg64 r)
    when Int64.compare n 0x8000_0000L >= 0 && Int64.compare n 0xFFFF_FFFFL <= 0
    ->
    (* Work-around a bug in ml64. Use a mov to the corresponding 32-bit lower
       register when the constant fits in 32-bit. The associated higher 32-bit
       register will be zeroed. *)
    i2 b "mov" arg1 (Reg32 r)
  | MOV (arg1, arg2) -> i2 b "mov" arg1 arg2
  | MOVSX (arg1, arg2) -> i2 b "movsx" arg1 arg2
  | MOVSXD (arg1, arg2) -> i2 b "movsxd" arg1 arg2
  | MOVZX (arg1, arg2) -> i2 b "movzx" arg1 arg2
  | NEG arg -> i1 b "neg" arg
  | NOP -> i0 b "nop"
  | OR (arg1, arg2) -> i2 b "or" arg1 arg2
  | PAUSE -> i0 b "pause"
  | POP arg -> i1 b "pop" arg
  | POPCNT (arg1, arg2) -> i2 b "popcnt" arg1 arg2
  | PREFETCH (is_write, hint, arg1) -> (
    match is_write, hint with
    | true, T0 -> i1 b "prefetchw" arg1
    | true, (T1 | T2 | Nta) -> i1 b "prefetchwt1" arg1
    | false, (T0 | T1 | T2 | Nta) ->
      i1 b ("prefetch" ^ string_of_prefetch_temporal_locality_hint hint) arg1)
  | PUSH arg -> i1 b "push" arg
  | RDTSC -> i0 b "rdtsc"
  | RDPMC -> i0 b "rdpmc"
  | LFENCE -> i0 b "lfence"
  | SFENCE -> i0 b "sfence"
  | MFENCE -> i0 b "mfence"
  | RET -> i0 b "ret"
  | SAL (arg1, arg2) -> i2 b "sal" arg1 arg2
  | SAR (arg1, arg2) -> i2 b "sar" arg1 arg2
  | SET (c, arg) -> i1 b ("set" ^ string_of_condition c) arg
  | SHR (arg1, arg2) -> i2 b "shr" arg1 arg2
  | SUB (arg1, arg2) -> i2 b "sub" arg1 arg2
  | TEST (arg1, arg2) -> i2 b "test" arg1 arg2
  | XCHG (arg1, arg2) -> i2 b "xchg" arg1 arg2
  | XOR (arg1, arg2) -> i2 b "xor" arg1 arg2
  | LZCNT (arg1, arg2) -> i2 b "lzcnt" arg1 arg2
  | TZCNT (arg1, arg2) -> i2 b "tzcnt" arg1 arg2
  | SIMD (instr, args) -> (
    match instr.id, args with
    (* The assembler won't accept these mnemonics directly. *)
    | Cmpps, [| imm; arg1; arg2 |] ->
      i2 b ("cmp" ^ string_of_float_condition_imm imm ^ "ps") arg1 arg2
    | Cmppd, [| imm; arg1; arg2 |] ->
      i2 b ("cmp" ^ string_of_float_condition_imm imm ^ "pd") arg1 arg2
    | Cmpss, [| imm; arg1; arg2 |] ->
      i2 b ("cmp" ^ string_of_float_condition_imm imm ^ "ss") arg1 arg2
    | Cmpsd, [| imm; arg1; arg2 |] ->
      i2 b ("cmp" ^ string_of_float_condition_imm imm ^ "sd") arg1 arg2
    (* The assembler needs a suffix to disambiguate the memory argument. *)
    | Crc32_r64_r64m64, [| arg1; arg2 |] -> i2 b "crc32q" arg1 arg2
    | Cvtsi2sd_X_r64m64, [| arg1; arg2 |] -> i2 b "cvtsi2sdq" arg1 arg2
    | Cvtsi2ss_X_r64m64, [| arg1; arg2 |] -> i2 b "cvtsi2ssq" arg1 arg2
    | Vcvtsi2sd_X_X_r64m64, [| arg1; arg2; arg3 |] ->
      i3 b "vcvtsi2sdq" arg1 arg2 arg3
    | Vcvtsi2ss_X_X_r64m64, [| arg1; arg2; arg3 |] ->
      i3 b "vcvtsi2ssq" arg1 arg2 arg3
    (* All other simd instructions. *)
    | _, [| arg1; arg2 |] -> i2 b instr.mnemonic arg1 arg2
    | _, [| arg1; arg2; arg3 |] -> i3 b instr.mnemonic arg1 arg2 arg3
    | _, [| arg1; arg2; arg3; arg4 |] -> i4 b instr.mnemonic arg1 arg2 arg3 arg4
    | _, _ ->
      Misc.fatal_errorf "unexpected instruction layout for %s (%d args)"
        instr.mnemonic (Array.length args))

let print_line b = function
  | Ins instr -> print_instr b instr
  (* Warning: The MASM printing of these directives is untested.*)
  | Directive dir -> Asm_targets.Asm_directives.Directive.print b dir

let generate_asm oc lines =
  let b = Buffer.create 10000 in
  List.iter
    (fun i ->
      Buffer.clear b;
      print_line b i;
      Buffer.add_char b '\n';
      Buffer.output_buffer oc b)
    lines;
  output_string oc "\tEND\n"
