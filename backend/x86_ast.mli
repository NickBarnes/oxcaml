(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Structured representation of Intel assembly language (32 and 64 bit). *)

[@@@ocaml.warning "+a-40-41-42"]

type condition =
  | L | GE     (* signed comparisons: less/greater *)
  | LE | G
  | B | AE     (* unsigned comparisons: below/above *)
  | BE | A
  | E | NE     (* equal *)
  | O | NO     (* overflow *)
  | S | NS     (* sign *)
  | P | NP     (* parity *)

type float_condition =
  | EQf
  | LTf
  | LEf
  | UNORDf
  | NEQf
  | NLTf
  | NLEf
  | ORDf

type rounding =
  | RoundUp
  | RoundDown
  | RoundNearest
  | RoundTruncate
  | RoundCurrent

type constant =
  | Const of int64
  | ConstThis
  | ConstLabel of string
  | ConstLabelOffset of string * int
  | ConstAdd of constant * constant
  | ConstSub of constant * constant

(* data_type is used mainly on memory addressing to specify
   the size of the addressed memory chunk.  It is directly
   used by the MASM emitter and indirectly by the GAS emitter
   to infer the instruction suffix. *)

type data_type =
  | NONE
  | REAL4 | REAL8 (* floating point values *)
  | BYTE | WORD | DWORD | QWORD (* integer values *)
  | VEC128 (* vector values (float & integer) *)
  | VEC256
  | VEC512
  | NEAR | PROC

type reg64 =
  | RAX | RBX | RCX | RDX | RSP | RBP | RSI | RDI
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

type reg8h =
  | AH | BH | CH | DH

type regf =
  | XMM of int
  | YMM of int
  | ZMM of int

type arch = X64 | X86

type addr =
  {
    arch: arch;
    typ: data_type;
    idx: reg64;
    scale: int;
    base: reg64 option;
    sym: string option;
    displ: int;
  }
  (** Addressing modes:
      displ + sym + base + idx * scale
      (if scale = 0, idx is ignored and base must be None)
  *)

type prefetch_temporal_locality_hint = Nta | T1 | T2 | T0

type arg =
  | Imm of int64
  (** Operand is an immediate constant integer *)

  | Sym of string
  (** Address of a symbol (absolute address except for call/jmp target
      where it is interpreted as a relative displacement *)

  | Reg8L of reg64
  | Reg8H of reg8h
  | Reg16 of reg64
  | Reg32 of reg64
  | Reg64 of reg64
  | Regf of regf

  | Mem of addr
  | Mem64_RIP of data_type * string * int

type instruction =
  | ADD of arg * arg
  | AND of arg * arg
  | BSF of arg * arg
  | BSR of arg * arg
  | BSWAP of arg
  | CALL of arg
  | CDQ
  | CLDEMOTE of arg
  | CMOV of condition * arg * arg
  | CMP of arg * arg
  | CQO
  | DEC of arg
  | HLT
  | IDIV of arg
  | IMUL of arg * arg option
  | MUL of arg
  | INC of arg
  | J of condition * arg
  | JMP of arg
  | LEA of arg * arg
  | LOCK_CMPXCHG of arg * arg
  | LOCK_XADD of arg * arg
  | LOCK_ADD of arg * arg
  | LOCK_SUB of arg * arg
  | LOCK_AND of arg * arg
  | LOCK_OR of arg * arg
  | LOCK_XOR of arg * arg
  | LEAVE
  | MOV of arg * arg
  | MOVSX of arg * arg
  | MOVSXD of arg * arg
  | MOVZX of arg * arg
  | NEG of arg
  | NOP
  | OR of arg * arg
  | PAUSE
  | POP of arg
  | POPCNT of arg * arg
  | PREFETCH of bool * prefetch_temporal_locality_hint * arg
  | PUSH of arg
  | RDTSC
  | RDPMC
  | LFENCE
  | SFENCE
  | MFENCE
  | RET
  | SAL of arg * arg
  | SAR of arg * arg
  | SET of condition * arg
  | SHR of arg * arg
  | SUB of arg * arg
  | TEST of arg * arg
  | XCHG of arg * arg
  | XOR of arg * arg
  | TZCNT of arg * arg
  | LZCNT of arg * arg
  | SIMD of Amd64_simd_instrs.instr * arg array

(* ELF specific *)
type reloc_type =
  | R_X86_64_PLT32

type reloc =
  { offset : constant;
    name : reloc_type;
    expr : constant;
  }

(* CR gyorsh: use inline record for Section and File constructors. *)
type asm_line =
  | Ins of instruction
  | Directive of Asm_targets.Asm_directives.Directive.t

type asm_program = asm_line list
