[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
open Format

let operation ?(print_reg = Printreg.reg) (op : Operation.t) arg ppf res =
  let reg = print_reg in
  let regs = Printreg.regs' ~print_reg in
  if Array.length res > 0 then fprintf ppf "%a := " regs res;
  match op with
  | Move -> regs ppf arg
  | Spill -> fprintf ppf "%a (spill)" regs arg
  | Reload -> fprintf ppf "%a (reload)" regs arg
  | Const_int n -> fprintf ppf "%s" (Nativeint.to_string n)
  | Const_float32 f -> fprintf ppf "%Fs" (Int32.float_of_bits f)
  | Const_float f -> fprintf ppf "%F" (Int64.float_of_bits f)
  | Const_symbol s -> fprintf ppf "\"%s\"" s.sym_name
  | Const_vec128 { word0; word1 } -> fprintf ppf "%016Lx:%016Lx" word0 word1
  | Const_vec256 { word0; word1; word2; word3 } ->
    fprintf ppf "%016Lx:%016Lx:%016Lx:%016Lx" word0 word1 word2 word3
  | Const_vec512 { word0; word1; word2; word3; word4; word5; word6; word7 } ->
    fprintf ppf "%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx" word0
      word1 word2 word3 word4 word5 word6 word7
  | Stackoffset n -> fprintf ppf "offset stack %i" n
  | Load { memory_chunk; addressing_mode; mutability = Immutable; is_atomic } ->
    fprintf ppf "%s %a[%a]"
      (Printcmm.chunk memory_chunk)
      (fun pp a -> if a then fprintf pp "atomic" else ())
      is_atomic
      (Arch.print_addressing reg addressing_mode)
      arg
  | Load { memory_chunk; addressing_mode; mutability = Mutable; is_atomic } ->
    fprintf ppf "%s %a mut[%a]"
      (Printcmm.chunk memory_chunk)
      (fun pp a -> if a then fprintf pp "atomic" else ())
      is_atomic
      (Arch.print_addressing reg addressing_mode)
      arg
  | Store (chunk, addr, is_assign) ->
    fprintf ppf "%s[%a] := %a %s" (Printcmm.chunk chunk)
      (Arch.print_addressing reg addr)
      (Array.sub arg 1 (Array.length arg - 1))
      reg arg.(0)
      (if is_assign then "(assign)" else "(init)")
  | Alloc { bytes = n; mode = Cmm.Alloc_mode.Heap; dbginfo = _ } ->
    fprintf ppf "alloc %i" n
  | Alloc { bytes = n; mode = Cmm.Alloc_mode.Local; dbginfo = _ } ->
    fprintf ppf "alloc_local %i" n
  | Intop op ->
    if Operation.is_unary_integer_operation op
    then (
      assert (Array.length arg = 1);
      fprintf ppf "%s%a" (Operation.string_of_integer_operation op) reg arg.(0))
    else (
      assert (Array.length arg = 2);
      fprintf ppf "%a%s%a" reg arg.(0)
        (Operation.string_of_integer_operation op)
        reg arg.(1))
  | Intop_imm (op, n) ->
    fprintf ppf "%a%s%i" reg arg.(0)
      (Operation.string_of_integer_operation op)
      n
  | Intop_atomic { op = Compare_set; size; addr } ->
    fprintf ppf "lock compare_set %s[%a] ?%a %a"
      (Printcmm.atomic_bitwidth size)
      (Arch.print_addressing reg addr)
      (Array.sub arg 2 (Array.length arg - 2))
      reg arg.(0) reg arg.(1)
  | Intop_atomic
      { op = (Fetch_and_add | Add | Sub | Land | Lor | Lxor) as op; size; addr }
    ->
    fprintf ppf "lock %s[%a] %s %a"
      (Printcmm.atomic_bitwidth size)
      (Arch.print_addressing reg addr)
      (Array.sub arg 1 (Array.length arg - 1))
      (Printcmm.atomic_op op) reg arg.(0)
  | Intop_atomic { op = Compare_exchange; size; addr } ->
    fprintf ppf "lock compare_exchange %s[%a] ?%a %a"
      (Printcmm.atomic_bitwidth size)
      (Arch.print_addressing reg addr)
      (Array.sub arg 2 (Array.length arg - 2))
      reg arg.(0) reg arg.(1)
  | Intop_atomic { op = Exchange; size; addr } ->
    fprintf ppf "lock exchange %s[%a] %a"
      (Printcmm.atomic_bitwidth size)
      (Arch.print_addressing reg addr)
      (Array.sub arg 1 (Array.length arg - 1))
      reg arg.(0)
  | Floatop (_, ((Icompf _ | Iaddf | Isubf | Imulf | Idivf) as op)) ->
    fprintf ppf "%a %a %a" reg arg.(0) Operation.format_float_operation op reg
      arg.(1)
  | Floatop (_, ((Inegf | Iabsf) as op)) ->
    fprintf ppf "%a %a" Operation.format_float_operation op reg arg.(0)
  | Csel tst ->
    let len = Array.length arg in
    fprintf ppf "csel %a ? %a : %a"
      (Operation.format_test ~print_reg:Printreg.reg tst)
      arg reg
      arg.(len - 2)
      reg
      arg.(len - 1)
  | Reinterpret_cast cast ->
    fprintf ppf "%s %a" (Printcmm.reinterpret_cast cast) reg arg.(0)
  | Static_cast cast ->
    fprintf ppf "%s %a" (Printcmm.static_cast cast) reg arg.(0)
  | Opaque -> fprintf ppf "opaque %a" reg arg.(0)
  | Name_for_debugger
      { ident; which_parameter; regs = r; provenance = _; is_assignment = _ } ->
    fprintf ppf "%a holds the value of %a%s" regs r Backend_var.print ident
      (match which_parameter with
      | None -> ""
      | Some index -> sprintf "[P%d]" index)
  | Begin_region -> fprintf ppf "beginregion"
  | End_region -> fprintf ppf "endregion %a" reg arg.(0)
  | Specific op -> Arch.print_specific_operation reg op ppf arg
  | Dls_get -> fprintf ppf "dls_get"
  | Poll -> fprintf ppf "poll call"
  | Pause -> fprintf ppf "pause"
  | Probe_is_enabled { name } -> fprintf ppf "probe_is_enabled \"%s\"" name
