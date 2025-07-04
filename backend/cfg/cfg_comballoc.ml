[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module List = ListLabels
module DLL = Oxcaml_utils.Doubly_linked_list

type cell = Cfg.basic Cfg.instruction DLL.cell

(* Description of an allocation: has the fields of a [Cfg.Alloc _] value, and a
   cell so that the instruction can be modified. *)
type allocation =
  { bytes : int;
    dbginfo : Cmm.alloc_dbginfo;
    mode : Cmm.Alloc_mode.t;
    cell : cell
  }

(* Description of allocations that can be folded into a previous one, and a cell
   indicating where to continue the process. *)
type compatible_allocations =
  { allocations : allocation list;
    next_cell : cell option
  }

(* [find_next_allocation cell] returns the first allocation found by iterating
   from [cell]. *)
let rec find_next_allocation : cell option -> allocation option =
 fun cell ->
  match cell with
  | None -> None
  | Some cell -> (
    let instr = DLL.value cell in
    match instr.desc with
    | Op (Alloc { bytes; dbginfo; mode }) -> Some { bytes; dbginfo; mode; cell }
    | Op
        ( Move | Spill | Reload | Const_int _ | Const_float _ | Const_float32 _
        | Const_symbol _ | Const_vec128 _ | Const_vec256 _ | Const_vec512 _
        | Stackoffset _ | Load _ | Store _ | Intop _ | Intop_imm _
        | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _
        | Static_cast _ | Probe_is_enabled _ | Opaque | Begin_region
        | End_region | Specific _ | Name_for_debugger _ | Dls_get | Poll | Pause
          )
    | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Stack_check _ ->
      find_next_allocation (DLL.next cell))

(* [find_compatible_allocations cell ~curr_mode ~curr_size] returns the
   allocations compatible with mode [curr_mode] and total size [curr_size]. *)
let find_compatible_allocations :
    cell option ->
    curr_mode:Cmm.Alloc_mode.t ->
    curr_size:int ->
    compatible_allocations =
 fun cell ~curr_mode ~curr_size ->
  let rec loop (allocations : allocation list) (cell : cell option)
      ~(curr_mode : Cmm.Alloc_mode.t) ~(curr_size : int) :
      compatible_allocations =
    match cell with
    | None -> { allocations = List.rev allocations; next_cell = None }
    | Some cell -> (
      let instr = DLL.value cell in
      let return () =
        { allocations = List.rev allocations; next_cell = Some cell }
      in
      match instr.desc with
      | Op (Alloc { bytes; dbginfo; mode }) ->
        let is_compatible =
          Cmm.Alloc_mode.equal mode curr_mode
          && (curr_size + bytes
              <= (Config.max_young_wosize + 1) * Arch.size_addr
             || Cmm.Alloc_mode.is_local mode)
        in
        if is_compatible
        then
          let allocation = { bytes; dbginfo; mode; cell } in
          loop
            (allocation :: allocations)
            (DLL.next cell) ~curr_mode ~curr_size:(curr_size + bytes)
        else { allocations = List.rev allocations; next_cell = Some cell }
      | Op (Begin_region | End_region) -> (
        match curr_mode with
        | Local -> return ()
        | Heap -> loop allocations (DLL.next cell) ~curr_mode ~curr_size)
      | Op Poll -> return ()
      | Reloadretaddr | Poptrap _ | Prologue | Pushtrap _ | Stack_check _ ->
        (* CR-soon xclerc for xclerc: is it too conservative? (note: only the
           `Pushtrap` case may be too conservative) *)
        { allocations = List.rev allocations; next_cell = Some cell }
      | Op
          ( Move | Spill | Reload | Floatop _ | Reinterpret_cast _ | Opaque
          | Pause | Const_int _ | Const_float _ | Const_float32 _
          | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Const_symbol _
          | Stackoffset _ | Load _
          | Store (_, _, _)
          | Csel _ | Specific _ | Name_for_debugger _ | Probe_is_enabled _
          | Static_cast _ | Dls_get
          | Intop
              ( Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor | Ilsl
              | Ilsr | Iasr | Ipopcnt | Imulh _ | Iclz _ | Ictz _ | Icomp _ )
          | Intop_imm
              ( ( Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor | Ilsl
                | Ilsr | Iasr | Ipopcnt | Imulh _ | Iclz _ | Ictz _ | Icomp _ ),
                _ )
          | Intop_atomic _ ) ->
        loop allocations (DLL.next cell) ~curr_mode ~curr_size)
  in
  loop [] cell ~curr_mode ~curr_size

(** [combine ~max_instr_id cell] combines allocations, starting from [cell] and
   using [max_instr_id] as the counter to get new instruction identifiers.

   The allocation are combined by repeatedly:

   - 1. looking for a "first" allocation;
   - 2. looking for all subsequent allocations compatible with the "first" one;
   - 3. continuing the process at step 1. from the instruction after the last
        one seen at step 2.

    When steps 1 and 2 are both successful, allocations are effectively
    combined. This means that:

    - the "first" allocation is made bigger to account for all allocations;
    - the other allocations are replaced with a reference to the result of the
      previous allocation, with a different offset. *)
let rec combine : instr_id:InstructionId.sequence -> cell option -> unit =
 fun ~instr_id cell ->
  let first_allocation = find_next_allocation cell in
  match first_allocation with
  | None -> ()
  | Some { bytes; dbginfo; mode; cell } ->
    assert (List.length dbginfo = 1);
    let compatible_allocs =
      find_compatible_allocations (DLL.next cell) ~curr_mode:mode
        ~curr_size:bytes
    in
    (match compatible_allocs.allocations with
    | [] -> ()
    | other_allocations ->
      let first_allocation_instr = DLL.value cell in
      let first_allocation_res0 = first_allocation_instr.res.(0) in
      (* First, replace the "other" allocations with a reference to the result
         of the previous allocation and compute the total size. *)
      let total_size_of_other_allocations, dbginfo_of_other_allocations, _ =
        List.fold_left other_allocations ~init:(0, [], first_allocation_res0)
          ~f:(fun (size, dbginfos, prev_res0) other_allocation ->
            let other_allocation_instr = DLL.value other_allocation.cell in
            let res0 = other_allocation_instr.res.(0) in
            DLL.set_value other_allocation.cell
              { other_allocation_instr with
                desc =
                  Cfg.Op (Intop_imm (Operation.Iadd, -other_allocation.bytes));
                arg = [| prev_res0 |]
              };
            ( size + other_allocation.bytes,
              other_allocation.dbginfo @ dbginfos,
              res0 ))
      in
      (* Then, change the size of the first allocation so that it is the sum of
         all allocations, and update the debug info. *)
      DLL.set_value cell
        { first_allocation_instr with
          desc =
            Cfg.Op
              (Alloc
                 { bytes = bytes + total_size_of_other_allocations;
                   dbginfo = dbginfo_of_other_allocations @ dbginfo;
                   mode
                 })
        };
      DLL.insert_after cell
        { first_allocation_instr with
          desc =
            Cfg.Op (Intop_imm (Operation.Iadd, total_size_of_other_allocations));
          arg = [| first_allocation_res0 |];
          res = [| first_allocation_res0 |];
          id = InstructionId.get_and_incr instr_id
        });
    combine ~instr_id compatible_allocs.next_cell

let run : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let instr_id = cfg.next_instruction_id in
  Cfg.iter_blocks cfg ~f:(fun _label block ->
      combine ~instr_id (DLL.hd_cell block.body));
  cfg_with_layout
