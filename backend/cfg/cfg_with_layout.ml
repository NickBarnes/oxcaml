(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

let debug = false

module DLL = Oxcaml_utils.Doubly_linked_list

type layout = Label.t DLL.t

type t =
  { cfg : Cfg.t;
    mutable layout : layout;
    sections : (Label.t, string) Hashtbl.t
  }

let create cfg ~layout = { cfg; layout; sections = Hashtbl.create 3 }

let cfg t = t.cfg

let layout t = t.layout

let label_set_of_layout : layout -> Label.Set.t =
 fun layout -> DLL.fold_right layout ~init:Label.Set.empty ~f:Label.Set.add

let set_layout t layout =
  (if debug
  then
    let cur_layout = label_set_of_layout t.layout in
    let new_layout = label_set_of_layout layout in
    let hd_is_entry =
      match DLL.hd layout with
      | None -> false
      | Some label -> Label.equal label t.cfg.entry_label
    in
    if not (hd_is_entry && Label.Set.equal cur_layout new_layout)
    then
      Misc.fatal_error
        "Cfg set_layout: new layout is not a permutation of the current \
         layout, or first label is not entry");
  t.layout <- layout

let assign_blocks_to_section t labels name =
  List.iter
    (fun label ->
      match Hashtbl.find_opt t.sections label with
      | Some new_name ->
        Misc.fatal_errorf
          "Cannot add %a->%s section mapping, already have %a->%s" Label.format
          label name Label.format label new_name ()
      | None -> Hashtbl.replace t.sections label name)
    labels

let get_section t label = Hashtbl.find_opt t.sections label

exception Found_all

let remove_blocks t labels_to_remove =
  let num_to_remove = Label.Set.cardinal labels_to_remove in
  if num_to_remove > 0
  then (
    (* remove from cfg *)
    Cfg.remove_blocks t.cfg labels_to_remove;
    (* remove from layout *)
    let num_removed = ref 0 in
    try
      DLL.iter_cell t.layout ~f:(fun cell ->
          if !num_removed = num_to_remove then raise Found_all;
          let l = DLL.value cell in
          if Label.Set.mem l labels_to_remove
          then (
            DLL.delete_curr cell;
            incr num_removed))
    with Found_all -> ())

let add_block t (block : Cfg.basic_block) ~after =
  match
    DLL.find_cell_opt t.layout ~f:(fun label -> Label.equal label after)
  with
  | None -> Misc.fatal_error "Cfg set_layout: 'after' block is not present"
  | Some cell ->
    DLL.insert_after cell block.start;
    Cfg.add_block_exn t.cfg block

let is_trap_handler t label =
  let block = Cfg.get_block_exn t.cfg label in
  block.is_trap_handler

(* Printing utilities for debug *)

let dump ppf t ~msg =
  let open Format in
  fprintf ppf "\ncfg for %s\n" msg;
  fprintf ppf "%s\n" t.cfg.fun_name;
  fprintf ppf "layout.length=%d\n" (DLL.length t.layout);
  fprintf ppf "blocks.length=%d\n" (Label.Tbl.length t.cfg.blocks);
  let print_block label =
    let block = Label.Tbl.find t.cfg.blocks label in
    fprintf ppf "\n%a:\n" Label.format label;
    let pp_with_id ppf ~pp (instr : _ Cfg.instruction) =
      fprintf ppf "(id:%a) %a\n" InstructionId.format instr.id pp instr
    in
    DLL.iter ~f:(pp_with_id ppf ~pp:Cfg.print_basic) block.body;
    pp_with_id ppf ~pp:Cfg.print_terminator block.terminator;
    fprintf ppf "\npredecessors:";
    Label.Set.iter (fprintf ppf " %a" Label.format) block.predecessors;
    fprintf ppf "\nsuccessors:";
    Label.Set.iter
      (fprintf ppf " %a" Label.format)
      (Cfg.successor_labels ~normal:true ~exn:false block);
    fprintf ppf "\nexn-successors:";
    Label.Set.iter
      (fprintf ppf " %a" Label.format)
      (Cfg.successor_labels ~normal:false ~exn:true block);
    fprintf ppf "\n"
  in
  DLL.iter ~f:print_block t.layout

let print_row r ppf = Format.dprintf "@,@[<v 1><tr>%t@]@,</tr>" r ppf

type align =
  | Left
  | Right
  | Center

let print_align ppf align =
  let s =
    match align with Left -> "left" | Right -> "right" | Center -> "center"
  in
  Format.fprintf ppf "%s" s

let print_cell ?(col_span = 1) ~align f ppf =
  Format.dprintf
    "@,@[<v 1><td align=\"%a\" balign=\"%a\" colspan=\"%d\">@,%t@]@,</td>"
    print_align align print_align align col_span f ppf

let empty_cell ~col_span ppf =
  if col_span > 0 then print_cell ~align:Center (fun _ -> ()) ppf

let ( ++ ) (f1 : Format.formatter -> unit) (f2 : Format.formatter -> unit) ppf =
  f1 ppf;
  f2 ppf

let print_escaped ppf s =
  (* This prints characters one by one, but that's the best we can do without
     allocations. *)
  String.iter
    (function
      | '&' -> Format.pp_print_string ppf "&amp;"
      | '<' -> Format.pp_print_string ppf "&lt;"
      | '>' -> Format.pp_print_string ppf "&gt;"
      | '\"' -> Format.pp_print_string ppf "&quot;"
      | '\n' -> Format.pp_print_string ppf "<br/>"
      | '\t' ->
        (* Convert tabs to 4 spaces because tabs aren't rendered in html-like
           labels. *)
        Format.pp_print_string ppf "    "
      | c -> Format.pp_print_char ppf c)
    s

let with_escape_ppf f ppf =
  let buffer = Buffer.create 0 in
  let buf_ppf = Format.formatter_of_buffer buffer in
  f buf_ppf;
  Format.pp_print_flush buf_ppf ();
  Buffer.to_bytes buffer |> Bytes.to_string |> print_escaped ppf;
  ()

let print_dot ?(show_instr = true) ?(show_exn = true)
    ?(annotate_instr = [Cfg.print_instruction]) ?annotate_block
    ?annotate_block_end ?(annotate_succ : (Label.t -> Label.t -> string) option)
    ppf t =
  let ppf =
    (* Change space indent into tabs because spaces are rendered by [dot]
       command and tabs not. *)
    let funcs = Format.pp_get_formatter_out_functions ppf () in
    let out_indent n =
      for _ = 1 to n do
        funcs.out_string "\t" 0 1
      done
    in
    Format.formatter_of_out_functions { funcs with out_indent }
  in
  Format.fprintf ppf "strict digraph \"%s\" {\n" t.cfg.fun_name;
  let col_count = 1 + List.length annotate_instr in
  let annotate_instr i ppf =
    List.iter
      (fun f ->
        print_cell ~align:Left (with_escape_ppf (fun ppf -> f ppf i)) ppf)
      annotate_instr
  in
  let annotate_block (label : Label.t) : string =
    match annotate_block with
    | None -> ""
    | Some f -> Printf.sprintf " %s" (f label)
  in
  let annotate_succ (l1 : Label.t) (l2 : Label.t) : string =
    match annotate_succ with
    | None -> ""
    | Some f -> Printf.sprintf " label=\"%s\"" (f l1 l2)
  in
  let print_block_dot label (block : Cfg.basic_block) index =
    let name (l : Label.t) : string =
      Printf.sprintf "\".L%s\"" (Label.to_string l)
    in
    let show_index = Option.value index ~default:(-1) in
    Format.fprintf ppf
      "\n\
       %s [shape=none width=0 height=0 margin=0 label=<@,\
       @[<v 0>@[<v 1><table border=\"0\" cellborder=\"1\" cellspacing=\"0\" \
       align=\"left\">%t"
      (name label)
      (print_row
         (print_cell ~col_span:col_count ~align:Center
            (Format.dprintf ".L%a:I%d:S%d%s%s%s" Label.format label show_index
               (DLL.length block.body)
               (if block.stack_offset > 0
               then ":T" ^ string_of_int block.stack_offset
               else "")
               (if block.is_trap_handler then ":eh" else "")
               (annotate_block label))));
    if show_instr
    then (
      (print_row
         (print_cell ~col_span:col_count ~align:Left
            (Format.dprintf "preds: %a"
               (Format.pp_print_seq
                  ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
                  Label.format)
               (Label.Set.to_seq block.predecessors))))
        ppf;
      let print_id_and_ls_order :
          type a. a Cfg.instruction -> Format.formatter -> unit =
       fun i ppf ->
        if i.ls_order >= 0
        then
          Format.dprintf "id:%a ls:%d" InstructionId.format i.id i.ls_order ppf
        else Format.dprintf "id:%a" InstructionId.format i.id ppf
      in
      DLL.iter
        ~f:(fun (i : _ Cfg.instruction) ->
          (print_row
             (print_cell ~align:Right (print_id_and_ls_order i)
             ++ annotate_instr (`Basic i)))
            ppf)
        block.body;
      let ti = block.terminator in
      (print_row
         (print_cell ~align:Right (print_id_and_ls_order ti)
         ++ annotate_instr (`Terminator ti)))
        ppf;
      match annotate_block_end with
      | None -> ()
      | Some annotate_block_end ->
        let col_span = max 1 (col_count - 1) in
        (print_row
           (empty_cell ~col_span:(col_count - col_span)
           ++ print_cell ~col_span ~align:Left (fun ppf ->
                  annotate_block_end ppf block)))
          ppf);
    Format.fprintf ppf "@]@,</table>@]\n>]\n";
    let print_arrow ?style ?label ppf from to_ =
      let port_head, port_tail =
        if String.equal from to_ then ":s", ":n" else "", ""
      in
      Format.fprintf ppf "%s%s->%s%s [%a%a]\n" from port_head to_ port_tail
        (Format.pp_print_option (fun ppf -> Format.fprintf ppf "style=\"%s\""))
        style
        (Format.pp_print_option Format.pp_print_string)
        label
    in
    Label.Set.iter
      (fun l ->
        print_arrow ppf (name label) (name l) ~label:(annotate_succ label l))
      (Cfg.successor_labels ~normal:true ~exn:false block);
    if show_exn
    then (
      Label.Set.iter
        (fun l ->
          print_arrow ppf (name label) (name l) ~style:"dashed"
            ~label:(annotate_succ label l))
        (Cfg.successor_labels ~normal:false ~exn:true block);
      if Cfg.can_raise_interproc block
      then print_arrow ppf (name label) "placeholder" ~style:"dashed")
  in
  (* print all the blocks, even if they don't appear in the layout *)
  DLL.iteri t.layout ~f:(fun i label ->
      let block = Label.Tbl.find t.cfg.blocks label in
      print_block_dot label block (Some i));
  if DLL.length t.layout < Label.Tbl.length t.cfg.blocks
  then
    Label.Tbl.iter
      (fun label block ->
        match DLL.find_opt ~f:(fun lbl -> Label.equal label lbl) t.layout with
        | None -> print_block_dot label block None
        | _ -> ())
      t.cfg.blocks;
  Format.fprintf ppf "}\n%!";
  ()

let save_as_dot ?show_instr ?show_exn ?annotate_instr ?annotate_block
    ?annotate_block_end ?annotate_succ ?filename t msg =
  let filename =
    match filename with
    | Some filename -> filename
    | None ->
      Printf.sprintf "%s%s%s.dot"
        (* some of all the special characters that confuse assemblers also
           confuse dot. get rid of them.*)
        (X86_proc.string_of_symbol "" t.cfg.fun_name)
        (if String.equal msg "" then "" else ".")
        msg
  in
  if !Cfg.verbose then Printf.printf "Writing cfg for %s to %s\n" msg filename;
  let oc = open_out filename in
  Misc.try_finally
    (fun () ->
      let ppf = Format.formatter_of_out_channel oc in
      print_dot ?show_instr ?show_exn ?annotate_instr ?annotate_block
        ?annotate_block_end ?annotate_succ ppf t)
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun _exn -> Misc.remove_file filename)

module Permute = struct
  (* Implementation of this module is copied from Base *)
  external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

  external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"

  let default_random_state = Random.State.make_self_init ()

  let array ?(random_state = default_random_state) t =
    let swap t i j =
      let elt_i = unsafe_get t i in
      let elt_j = unsafe_get t j in
      unsafe_set t i elt_j;
      unsafe_set t j elt_i
    in
    let num_swaps = Array.length t - 1 in
    for i = num_swaps downto 1 do
      (* [random_i] is drawn from [0,i] *)
      let random_i = Random.State.int random_state (i + 1) in
      swap t i random_i
    done

  let list ?(random_state = default_random_state) list =
    match list with
    (* special cases to speed things up in trivial cases *)
    | [] | [_] -> list
    | [x; y] -> if Random.State.bool random_state then [y; x] else list
    | _ ->
      let arr = Array.of_list list in
      array ~random_state arr;
      Array.to_list arr
end

let reorder_blocks_random ?random_state t =
  (* Ensure entry block remains first *)
  let original_layout = DLL.to_list (layout t) in
  let new_layout =
    List.hd original_layout
    :: Permute.list ?random_state (List.tl original_layout)
  in
  set_layout t (DLL.of_list new_layout)

let reorder_blocks ~comparator t =
  (* CR ncourant: this is only ever called with a boolean comparator, we could
     do better. Or maybe we should write stable_sort on DLL to avoid the
     conversions? *)
  (* Ensure entry block remains first *)
  let original_layout = DLL.to_list (layout t) in
  let new_layout =
    List.hd original_layout
    :: List.stable_sort comparator (List.tl original_layout)
  in
  set_layout t (DLL.of_list new_layout)

let iter_blocks : t -> f:(Cfg.basic_block -> unit) -> unit =
 fun cfg_with_layout ~f ->
  let cfg = cfg_with_layout.cfg in
  DLL.iter cfg_with_layout.layout ~f:(fun label ->
      let block = Cfg.get_block_exn cfg label in
      f block)

let iter_instructions :
    t ->
    instruction:(Cfg.basic Cfg.instruction -> unit) ->
    terminator:(Cfg.terminator Cfg.instruction -> unit) ->
    unit =
 fun cfg_with_layout ~instruction ~terminator ->
  Cfg.iter_blocks cfg_with_layout.cfg ~f:(fun _label block ->
      DLL.iter ~f:instruction block.body;
      terminator block.terminator)

let fold_instructions :
    type a.
    t ->
    instruction:(a -> Cfg.basic Cfg.instruction -> a) ->
    terminator:(a -> Cfg.terminator Cfg.instruction -> a) ->
    init:a ->
    a =
 fun cfg_with_layout ~instruction ~terminator ~init ->
  Cfg.fold_blocks cfg_with_layout.cfg ~init ~f:(fun _label block acc ->
      let acc = DLL.fold_left ~f:instruction ~init:acc block.body in
      let acc = terminator acc block.terminator in
      acc)

let insert_block :
    t ->
    Cfg.basic_instruction_list ->
    after:Cfg.basic_block ->
    before:Cfg.basic_block option ->
    next_instruction_id:(unit -> InstructionId.t) ->
    Cfg.basic_block list =
 fun cfg_with_layout body ~after:predecessor_block ~before:only_successor
     ~next_instruction_id ->
  let cfg = cfg_with_layout.cfg in
  let successors =
    match only_successor with
    | None -> Cfg.successor_labels ~normal:true ~exn:false predecessor_block
    | Some only_successor -> Label.Set.singleton only_successor.start
  in
  if Label.Set.cardinal successors = 0
  then
    Misc.fatal_errorf
      "Cannot insert a block after block %a: it has no successors" Label.print
      predecessor_block.start;
  let dbg, fdo, live, stack_offset, available_before, available_across =
    match DLL.last body with
    | None ->
      ( Debuginfo.none,
        Fdo_info.none,
        Reg.Set.empty,
        predecessor_block.terminator.stack_offset,
        None,
        None )
    | Some
        { dbg; fdo; live; stack_offset; available_before; available_across; _ }
      ->
      dbg, fdo, live, stack_offset, available_before, available_across
  in
  let copy (i : Cfg.basic Cfg.instruction) : Cfg.basic Cfg.instruction =
    { i with id = next_instruction_id () }
  in
  (* copy body if there is more than one successor *)
  let first = ref true in
  let get_body () =
    if !first
    then (
      first := false;
      body)
    else
      let new_body = DLL.make_empty () in
      DLL.iter body ~f:(fun instr -> DLL.add_end new_body (copy instr));
      new_body
  in
  Label.Set.fold
    (fun successor_label new_labels ->
      let successor_block = Cfg.get_block_exn cfg successor_label in
      let start = Cmm.new_label () in
      let block : Cfg.basic_block =
        { start;
          body = get_body ();
          terminator =
            { (* The [successor_block] is the only successor. *)
              desc = Cfg.Always successor_label;
              arg = [||];
              res = [||];
              dbg;
              fdo;
              live;
              stack_offset;
              id = next_instruction_id ();
              irc_work_list = Unknown_list;
              ls_order = -1;
              available_before;
              available_across
            };
          (* The [predecessor_block] is the only predecessor. *)
          predecessors = Label.Set.singleton predecessor_block.start;
          stack_offset = predecessor_block.terminator.stack_offset;
          exn = None;
          can_raise = false;
          is_trap_handler = false;
          cold = predecessor_block.cold
        }
      in
      add_block cfg_with_layout block ~after:predecessor_block.start;
      (* Change the labels for the terminator in [predecessor_block]. *)
      Cfg.replace_successor_labels cfg ~normal:true ~exn:false predecessor_block
        ~f:(fun old_label ->
          if Label.equal old_label successor_label then start else old_label);
      (* Update predecessors for the [successor_block]. *)
      successor_block.predecessors
        <- successor_block.predecessors
           |> Label.Set.remove predecessor_block.start
           |> Label.Set.add start;
      block :: new_labels)
    successors []
