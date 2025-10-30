(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Malo Monin, projet Cambium, Inria Paris                 *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


type cost_model = {
  insertion:int;
  deletion:int;
  substitution:int;
}

module Trie = struct
  let new_uid =
    let counter = ref 0 in
    fun () ->
      incr counter;
      !counter

  type 'a t = {
    uid : int;
    mutable leaf_data : 'a option;
    strict_suffixes : (char, 'a t) Hashtbl.t;
    mutable subtrie_count : int;
        (** The total number of subtries this trie contains (including
            itself). *)
    mutable shortest_suffix : (int * 'a) option;
        (** The length and associated data of a shortest suffix, if any. *)
    mutable longest_suffix : (int * 'a) option;
        (** The length and associated data of a longest suffix, if any. *)
  }

  let create () =
    {
      uid = new_uid ();
      leaf_data = None;
      strict_suffixes = Hashtbl.create 1;
      subtrie_count = 1;
      shortest_suffix = None;
      longest_suffix = None;
    }

  let add trie string data =
    let rec aux s length trie =
      (trie.shortest_suffix <-
        match trie.shortest_suffix with
        | Some (l, d) when l <= length -> Some (l, d)
        | _ -> Some (length, data));
      (trie.longest_suffix <-
        match trie.longest_suffix with
        | Some (l, d) when l >= length -> Some (l, d)
        | _ -> Some (length, data));
      match s () with
      | Seq.Nil ->
          trie.leaf_data <- Some data
      | Seq.Cons (c, next) ->
          match Hashtbl.find_opt trie.strict_suffixes c with
          | None ->
              let new_child = create () in
              aux next (length - 1) new_child;
              Hashtbl.add trie.strict_suffixes c new_child;
              trie.subtrie_count <- trie.subtrie_count + new_child.subtrie_count
          | Some child ->
              let subtries_without_child =
                trie.subtrie_count - child.subtrie_count
              in
              aux next (length - 1) child;
              trie.subtrie_count <- subtries_without_child + child.subtrie_count
    in
    aux (String.to_seq string) (String.length string) trie

  let of_seq entries =
    let trie = create () in
    Seq.iter (fun (string, data) -> add trie string data) entries;
    trie

  module Levenshtein_state = struct
    (** A state of a Levenshtein automaton. *)

    type nonrec 'a t = {
      trie : 'a t;  (** The remaining suffixes we can match against. *)
      remaining_length : int;
          (** The remaining length of the string we are trying to match. *)
      distance : int;
          (** The current distance to the string we are trying to match. *)
      remaining_distance_estimation : int;
          (** An estimation of the remaining distance. *)
    }

    let priority state = state.distance + state.remaining_distance_estimation
    let compare s s' = compare (priority s) (priority s')

    (** An admissible heuristic for A* (as in, it always under-estimates the
        true remainign distance). *)
    let estimate_remaining_distance cost remaining_length trie =
      match (trie.shortest_suffix, trie.longest_suffix) with
      | Some (shortest_length, _), _
      when remaining_length <= shortest_length ->
          Some ((shortest_length - remaining_length) * cost.insertion)
      | _, Some (longest_length, _) when remaining_length >= longest_length ->
          Some ((remaining_length - longest_length) * cost.deletion)
      | None, None -> None
      | _, _ -> Some 0

    let make cost trie remaining_length distance =
      match estimate_remaining_distance cost remaining_length trie with
      | Some remaining_distance_estimation ->
          [{
            trie;
            remaining_length;
            distance;
            remaining_distance_estimation
          }]
      | None -> []

    (** Computes a list of all possible states after performing a single
        operation. *)
    let transitions cost text state =
      let n = String.length text in
      let deletions =
        if state.remaining_length > 0 then
          make cost state.trie
            (state.remaining_length - 1)
            (state.distance + cost.deletion)
        else []
      in
      Hashtbl.fold
        (fun c suffix_trie transitions ->
           let insertion = make cost suffix_trie
               state.remaining_length
               (state.distance + cost.insertion)
           in
           let subst =
             if state.remaining_length = 0 then [] else
             let substitution_cost_here =
               if c = text.[n - state.remaining_length] then
                 0
               else
                 cost.substitution
             in
             make cost suffix_trie
               (state.remaining_length - 1)
               (state.distance + substitution_cost_here)
           in
            subst @ insertion @ transitions
        ) state.trie.strict_suffixes deletions
  end


  let (%>%) (x:int) (y:int option) =
    match y with
    | None -> false
    | Some y -> x > y

  module State = Levenshtein_state

  let default_cost = { deletion = 1; insertion=1; substitution=1 }

  let compute_preferences (type a) cost ?(cutoff : int option) (trie : a t)
      (string : string) : (a * int) Seq.t =
    let module PriorityQueue =
      Pqueue.MakeMin (struct type t = a State.t let compare = State.compare end)
    in
    let rec compute queue seen_states = fun () ->
      match PriorityQueue.pop_min queue with
      | None -> Seq.Nil
      | Some state ->
          if State.priority state %>% cutoff then
            Seq.Nil
          else
            let state_id = state.trie.uid, state.State.remaining_length in
            if Hashtbl.mem seen_states state_id then
              compute queue seen_states ()
            else (
              Hashtbl.add seen_states state_id ();
              List.iter
                  (PriorityQueue.add queue)
                  (State.transitions cost string state);
              match state with
              | {
                State.trie = { leaf_data = Some data; _ };
                remaining_length = 0;
                distance;
                _;
              } ->
                  Seq.Cons ((data, distance), compute queue seen_states)
              | _ -> compute queue seen_states ()
            )
    in

    let n = String.length string in
    let queue = PriorityQueue.create () in
    List.iter (PriorityQueue.add queue) (State.make cost trie n 0);
    let seen_states = Hashtbl.create trie.subtrie_count in
    compute queue seen_states

  let rec group_ties seq current_distance acc () =
    match seq () with
    | Seq.Nil -> Seq.Cons ((acc, current_distance), Seq.empty)
    | Seq.Cons ((data, distance), next) ->
        if distance = current_distance then
          group_ties next current_distance (data :: acc) ()
        else
          let next_layer = group_ties next distance [ data ] in
          Seq.Cons ((acc, current_distance), next_layer)

  let compute_preference_layers ?(cost = default_cost) ?cutoff ?max_elements
      trie query =
    let preferences = compute_preferences cost ?cutoff trie query in
    let seq =
      match max_elements with
      | Some n -> Seq.take n preferences
      | None -> preferences
    in
    match seq () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons((data,distance), seq) -> group_ties seq distance [data]
end


type layer = { left_candidates: int list; pref:int }

type ('a,'v) matches = {
  left : 'a list;
  pairs : ('v * 'v) list;
  right : 'a list;
}


let reverse_matches d =
  {
    right = d.left;
    left = d.right;
    pairs = List.map (fun (right, left) -> (left, right)) d.pairs;
  }

module Item = struct
  type ('v, 'k) t = {
    name: string;
    item: 'v;
    kind: 'k;
  }

  let name f = f.name
  let item f = f.item
  let kind i = i.kind
end
type nonrec ('v,'k) item_matches =  (('v,'k) Item.t, 'v) matches


(** An implementation (in [diff]) of Zoltan Kiraly's "New Algorithm," presented
    in "Linear Time Local Approximation Algorithm for Maximum Stable Marriage":
    https://www.mdpi.com/1999-4893/6/3/471. It computes a 3/2-approximation of
    a maximum stable marriage in linear time (linear in the sum of the lengths
    of the preference lists). *)
module Stable_marriage_diff = struct

  (* This implementation does not use the same semantics as the original paper.
     Below is a conversion from the paper's terms to the implementation's terms:
     - woman: left
     - man: right
     - engaged (woman / man): paired
     - maiden (woman): unpaired
     - active (man): active
     - lad: first phase
     - bachelor: second phase
     - old bachelor: closed
     - uncertain (man): has other choices
     - flighty (woman): has a weak pair *)

  type distance = int

  module Tie_list = struct
    (* List of element tied at a given distance in the global preference list *)
    type t =
      | First_round of { front: int list; second_round:int list }
       (* During the first round, the list of ties is split in two:
         - [front], possibly unpaired left element
         - [second_round] certainly paired left elements
      *)

      | Second_round of int list

    let first_round front second_round = First_round { front; second_round }
    let in_first_round = function
      | First_round _ -> true
      | Second_round _ -> false
    let next tl = match tl with
      | Second_round [] -> None
      | Second_round (a::q) -> Some(a, Second_round q)
      | First_round ({ front = a :: front; _ } as dq) ->
          Some (a, First_round { dq with front })
      | First_round { front = []; second_round } ->
          match List.rev second_round with
          | [] -> None
          | a :: front -> Some (a, Second_round front)
    let delay_to_second_round tl x = match tl with
      | First_round dq ->
          First_round { dq with second_round = x :: dq.second_round }
      | Second_round _ -> tl
    let replace_front x = function
      | First_round dq -> First_round { dq with front = x :: dq.front }
      | Second_round l -> Second_round (x :: l)
    let of_list front = First_round { front; second_round = [] }
  end

  type left_state =
    | Left_unpaired
    | Left_paired of int * distance

  type right_phase =
    | First
    | Second

  type active_right_state = {
    mutable previous_layers : layer list;
        (** Invariant: this list is not empty in the first phase . *)
    mutable current_layer : Tie_list.t;
    mutable current_distance : distance;
    mutable paired: bool;
    mutable phase: right_phase;
    mutable next_layers : layer Seq.t;
  }

  type ('a,'b) state =
    { left: 'a array; right: 'b array; mutable reactivated:int list }

  let is_never_paired state j = match state.left.(j) with
    | Left_unpaired -> true
    | _ -> false

  let rec has_alternative_choices ~compatible state ir r =
    let cl = r.current_layer in
    match cl with
    | Second_round _ | First_round { front = [] | [_]; _ }-> false
    | First_round ({ front = a :: b :: q ; _ } as cl) ->
        if not (compatible b ir) then begin
          r.current_layer <- First_round { cl with front = a :: q};
          has_alternative_choices ~compatible state ir r
        end else
          is_never_paired state b ||
          let current_layer =
            Tie_list.First_round {
              front = a :: q;
              second_round = b :: cl.second_round
            }
          in
          r.current_layer <- current_layer;
          has_alternative_choices ~compatible state ir r

  let rec skip_paired state dq =
    assert (Tie_list.in_first_round dq);
    match Tie_list.next dq with
    | None -> assert false
    | Some (first,others) ->
        if is_never_paired state first then
          first, others
        else skip_paired state (Tie_list.delay_to_second_round others first)

  let has_weak_pair ~compatible state j =
    match state.left.(j) with
    | Left_unpaired -> false
    | Left_paired (i, _) ->
        match state.right.(i) with
        | None -> assert false
        | Some r -> has_alternative_choices ~compatible state i r

  let phase state i =
    Option.map (fun x -> x.phase) state.right.(i)

  let prepare_tie_list state { left_candidates=i; pref=d} r =
    let first, later = List.partition (is_never_paired state) i in
    let tie_list = Tie_list.first_round first later in
    match state.right.(r) with
    | None -> ()
    | Some r ->
        r.current_distance <- d;
        r.current_layer <- tie_list

  let second_phase state ir r =
    let layers = List.rev r.previous_layers in
    r.previous_layers <- [];
    r.phase <- Second;
    match layers with
    | [] -> assert false
    | layer :: q ->
        prepare_tie_list state layer ir;
        r.next_layers <- List.to_seq q

  let next_layer state ir r = match r.next_layers () with
    | Seq.Nil ->
        begin match r.phase with
        | First -> second_phase state ir r; true
        | Second -> false
        end
    | Seq.Cons(layer, next_layers) ->
        r.previous_layers <- layer :: r.previous_layers;
        r.next_layers <- next_layers;
        prepare_tie_list state layer ir;
        true

  let rec get_left_candidate ~compatible state ir r =
    assert (r.paired = false);
    if has_alternative_choices ~compatible state ir r then
      let f, others = skip_paired state r.current_layer in
      r.current_layer <- others;
      Some f
    else match Tie_list.next r.current_layer with
      | Some (f,others) ->
          r.current_layer <- others;
          Some f
      | None ->
          if next_layer state ir r then
            get_left_candidate ~compatible state ir r
          else None

  let rec get_compatible_left_candidate ~compatible state ir r =
    match get_left_candidate ~compatible state ir r with
    | None -> None
    | Some l as c ->
        if compatible l ir then c
        else
          get_compatible_left_candidate ~compatible state ir r

  let reject state i =
    match state.right.(i) with
    | None -> ()
    | Some right ->
        right.paired <- false;
        match Tie_list.next right.current_layer with
        | None -> state.right.(i) <- None
        | Some (f,others) ->
            let tie_list = Tie_list.delay_to_second_round others f in
            right.current_layer <- tie_list;
            state.reactivated <- i :: state.reactivated

  let accepted_proposal ~compatible state i j d =
    has_weak_pair ~compatible state j ||
    match state.left.(j) with
    | Left_unpaired -> true
    | Left_paired (i', d') ->
        d < d' ||
        d = d' &&
        match phase state i, phase state i' with
        | Some Second, Some First -> true
        | _ -> false

  let pair state i j d =
    begin match state.right.(i) with
    | None -> ()
    | Some r ->
      r.paired <- true;
      r.current_layer <- Tie_list.replace_front j r.current_layer
    end;
    match state.left.(j) with
    | Left_unpaired -> state.left.(j) <- Left_paired (i, d)
    | Left_paired (i', _) ->
        reject state i';
        state.left.(j) <- Left_paired (i, d)

  let init_right_state ~preferences right =
    Array.map
      (fun right_field ->
         let name = Item.name right_field in
         let sequence = preferences name in
         match sequence () with
         | Seq.Nil -> None
         | Seq.Cons (layer, tail) ->
             Some {
               paired = false;
               phase = First;
               current_distance = layer.pref;
               current_layer = Tie_list.of_list layer.left_candidates;
               previous_layers = [layer];
               next_layers = tail;
             }
      )
      right


  let rec proposals ~compatible state i right =
    match get_compatible_left_candidate ~compatible state i right with
    | None -> ()
    | Some j ->
        if accepted_proposal ~compatible state i j right.current_distance then
          pair state i j right.current_distance
        else
          proposals ~compatible state i right

  let diff ~preferences ~compatible left right =
    let preferences = preferences left in
    let n = Array.length left in
    let m = Array.length right in
    let left_state = Array.make n Left_unpaired in
    let right_state = init_right_state ~preferences right in
    let state = { left=left_state; reactivated = []; right=right_state } in
    let rec loop = function
      | [] ->
          begin  match state.reactivated with
          | [] -> ()
          | l -> state.reactivated <- []; loop l
          end
      | i :: l ->
        match state.right.(i) with
          | None -> loop l
          | Some right ->
              proposals ~compatible state i right;
              loop l
    in
    loop (List.init m Fun.id);
    let left_final = Seq.zip (Array.to_seq left) (Array.to_seq state.left) in
    let left, pairs = Seq.partition_map (fun (field, status) ->
        match status with
        | Left_unpaired -> Either.Left field
        | Left_paired (i,_) ->
            Either.Right (field, right.(i))
      ) left_final
    in
    {
      left = List.of_seq left;
      right =
        Array.to_seq right
        |> Seq.filteri (fun i _ ->
          match state.right.(i) with
          | Some r -> not r.paired
          | None -> true)
        |> List.of_seq
      ;
      pairs = List.of_seq pairs;
    }

  let diff ~preferences ~compatible left right =
    if Array.length right >  Array.length left then
      diff
        ~preferences
        ~compatible:(fun a b -> compatible b a)
        right left
      |> reverse_matches
    else diff ~preferences ~compatible left right

end

let (%<%) (x:int option) (y:int option) = match x, y with
  | None , _ -> false
  | _, None -> true
  | Some x, Some y -> x < y

let greedy_matching ~compatibility ~cutoff missings additions =
  let rec list_extract predicate l =
    match l with
    | [] -> None
    | hd :: tl when predicate hd -> Some (hd, tl)
    | hd :: tl -> (
        match list_extract predicate tl with
        | None -> None
        | Some (x, tl') -> Some (x, hd :: tl'))
  in

  let compute_distance expected_field added_field =
    if compatibility (Item.kind added_field) (Item.kind expected_field) then
      let distance =
        let expected_name = Item.name expected_field in
        Misc.edit_distance
          expected_name
          (Item.name added_field)
          (cutoff expected_name)
      in
      distance
    else
      None
  in

  let remaining_added_fields = ref additions in
  let name_changes = ref [] in
  let actually_missing =
    missings
    |> List.filter_map
      (fun missing_field ->
        let missing_id = missing_field in
        let missing_name = Item.name missing_id in
        match
          list_extract
            (fun added_field ->
              (compute_distance missing_field added_field)
                %<% Some (cutoff missing_name))
            !remaining_added_fields
        with
        | None -> Some missing_field
        | Some (added_field, additions) ->
            let name_change = Item.item added_field, Item.item missing_field in
            name_changes := name_change :: !name_changes;
            remaining_added_fields := additions;
            None)
  in
  {
    left = !remaining_added_fields;
    pairs= !name_changes;
    right = actually_missing
  }

let preferences ~cutoff left name =
  let cutoff = 1 + cutoff name in
  let a =
    Array.of_seq
    @@ Seq.filter (fun (_,d) -> d < cutoff)
    @@ Seq.mapi (fun i r ->
        i, String.edit_distance ~limit:cutoff name @@ Item.name r)
    @@ Array.to_seq left in
  let () = Array.sort (fun (_,n) (_,n') -> Int.compare n n') a in
  let rec group_by current acc pos () =
    if pos >= Array.length a then
      match acc with
      | [] -> Seq.Nil
      | _ -> Seq.Cons ({ left_candidates=acc; pref=current }, Seq.empty)
    else
      let x, dist = a.(pos) in
      if dist = current then
        group_by current (x::acc) (pos+1) ()
      else if acc = [] then
        group_by dist [x] (pos+1) ()
      else
        Seq.Cons (
          {left_candidates=acc; pref=current}, group_by dist [x] (pos+1)
        )
  in
  group_by 0 [] 0

let fuzzy_match_names ~compatibility left0 right =
  (* The edit distance between an existing name and a suggested rename must be
     at most half the length of the name. *)
  let cutoff name =
    let len = String.length name in
    len/2
  in
  if (*  *List.length left < 60 && List.length right < 60 *) true then
    (* Stable marriages. *)
    let left = Array.of_list left0 in
    let right = Array.of_list right in
    let compatible i j =
      compatibility (Item.kind left.(i)) (Item.kind right.(j))
    in
    let preferences = preferences ~cutoff in
    let matches =
      Stable_marriage_diff.diff ~preferences ~compatible left right
    in
    let pairs = List.map (fun (x,y) -> Item.(item x, item y)) matches.pairs in
    { matches with pairs }
  else
    (* Greedy. *)
    greedy_matching ~compatibility ~cutoff left0 right
