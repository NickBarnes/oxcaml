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

module Item: sig
  type ('v, 'k) t = {
    name: string;
    item : 'v;
    kind : 'k;
  }
  val item: ('v,'k) t -> 'v
end

type left_index = int
type right_index = int
type rank = int

type ('a,'v) matches = {
  left: 'a list;
  pairs:('v * 'v) list;
  right:'a list;
}
type ('v,'k) item_matches =  (('v,'k) Item.t, 'v) matches

type unstable_matching = {
  first:left_index * right_index;
  second: left_index * right_index;
  current_rank: rank * rank;
  optimal: rank * rank
}

val stable_matches:
  distance:(int -> int -> int) -> (_,int) matches ->
  (unit, unstable_matching) Result.t

val strong_stable_matches:
  distance:(int -> int -> int) -> (_,int) matches ->
  (unit, unstable_matching) Result.t

module Gale_Shapley: sig

  val matches:
    compatible:(left_index -> right_index -> bool)
    -> preferences:(right_index -> (left_index * rank) array)
    -> size: (int * int)
    -> (int, int) matches

  val fuzzy_match_names:
    compatibility:('k -> 'k -> bool)
    -> max_right_items:int
    -> cutoff:(string -> int)
    -> ('v,'k) Item.t list -> ('v,'k) Item.t list
    -> ('v,'k) item_matches
end

val matches:
    compatible:(left_index -> right_index -> bool)
    -> preferences:(right_index -> (left_index * rank) array)
    -> size: (int * int)
    -> (int, int) matches

val fuzzy_match_names:
  compatibility:('k -> 'k -> bool)
  -> max_right_items:int
  -> cutoff:(string -> int)
  -> ('v,'k) Item.t list -> ('v,'k) Item.t list
  -> ('v,'k) item_matches
