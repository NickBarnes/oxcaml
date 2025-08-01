# 2 "obj.ml"
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

open! Stdlib

[@@@ocaml.flambda_o3]

(* Operations on internal representations of values *)

type t

type raw_data = nativeint

external repr : 'a -> t @@ portable = "%obj_magic"
external repr_contended : 'a @ contended -> t @ contended @@ portable = "%obj_magic"
external obj : t -> 'a @@ portable = "%obj_magic"
external obj_contended : t @ contended -> 'a @ contended @@ portable = "%obj_magic"
external magic : 'a -> 'b @@ portable = "%obj_magic"
external magic_portable : ('a[@local_opt]) -> ('a[@local_opt]) @ portable @@ portable = "%identity"
external magic_uncontended : ('a[@local_opt]) @ contended -> ('a[@local_opt]) @@ portable = "%identity"
external magic_unique : ('a[@local_opt]) -> ('a[@local_opt]) @ unique @@ portable = "%identity"
external magic_many : ('a[@local_opt]) @ once -> ('a[@local_opt]) @@ portable = "%identity"
external magic_at_unique : ('a[@local_opt]) @ unique -> ('b[@local_opt]) @ unique @@ portable= "%identity"
external is_int : t @ contended -> bool @@ portable = "%obj_is_int"
let [@inline always] is_block a = not (is_int a)
external tag : t @ contended -> int @@ portable = "caml_obj_tag" [@@noalloc]
(* For Flambda 2 there is a strict distinction between arrays and other
   blocks.  %obj_size and %obj_field may only be used on blocks.  As such
   they are protected here using [Sys.opaque_identity], since this
   restriction is likely not respected by callees of this module. *)
external size : t @ contended -> int @@ portable = "%obj_size"
external opaque_identity_contended : 'a @ contended -> 'a @ contended @@ portable = "%opaque"
let [@inline always] size t = size (opaque_identity_contended t)
external reachable_words : t -> int @@ portable = "caml_obj_reachable_words"
external uniquely_reachable_words : t array -> int array * int @@ portable = "caml_obj_uniquely_reachable_words"
external field : t -> int -> t @@ portable = "%obj_field"
let [@inline always] field t index = field (Sys.opaque_identity t) index
external field_contended : t @ contended -> int -> t @ contended @@ portable = "%obj_field"
let [@inline always] field_contended t index = field_contended (opaque_identity_contended t) index
external set_field : t -> int -> t -> unit @@ portable = "%obj_set_field"
let [@inline always] set_field t index new_value =
  set_field (Sys.opaque_identity t) index new_value
external floatarray_get : floatarray -> int -> float @@ portable = "caml_floatarray_get"
external floatarray_set :
    floatarray -> int -> float -> unit @@ portable = "caml_floatarray_set"
let [@inline always] double_field x i = floatarray_get (obj x : floatarray) i
let [@inline always] set_double_field x i v =
  floatarray_set (obj x : floatarray) i v
external raw_field : t -> int -> raw_data @@ portable = "caml_obj_raw_field"
external set_raw_field : t -> int -> raw_data -> unit @@ portable
                                          = "caml_obj_set_raw_field"

external new_block : int -> int -> t @@ portable = "caml_obj_block"

external dup : t -> t @@ portable = "%obj_dup"
external add_offset : t -> Int32.t -> t @@ portable = "caml_obj_add_offset"
external with_tag : int -> t -> t @@ portable = "caml_obj_with_tag"

let first_non_constant_constructor_tag = 0
let last_non_constant_constructor_tag = 243

let forcing_tag = 244
(* Note that cmmgen.ml contains a copy of [cont_tag] of its own *)
let cont_tag = 245
let lazy_tag = 246
let closure_tag = 247
let object_tag = 248
let infix_tag = 249
let forward_tag = 250

let no_scan_tag = 251

let abstract_tag = 251
let string_tag = 252
let double_tag = 253
let double_array_tag = 254
let custom_tag = 255


let int_tag = 1000
let out_of_heap_tag = 1001
let unaligned_tag = 1002

(* [null_tag] is not exposed in the interface of [Stdlib.Obj]
   since [Stdlib.Obj.tag] accepts only non-null values. *)
let[@warning "-32"] null_tag = 1010

module Extension_constructor =
struct
  type t = extension_constructor
  let of_val x =
    let x = repr_contended x in
    let slot =
      if (is_block x) && (tag x) <> object_tag && (size x) >= 1 then field_contended x 0
      else x
    in
    let name =
      if (is_block slot) && (tag slot) = object_tag then field_contended slot 0
      else invalid_arg "Obj.extension_constructor"
    in
      if (tag name) = string_tag then (obj_contended slot : t)
      else invalid_arg "Obj.extension_constructor"

  let [@inline always] name (slot : t) =
    (obj (field (repr slot) 0) : string)

  let [@inline always] id (slot : t) =
    (obj (field (repr slot) 1) : int)
end

module Ephemeron = struct
  type obj_t = t

  type t (** ephemeron *)

   (** To change in sync with weak.h *)
  let additional_values = 2
  let max_ephe_length = Sys.max_array_length - additional_values

  external create : int -> t @@ portable = "caml_ephe_create"
  let create l =
    if not (0 <= l && l <= max_ephe_length) then
      invalid_arg "Obj.Ephemeron.create";
    create l

  let length x = size(repr x) - additional_values

  let raise_if_invalid_offset e o msg =
    if not (0 <= o && o < length e) then
      invalid_arg msg

  external get_key: t -> int -> obj_t option @@ portable = "caml_ephe_get_key"
  let get_key e o =
    raise_if_invalid_offset e o "Obj.Ephemeron.get_key";
    get_key e o

  external get_key_copy: t -> int -> obj_t option @@ portable = "caml_ephe_get_key_copy"
  let get_key_copy e o =
    raise_if_invalid_offset e o "Obj.Ephemeron.get_key_copy";
    get_key_copy e o

  external set_key: t -> int -> obj_t -> unit @@ portable = "caml_ephe_set_key"
  let set_key e o x =
    raise_if_invalid_offset e o "Obj.Ephemeron.set_key";
    set_key e o x

  external unset_key: t -> int -> unit @@ portable = "caml_ephe_unset_key"
  let unset_key e o =
    raise_if_invalid_offset e o "Obj.Ephemeron.unset_key";
    unset_key e o

  external check_key: t -> int -> bool @@ portable = "caml_ephe_check_key"
  let check_key e o =
    raise_if_invalid_offset e o "Obj.Ephemeron.check_key";
    check_key e o

  external blit_key : t -> int -> t -> int -> int -> unit @@ portable
    = "caml_ephe_blit_key"

  let blit_key e1 o1 e2 o2 l =
    if l < 0 || o1 < 0 || o1 > length e1 - l
       || o2 < 0 || o2 > length e2 - l
    then invalid_arg "Obj.Ephemeron.blit_key"
    else if l <> 0 then blit_key e1 o1 e2 o2 l

  external get_data: t -> obj_t option @@ portable = "caml_ephe_get_data"
  external get_data_copy: t -> obj_t option @@ portable = "caml_ephe_get_data_copy"
  external set_data: t -> obj_t -> unit @@ portable = "caml_ephe_set_data"
  external unset_data: t -> unit @@ portable = "caml_ephe_unset_data"
  external check_data: t -> bool @@ portable = "caml_ephe_check_data"
  external blit_data : t -> t -> unit @@ portable = "caml_ephe_blit_data"

end

module Uniform_or_mixed = struct
  type obj_t = t

  (* In native code, the raw reserved header bits, which is either 0 if the
     block is uniform or n+1 if the block has a scannable prefix of length n.
     In bytecode, this will be size+1 for "faux mixed blocks" representing
     mixed records, and otherwise 0.
   *)
  type t = int

  external of_block : obj_t -> t @@ portable = "caml_succ_scannable_prefix_len" [@@noalloc]

  type repr =
    | Uniform
    | Mixed of { scannable_prefix_len : int }

  let repr = function
    | 0 -> Uniform
    | n -> Mixed { scannable_prefix_len = n - 1 }

  let is_uniform t = t = 0
  let is_mixed t = not (is_uniform t)
  let mixed_scannable_prefix_len_exn t =
    if is_uniform t
    then invalid_arg "Uniform_or_mixed.mixed_scannable_prefix_len_exn";
    t - 1
end

