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

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Types

val extension_descr:
  current_unit:Unit_info.t option -> Path.t -> extension_constructor ->
  constructor_description

val labels_of_type:
  Path.t -> type_declaration ->
  (Ident.t * label_description) list
val unboxed_labels_of_type:
  Path.t -> type_declaration ->
  (Ident.t * unboxed_label_description) list
val constructors_of_type:
  current_unit:Unit_info.t option -> Path.t -> type_declaration ->
  (Ident.t * constructor_description) list


exception Constr_not_found

val find_constr_by_tag:
  constant:bool -> int -> ((constructor_description * 'a) * 'b) list ->
    constructor_description

val constructor_existentials :
    constructor_arguments -> type_expr option -> type_expr list * type_expr list
(** Takes [cd_args] and [cd_res] from a [constructor_declaration] and
    returns:
    - the types of the constructor's arguments
    - the existential variables introduced by the constructor
 *)
