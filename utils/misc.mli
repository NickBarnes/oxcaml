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

(** Miscellaneous useful types and functions

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

(** {1 Reporting fatal errors} *)

val fatal_error: string -> 'a
  (** Raise the [Fatal_error] exception with the given string. *)

val fatal_errorf: ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** Format the arguments according to the given format string
      and raise [Fatal_error] with the resulting string. *)

val unboxed_small_int_arrays_are_not_implemented : unit -> _
  (** Unboxed small int arrays are not implemented. *)

exception Fatal_error

(** {1 Exceptions and finalization} *)

val try_finally :
  ?always:(unit -> unit) ->
  ?exceptionally:(unit -> unit) ->
  (unit -> 'a) -> 'a
(** [try_finally work ~always ~exceptionally] is designed to run code
    in [work] that may fail with an exception, and has two kind of
    cleanup routines: [always], that must be run after any execution
    of the function (typically, freeing system resources), and
    [exceptionally], that should be run only if [work] or [always]
    failed with an exception (typically, undoing user-visible state
    changes that would only make sense if the function completes
    correctly). For example:

    {[
      let objfile = outputprefix ^ ".cmo" in
      let oc = open_out_bin objfile in
      Misc.try_finally
        (fun () ->
           bytecode
           ++ Timings.(accumulate_time (Generate sourcefile))
               (Emitcode.to_file oc modulename objfile);
           Warnings.check_fatal ())
        ~always:(fun () -> close_out oc)
        ~exceptionally:(fun _exn -> remove_file objfile);
    ]}

    If [exceptionally] fail with an exception, it is propagated as
    usual.

    If [always] or [exceptionally] use exceptions internally for
    control-flow but do not raise, then [try_finally] is careful to
    preserve any exception backtrace coming from [work] or [always]
    for easier debugging.
*)

val reraise_preserving_backtrace : exn -> (unit -> unit) -> 'a
(** [reraise_preserving_backtrace e f] is (f (); raise e) except that the
    current backtrace is preserved, even if [f] uses exceptions internally. *)

(** {1 List operations} *)

val map_end: ('a -> 'b) -> 'a list -> 'b list -> 'b list
       (** [map_end f l t] is [map f l @ t], just more efficient. *)

val rev_map_end: ('a -> 'b) -> 'a list -> 'b list -> 'b list
       (** [rev_map_end f l t] is [map f (rev l) @ t], just more efficient. *)

val map_left_right: ('a -> 'b) -> 'a list -> 'b list
       (** Like [List.map], with guaranteed left-to-right evaluation order *)

val for_all2: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
       (** Same as [List.for_all] but for a binary predicate.
           In addition, this [for_all2] never fails: given two lists
           with different lengths, it returns false. *)

val replicate_list: 'a -> int -> 'a list
       (** [replicate_list elem n] is the list with [n] elements
           all identical to [elem]. *)

val list_remove: 'a -> 'a list -> 'a list
       (** [list_remove x l] returns a copy of [l] with the first
           element equal to [x] removed. *)

val split_last: 'a list -> 'a list * 'a
        (** Return the last element and the other elements of the given list. *)

val last : 'a list -> 'a option
        (** Return the last element of a list if it's nonempty *)

(** {1 Hash table operations} *)

val create_hashtable: int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
       (** Create a hashtable with the given initial size and fills it
           with the given bindings. *)

(** {1 Extensions to the standard library} *)

module Stdlib : sig

(** {2 Extensions to the List module} *)
  module List : sig
    type 'a t = 'a list

    val is_empty : 'a list -> bool
    (** [is_empty l] is true if and only if [l] has no elements. It is equivalent to
        [compare_length_with l 0 = 0].  *)

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** The lexicographic order supported by the provided order.
        There is no constraint on the relative lengths of the lists. *)

    val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (** Returns [true] if and only if the given lists have the same length and
        content with respect to the given equality function. *)

    val some_if_all_elements_are_some : 'a option t -> 'a t option
    (** If all elements of the given list are [Some _] then [Some xs]
        is returned with the [xs] being the contents of those [Some]s, with
        order preserved.  Otherwise return [None]. *)

    val map_option : ('a -> 'b option) -> 'a t -> 'b t option
    (** [map_option f l] is [some_if_all_elements_are_some (map f l)], but with
        short circuiting. *)

    val map2_option : ('a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t option

    val map2_prefix : ('a -> 'b -> 'c) -> 'a t -> 'b t -> ('c t * 'b t)
    (** [let r1, r2 = map2_prefix f l1 l2]
        If [l1] is of length n and [l2 = h2 @ t2] with h2 of length n,
        r1 is [List.map2 f l1 h1] and r2 is t2. *)

    val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

    val concat_map2 : ('a -> 'b -> 'c list) -> 'a list -> 'b list -> 'c list
    (** [concat_map2 f l1 l2] gives the same result as [concat (map2 f l1 l2)].
        Tail-recursive. *)

    val iteri2 : (int -> 'a -> 'b -> unit) -> 'a list -> 'b list -> unit
    (** Same as {!List.iter2}, but the function is applied to the index of
        the element as first argument (counting from 0) *)

    val split_at : int -> 'a t -> 'a t * 'a t
    (** [split_at n l] returns the pair [before, after] where [before] is
        the [n] first elements of [l] and [after] the remaining ones.
        If [l] has less than [n] elements, raises Invalid_argument. *)

    val map_sharing : ('a -> 'a) -> 'a t -> 'a t
    (** [map_sharing f l] is [map f l]. If for all elements of the list
        [f e == e] then [map_sharing f l == l] *)

    val fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    (** [fold_lefti f init l] is like [fold_left] but also takes as parameter
        the zero-based index of the element *)

    val fold_left_map2
      : ('acc -> 'a -> 'b -> 'acc * 'r)
      -> 'acc
      -> 'a list
      -> 'b list
      -> 'acc * 'r list
    (** [fold_left_map2] is a combination of [fold_left2] and [map2] that threads an
        accumulator through calls to [f]. *)

    val chunks_of : int -> 'a t -> 'a t t
    (** [chunks_of n t] returns a list of nonempty lists whose
        concatenation is equal to the original list. Every list has [n]
        elements, except for possibly the last list, which may have fewer.
        [chunks_of] raises if [n <= 0]. *)

    val is_prefix
       : equal:('a -> 'a -> bool)
      -> 'a list
      -> of_:'a list
      -> bool
    (** Returns [true] if and only if the given list, with respect to the given
        equality function on list members, is a prefix of the list [of_]. *)

    type 'a longest_common_prefix_result = private {
      longest_common_prefix : 'a list;
      first_without_longest_common_prefix : 'a list;
      second_without_longest_common_prefix : 'a list;
    }

    val find_and_chop_longest_common_prefix
       : equal:('a -> 'a -> bool)
      -> first:'a list
      -> second:'a list
      -> 'a longest_common_prefix_result
    (** Returns the longest list that, with respect to the provided equality
        function, is a prefix of both of the given lists.  The input lists,
        each with such longest common prefix removed, are also returned. *)

     val iter_until_error
       : f:('a -> (unit, 'b) Result.t)
      -> 'a list
      -> (unit, 'b) Result.t
     (** [iter_until_error f l] applies [f] to each element of [l], in order,
         short-circuiting in the case [f] returns [Error] on any element. *)

    val merge_iter
       : cmp:('a -> 'b -> int)
      -> left_only:('a -> unit)
      -> right_only:('b -> unit)
      -> both:('a -> 'b -> unit)
      -> 'a t
      -> 'b t
      -> unit
    (** Iterates over two sorted lists, calling [left_only] on those elements
        that appear only in the left list, [right_only] on those elements
        that appear only in the right list, and [both] on those elements that
        appear in both. *)

    val merge_fold
      : cmp:('a -> 'b -> int)
      -> left_only:('acc -> 'a -> 'acc)
      -> right_only:('acc -> 'b -> 'acc)
      -> both:('acc -> 'a -> 'b -> 'acc)
      -> init:'acc
      -> 'a t
      -> 'b t
      -> 'acc
      (** Folds over two sorted lists, calling [left_only] on those elements
          that appear only in the left list, [right_only] on those elements
          that appear only in the right list, and [both] on those elements that
          appear in both. *)
  end

(** {2 Extensions to the Option module} *)
  module Option : sig
    type 'a t = 'a option

    (* short circuits if the first argument really is a [Some] *)
    val first_some : 'a option -> (unit -> 'a option) -> 'a option

    val print
       : (Format.formatter -> 'a -> unit)
      -> Format.formatter
      -> 'a t
      -> unit
  end

(** {2 Extensions to the Array module} *)
  module Array : sig
    val exists2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
    (** Same as [Array.exists2] from the standard library. *)

    val fold_left2 :
      ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a array -> 'b array -> 'acc
    (** [fold_left2 f init [|a1; ...; an|] [|b1; ...; bn|]] is
        [f (... (f (f init a1 b1) a2 b2) ...) an bn].
        @raise Invalid_argument if the two arrays are determined
        to have different lengths.
    *)

    val for_alli : (int -> 'a -> bool) -> 'a array -> bool
    (** Same as [Array.for_all] from the standard library, but the
        function is applied with the index of the element as first argument,
        and the element itself as second argument. *)

    val all_somes : 'a option array -> 'a array option

    val equal : ('a -> 'a -> bool) -> 'a array -> 'a array -> bool
    (** Compare two arrays for equality, using the supplied predicate for
        element equality *)

    val compare : ('a -> 'a -> int) -> 'a array -> 'a array -> int
    (** Compare two arrays, using the supplied predicate for element equality *)

    val map_sharing : ('a -> 'a) -> 'a array -> 'a array
    (** [map_sharing f a] is [map f a]. If for all elements of the array
        [f e == e] then [map_sharing f a == a] *)

    val of_list_map : ('a -> 'b) -> 'a list -> 'b array

    val concat_arrays : 'a array array -> 'a array
    (** Concatenate an array of arrays into a single array. *)
  end

(** {2 Extensions to the String module} *)
  module String : sig
    include module type of String
    module Set : Set.S with type elt = string
    module Map : sig
      include Map.S with type key = string
      val of_seq_multi : (string * 'a) Seq.t -> 'a list t
    end
    module Tbl : Hashtbl.S with type key = string

    val print : Format.formatter -> t -> unit

    val for_all : (char -> bool) -> t -> bool

    val begins_with : ?from:int -> string -> prefix:string -> bool

    val split_on_string : string -> split_on:string -> string list

    val split_on_chars : string -> split_on:char list -> string list

    (** Splits on the last occurrence of the given character. *)
    val split_last_exn : string -> split_on:char -> string * string

    (** Splits on the first occurence of the given character. *)
    val split_first_exn : string -> split_on:char -> string * string

    val starts_with : prefix:string -> string -> bool
    val ends_with : suffix:string -> string -> bool

    val is_substring : string -> substring:string -> bool
  end

  module Int : sig
    include module type of Int

    val min : t -> t -> t
    val max : t -> t -> t
  end

  external compare : 'a -> 'a -> int = "%compare"

  module Monad : sig
    module type Basic2 = sig
      (** Multi parameter monad. The second parameter gets unified across all
          the computation.  This is used to encode monads working on a multi
          parameter data structure like ([('a,'b) result]). *)

      type ('a, 'e) t

      val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

      val return : 'a -> ('a, _) t

      (** The following identities ought to hold (for some value of =):

          - [return x >>= f = f x]
          - [t >>= fun x -> return x = t]
          - [(t >>= f) >>= g = t >>= fun x -> (f x >>= g)]

          Note: [>>=] is the infix notation for [bind]) *)
    end

    module type Basic = sig
      type 'a t
      include Basic2 with type ('a, _) t := 'a t
    end

    module type S2 = sig
      type ('a, 'e) t

      val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

      (** [>>=] is a synonym for [bind] *)
      val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

      (** [return v] returns the (trivial) computation that returns v. *)
      val return : 'a -> ('a, _) t

      val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t

      (** [join t] is [t >>= (fun t' -> t')]. *)
      val join : (('a, 'e) t, 'e) t -> ('a, 'e) t

      val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

      (** [ignore_m t] is [map (fun _ -> ()) t]. *)
      val ignore_m : (_, 'e) t -> (unit, 'e) t

      val all : ('a, 'e) t list -> ('a list, 'e) t

      (** Like [all], but ensures that every monadic value in the list produces
          a unit value, all of which are discarded rather than being collected
          into a list. *)
      val all_unit : (unit, 'e) t list -> (unit, 'e) t

      (** As described at https://ocaml.org/manual/latest/bindingops.html *)
      module Syntax : sig
        val (let+) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
        val (and+) : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
        val (let*) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
        val (and*) : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
      end
    end

    module type S = sig
      type 'a t
      include S2 with type ('a, _) t := 'a t
    end


    module Make (X : Basic) : S with type 'a t = 'a X.t
    module Make2 (X : Basic2) : S2 with type ('a, 'e) t = ('a, 'e) X.t

    module Identity : S with type 'a t = 'a
    module Option : S with type 'a t = 'a option
    module Result : S2 with type ('a, 'e) t = ('a, 'e) result
  end
end

(** {1 Operations on files and file paths} *)

val find_in_path: string list -> string -> string
       (** Search a file in a list of directories. *)

val find_in_path_rel: string list -> string -> string
       (** Search a relative file in a list of directories. *)

 (** Normalize file name [Foo.ml] to [foo.ml] *)
val normalized_unit_filename: string -> string

val find_in_path_normalized: string list -> string -> string
(** Same as {!find_in_path_rel} , but search also for normalized unit filename,
    i.e. if name is [Foo.ml], allow [/path/Foo.ml] and [/path/foo.ml] to
    match. *)

val remove_file: string -> unit
       (** Delete the given file if it exists and is a regular file.
           Does nothing for other kinds of files.
           Never raises an error. *)

val expand_directory: string -> string -> string
       (** [expand_directory alt file] eventually expands a [+] at the
           beginning of file into [alt] (an alternate root directory) *)

val split_path_contents: ?sep:char -> string -> string list
      (** [split_path_contents ?sep s] interprets [s] as the value of
          a "PATH"-like variable and returns the corresponding list of
          directories. [s] is split using the platform-specific delimiter, or
          [~sep] if it is passed.

          Returns the empty list if [s] is empty. *)

val copy_file: in_channel -> out_channel -> unit
       (** [copy_file ic oc] reads the contents of file [ic] and copies
           them to [oc]. It stops when encountering EOF on [ic]. *)

val copy_file_chunk: in_channel -> out_channel -> int -> unit
       (** [copy_file_chunk ic oc n] reads [n] bytes from [ic] and copies
           them to [oc]. It raises [End_of_file] when encountering
           EOF on [ic]. *)

val string_of_file: in_channel -> string
       (** [string_of_file ic] reads the contents of file [ic] and copies
           them to a string. It stops when encountering EOF on [ic]. *)

val output_to_file_via_temporary:
      ?mode:open_flag list -> string -> (string -> out_channel -> 'a) -> 'a
       (** Produce output in temporary file, then rename it
           (as atomically as possible) to the desired output file name.
           [output_to_file_via_temporary filename fn] opens a temporary file
           which is passed to [fn] (name + output channel).  When [fn] returns,
           the channel is closed and the temporary file is renamed to
           [filename]. *)

val protect_writing_to_file
   : filename:string
  -> f:(out_channel -> 'a)
  -> 'a
      (** Open the given [filename] for writing (in binary mode), pass
          the [out_channel] to the given function, then close the
          channel. If the function raises an exception then [filename]
          will be removed. *)

val concat_null_terminated : string list -> string
(** [concat_null_terminated [x1;x2; ... xn]] is
    [x1 ^ "\000" ^ x2 ^ "\000" ^ ... ^ xn ^ "\000"] *)

val split_null_terminated : string -> string list
(** [split_null_terminated s] is similar
    [String.split_on_char '\000'] but ignores the trailing separator, if any *)

val chop_extensions: string -> string
       (** Return the given file name without its extensions. The extensions
           is the longest suffix starting with a period and not including
           a directory separator, [.xyz.uvw] for instance.

           Return the given name if it does not contain an extension. *)

(** {1 Integer operations} *)

val log2: int -> int
       (** [log2 n] returns [s] such that [n = 1 lsl s]
           if [n] is a power of 2*)

val log2_nativeint: nativeint -> int
(** [log2_nativeint n] computes [floor (log2 n)] when [ n > 0 ].
    If [n] is also a power of 2, the result [s] satisfies
    [n = Nativeint.shift_left 1n s]
*)

val power : base:int -> int -> int
(** [power ~base x] computes [base**x]. *)

val align: int -> int -> int
       (** [align n a] rounds [n] upwards to a multiple of [a]
           (a power of 2). *)

val no_overflow_add: int -> int -> bool
       (** [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 + n2] does not overflow. *)

val no_overflow_sub: int -> int -> bool
       (** [no_overflow_sub n1 n2] returns [true] if the computation of
           [n1 - n2] does not overflow. *)

val no_overflow_mul: int -> int -> bool
       (** [no_overflow_mul n1 n2] returns [true] if the computation of
           [n1 * n2] does not overflow. *)

val no_overflow_lsl: int -> int -> bool
       (** [no_overflow_lsl n k] returns [true] if the computation of
           [n lsl k] does not overflow. *)

val letter_of_int : int -> string

module Int_literal_converter : sig
  val int : string -> int
    (** Convert a string to an integer.  Unlike {!Stdlib.int_of_string},
        this function accepts the string representation of [max_int + 1]
        and returns [min_int] in this case. *)

  val int32 : string -> int32
    (** Likewise, at type [int32] *)

  val int64 : string -> int64
    (** Likewise, at type [int64] *)

  val nativeint : string -> nativeint
    (** Likewise, at type [nativeint] *)

end

val find_first_mono : (int -> bool) -> int
  (**[find_first_mono p] takes an integer predicate [p : int -> bool]
     that we assume:
     1. is monotonic on natural numbers:
        if [a <= b] then [p a] implies [p b],
     2. is satisfied for some natural numbers in range [0; max_int]
        (this is equivalent to: [p max_int = true]).

     [find_first_mono p] is the smallest natural number N that satisfies [p],
     computed in O(log(N)) calls to [p].

     Our implementation supports two cases where the preconditions on [p]
     are not respected:
     - If [p] is always [false], we silently return [max_int]
       instead of looping or crashing.
     - If [p] is non-monotonic but eventually true,
       we return some satisfying value.
  *)

(** {1 String operations} *)

val search_substring: string -> string -> int -> int
       (** [search_substring pat str start] returns the position of the first
           occurrence of string [pat] in string [str].  Search starts
           at offset [start] in [str].  Raise [Not_found] if [pat]
           does not occur. *)

val replace_substring: before:string -> after:string -> string -> string
       (** [replace_substring ~before ~after str] replaces all
           occurrences of [before] with [after] in [str] and returns
           the resulting string. *)

val rev_split_words: string -> string list
       (** [rev_split_words s] splits [s] in blank-separated words, and returns
           the list of words in reverse order. *)

val cut_at : string -> char -> string * string
(** [String.cut_at s c] returns a pair containing the sub-string before
   the first occurrence of [c] in [s], and the sub-string after the
   first occurrence of [c] in [s].
   [let (before, after) = String.cut_at s c in
    before ^ String.make 1 c ^ after] is the identity if [s] contains [c].

   Raise [Not_found] if the character does not appear in the string
   @since 4.01
*)

val ordinal_suffix : int -> string
(** [ordinal_suffix n] is the appropriate suffix to append to the numeral [n] as
    an ordinal number: [1] -> ["st"], [2] -> ["nd"], [3] -> ["rd"],
    [4] -> ["th"], and so on.  Handles larger numbers (e.g., [42] -> ["nd"]) and
    the numbers 11--13 (which all get ["th"]) correctly. *)

val format_as_unboxed_literal : string -> string
(** [format_as_unboxed_literal constant_literal] converts [constant_literal] to its
    corresponding unboxed literal by either adding "#" in front or changing
    "-" to "-#".

    Examples:

      [0.1] to [#0.1]
      [-3] to [-#3]
      [0xa.cp-1] to [#0xa.cp-1] *)

val normalise_eol : string -> string
(** [normalise_eol s] returns a fresh copy of [s] with any '\r' characters
   removed. Intended for pre-processing text which will subsequently be printed
   on a channel which performs EOL transformations (i.e. Windows) *)

val delete_eol_spaces : string -> string
(** [delete_eol_spaces s] returns a fresh copy of [s] with any end of
   line spaces removed. Intended to normalize the output of the
   toplevel for tests. *)

(** {1 Operations on references} *)

type ref_and_value = R : 'a ref * 'a -> ref_and_value

val protect_refs : ref_and_value list -> (unit -> 'a) -> 'a
(** [protect_refs l f] temporarily sets [r] to [v] for each [R (r, v)] in [l]
    while executing [f]. The previous contents of the references is restored
    even if [f] raises an exception, without altering the exception backtrace.
*)

val get_ref: 'a list ref -> 'a list
       (** [get_ref lr] returns the content of the list reference [lr] and reset
           its content to the empty list. *)

val set_or_ignore : ('a -> 'b option) -> 'b option ref -> 'a -> unit
       (** [set_or_ignore f opt x] sets [opt] to [f x] if it returns [Some _],
           or leaves it unmodified if it returns [None]. *)

(** {1 Operations on triples and quadruples} *)

val fst3: 'a * 'b * 'c -> 'a
val snd3: 'a * 'b * 'c -> 'b
val thd3: 'a * 'b * 'c -> 'c

val fst4: 'a * 'b * 'c * 'd -> 'a
val snd4: 'a * 'b * 'c * 'd -> 'b
val thd4: 'a * 'b * 'c * 'd -> 'c
val for4: 'a * 'b * 'c * 'd -> 'd

(** {1 Long strings} *)

(** ``Long strings'' are mutable arrays of characters that are not limited
    in length to {!Sys.max_string_length}. *)

module LongString :
  sig
    type t = bytes array
    val create : int -> t
    val length : t -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val blit : t -> int -> t -> int -> int -> unit
    val output : out_channel -> t -> int -> int -> unit
    val input_bytes : in_channel -> int -> t
  end

(** {1 Spell checking and ``did you mean'' suggestions} *)

val edit_distance : string -> string -> int -> int option
(** [edit_distance a b cutoff] computes the edit distance between
    strings [a] and [b]. To help efficiency, it uses a cutoff: if the
    distance [d] is smaller than [cutoff], it returns [Some d], else
    [None].

    The distance algorithm currently used is Damerau-Levenshtein: it
    computes the number of insertion, deletion, substitution of
    letters, or swapping of adjacent letters to go from one word to the
    other. The particular algorithm may change in the future.
*)

val spellcheck : string list -> string -> string list
(** [spellcheck env name] takes a list of names [env] that exist in
    the current environment and an erroneous [name], and returns a
    list of suggestions taken from [env], that are close enough to
    [name] that it may be a typo for one of them. *)

val did_you_mean : Format.formatter -> (unit -> string list) -> unit
(** [did_you_mean ppf get_choices] hints that the user may have meant
    one of the option returned by calling [get_choices]. It does nothing
    if the returned list is empty.

    The [unit -> ...] thunking is meant to delay any potentially-slow
    computation (typically computing edit-distance with many things
    from the current environment) to when the hint message is to be
    printed. You should print an understandable error message before
    calling [did_you_mean], so that users get a clear notification of
    the failure even if producing the hint is slow.
*)

(** {1 Color support detection }*)
module Color: sig

  type setting = Auto | Always | Never

  val default_setting : setting

end


(** {1 Styling handling for terminal output } *)

module Style : sig
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset
  type Format.stag += Style of style list

  val ansi_of_style_l : style list -> string
  (* ANSI escape sequence for the given style *)

  type tag_style ={
    ansi: style list;
    text_open:string;
    text_close:string
  }

  type styles = {
    error: tag_style;
    warning: tag_style;
    loc: tag_style;
    hint: tag_style;
    inline_code: tag_style;
  }

  val as_inline_code: (Format.formatter -> 'a -> unit as 'printer) -> 'printer
  val inline_code: Format.formatter -> string -> unit

  val as_clflag:
    string -> (Format.formatter -> 'a -> unit as 'printer) -> 'printer

  val default_styles: styles
  val get_styles: unit -> styles
  val set_styles: styles -> unit

  val setup : Color.setting option -> unit
  (* [setup opt] will enable or disable color handling on standard formatters
     according to the value of color setting [opt].
     Only the first call to this function has an effect. *)

  val set_tag_handling : Format.formatter -> unit
  (* adds functions to support color tags to the given formatter. *)
end

(* See the -error-style option *)
module Error_style : sig
  type setting =
    | Contextual
    | Short

  val default_setting : setting
end

(** {1 Formatted output} *)

val print_if :
  Format.formatter -> bool ref -> (Format.formatter -> 'a -> unit) -> 'a -> 'a
(** [print_if ppf flag fmt x] prints [x] with [fmt] on [ppf]
    if [flag] is true. *)

val pp_two_columns :
  ?sep:string -> ?max_lines:int ->
  Format.formatter -> (string * string) list -> unit
(** [pp_two_columns ?sep ?max_lines ppf l] prints the lines in [l] as two
   columns separated by [sep] ("|" by default). [max_lines] can be used to
   indicate a maximum number of lines to print -- an ellipsis gets inserted at
   the middle if the input has too many lines.

   Example:

    {v pp_two_columns ~max_lines:3 Format.std_formatter [
      "abc", "hello";
      "def", "zzz";
      "a"  , "bllbl";
      "bb" , "dddddd";
    ] v}

    prints

    {v
    abc | hello
    ...
    bb  | dddddd
    v}
*)

val pp_table : Format.formatter -> (string * string list) list -> unit
(** [pp_table ppf l] prints the table [l], a list of columns with their
    header. The function fails with a fatal error if the columns have
    different length. *)

val pp_parens_if :
     bool
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a
  -> unit
(** [pp_parens_if bool formatter ppf arg] prints [formatter ppf arg], wrapping it with
    [()] if [bool] is true. *)

val pp_nested_list :
     nested:bool
  -> pp_element:(nested:bool -> Format.formatter -> 'a -> unit)
  -> pp_sep:(Format.formatter -> unit -> unit)
  -> Format.formatter
  -> 'a list
  -> unit
(** [pp_nested_list ~nested ~pp_element ~pp_sep ppf args] prints the list [args]
    with [pp_element] on [ppf]. The elements are separated by [pp_sep]. If
    [~nested] is true, the list is wrapped in parens. The element printer is
    always called with [nested:true], indicating that any inner lists are nested
    and need parens. *)

val print_see_manual : Format.formatter -> int list -> unit
(** See manual section *)

val output_of_print :
  (Format.formatter -> 'a -> unit) -> out_channel -> 'a -> unit
(** [output_of_print print] produces an output function from a pretty printer.
    Note that naively using [Format.formatter_of_out_channel] typechecks but
    doesn't work because it fails to flush the formatter. *)

val is_print_longer_than: int -> (Format.formatter -> unit) -> bool
(** Returns [true] if the printed string is longer than the given integer. Stops
    early if so. Spaces and newlines are counted, but indentation is not. *)

val to_string_of_print :
  (Format.formatter -> 'a -> unit) -> 'a -> string
(** [to_string_of_print print] produces a string conversion function from a
    pretty printer. This is similar but preferable to [Format.asprintf "%a"]
    when the output may be large, since [to_string] functions don't usually
    return embedded newlines. *)

(** {1 Displaying configuration variables} *)

val show_config_and_exit : unit -> unit
  (** Display the values of all compiler configuration variables from module
      [Config], then exit the program with code 0. *)

val show_config_variable_and_exit : string -> unit
  (** Display the value of the given configuration variable,
      then exit the program with code 0. *)

(** {1 Handling of build maps} *)

(** Build maps cause the compiler to normalize file names embedded in
    object files, thus leading to more reproducible builds. *)

val get_build_path_prefix_map: unit -> Build_path_prefix_map.map option
(** Returns the map encoded in the [BUILD_PATH_PREFIX_MAP] environment
    variable. *)

val debug_prefix_map_flags: unit -> string list
(** Returns the list of [--debug-prefix-map] flags to be passed to the
    assembler, built from the [BUILD_PATH_PREFIX_MAP] environment variable. *)

module Bitmap : sig
  type t
  val make : int -> t
  val set : t -> int -> unit
  val clear : t -> int -> unit
  val get : t -> int -> bool
  val iter : (int -> unit) -> t -> unit
end

(** {1 Handling of magic numbers} *)

module Magic_number : sig
  (** a typical magic number is "Caml1999I011"; it is formed of an
      alphanumeric prefix, here Caml1990I, followed by a version,
      here 011. The prefix identifies the kind of the versioned data:
      here the I indicates that it is the magic number for .cmi files.

      All magic numbers have the same byte length, [magic_length], and
      this is important for users as it gives them the number of bytes
      to read to obtain the byte sequence that should be a magic
      number. Typical user code will look like:
      {[
        let ic = open_in_bin path in
        let magic =
          try really_input_string ic Magic_number.magic_length
          with End_of_file -> ... in
        match Magic_number.parse magic with
        | Error parse_error -> ...
        | Ok info -> ...
      ]}

      A given compiler version expects one specific version for each
      kind of object file, and will fail if given an unsupported
      version. Because versions grow monotonically, you can compare
      the parsed version with the expected "current version" for
      a kind, to tell whether the wrong-magic object file comes from
      the past or from the future.

      An example of code block that expects the "currently supported version"
      of a given kind of magic numbers, here [Cmxa], is as follows:
      {[
        let ic = open_in_bin path in
        begin
          try Magic_number.(expect_current Cmxa (get_info ic)) with
          | Parse_error error -> ...
          | Unexpected error -> ...
        end;
        ...
      ]}

      Parse errors distinguish inputs that are [Not_a_magic_number str],
      which are likely to come from the file being completely
      different, and [Truncated str], raised by headers that are the
      (possibly empty) prefix of a valid magic number.

      Unexpected errors correspond to valid magic numbers that are not
      the one expected, either because it corresponds to a different
      kind, or to a newer or older version.

      The helper functions [explain_parse_error] and [explain_unexpected_error]
      will generate a textual explanation of each error,
      for use in error messages.

      @since 4.11
  *)

  type version = int

  type kind =
    | Exec
    | Cmi | Cmo | Cma
    | Cmx | Cmxa
    | Cmxs
    | Cmt | Cms | Ast_impl | Ast_intf

  type info = {
    kind: kind;
    version: version;
    (** Note: some versions of the compiler use the same [version] suffix
        for all kinds, but others use different versions counters for different
        kinds. We may only assume that versions are growing monotonically
        (not necessarily always by one) between compiler versions. *)
  }

  type raw = string
  (** the type of raw magic numbers,
      such as "Caml1999A027" for the .cma files of OCaml 4.10 *)

  (** {3 Parsing magic numbers} *)

  type parse_error =
    | Truncated of string
    | Not_a_magic_number of string

  val explain_parse_error : kind option -> parse_error -> string
  (** Produces an explanation for a parse error. If no kind is provided,
      we use an unspecific formulation suggesting that any compiler-produced
      object file would have been satisfying. *)

  val parse : raw -> (info, parse_error) result
  (** Parses a raw magic number *)

  val read_info : in_channel -> (info, parse_error) result
  (** Read a raw magic number from an input channel.

      If the data read [str] is not a valid magic number, it can be
      recovered from the [Truncated str | Not_a_magic_number str]
      payload of the [Error parse_error] case.

      If parsing succeeds with an [Ok info] result, we know that
      exactly [magic_length] bytes have been consumed from the
      input_channel.

      If you also wish to enforce that the magic number
      is at the current version, see {!read_current_info} below.
   *)

  val magic_length : int
  (** all magic numbers take the same number of bytes *)


  (** {3 Checking that magic numbers are current} *)

  type 'a unexpected = { expected : 'a; actual : 'a }
  type unexpected_error =
    | Kind of kind unexpected
    | Version of kind * version unexpected

  val check_current : kind -> info -> (unit, unexpected_error) result
  (** [check_current kind info] checks that the provided magic [info]
      is the current version of [kind]'s magic header. *)

  val explain_unexpected_error : unexpected_error -> string
  (** Provides an explanation of the [unexpected_error]. *)

  type error =
    | Parse_error of parse_error
    | Unexpected_error of unexpected_error

  val read_current_info :
    expected_kind:kind option -> in_channel -> (info, error) result
  (** Read a magic number as [read_info],
      and check that it is the current version as its kind.
      If the [expected_kind] argument is [None], any kind is accepted. *)


  (** {3 Information on magic numbers} *)

  val string_of_kind : kind -> string
  (** a user-printable string for a kind, eg. "exec" or "cmo", to use
      in error messages. *)

  val human_name_of_kind : kind -> string
  (** a user-meaningful name for a kind, eg. "executable file" or
      "bytecode object file", to use in error messages. *)

  val current_raw : kind -> raw
  (** the current magic number of each kind *)

  val current_version : kind -> version
  (** the current version of each kind *)


  (** {3 Raw representations}

      Mainly for internal usage and testing. *)

  type raw_kind = string
  (** the type of raw magic numbers kinds,
      such as "Caml1999A" for .cma files *)

  val parse_kind : raw_kind -> kind option
  (** parse a raw kind into a kind *)

  val raw_kind : kind -> raw_kind
  (** the current raw representation of a kind.

      In some cases the raw representation of a kind has changed
      over compiler versions, so other files of the same kind
      may have different raw kinds.
      Note that all currently known cases are parsed correctly by [parse_kind].
  *)

  val raw : info -> raw
  (** A valid raw representation of the magic number.

      Due to past and future changes in the string representation of
      magic numbers, we cannot guarantee that the raw strings returned
      for past and future versions actually match the expectations of
      those compilers. The representation is accurate for current
      versions, and it is correctly parsed back into the desired
      version by the parsing functions above.
   *)

  val all_kinds : kind list
end

(** The result of a less-than-or-equal comparison *)
module Le_result : sig
  type t =
    | Equal
    | Less
    | Not_le

  val combine : t -> t -> t
  val combine_list : t list -> t

  val is_le : t -> bool
  val is_equal : t -> bool

  (* adaptors for structures that can only compare less-or-equal *)
  val less_or_equal : le:('a -> 'a -> bool) -> 'a -> 'a -> t
  val equal : le:('a -> 'a -> bool) -> 'a -> 'a -> bool
end

(** Propositional equality *)
type (_, _) eq = Refl : ('a, 'a) eq

(** Utilities for module-level programming *)
module type T = sig
  type t
end

module type T1 = sig
  type 'a t
end

module type T2 = sig
  type ('a, 'b) t
end

module type T3 = sig
  type ('a, 'b, 'c) t
end

module type T4 = sig
  type ('a, 'b, 'c, 'd) t
end

(** {1 Miscellaneous type aliases} *)

type filepath = string

type alerts = string Stdlib.String.Map.t

val remove_double_underscores : string -> string

(** Non-empty lists *)
module Nonempty_list : sig
  type nonrec 'a t = ( :: ) of 'a * 'a list

  val to_list : 'a t -> 'a list
  val of_list_opt : 'a list -> 'a t option
  val map : ('a -> 'b) -> 'a t -> 'b t

  val pp_print :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit

  val (@) : 'a t -> 'a t -> 'a t
end
