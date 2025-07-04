(* The below code recursively enumerates all mixed record types,
   takes a finite prefix of that, and prints a program that
   uses the mixed record types in interesting ways that exercise
   corners of the runtime. (Especially polymorphic operations,
   copying, and garbage collection.)

   It is used in [test.ml].
*)

let () = Random.init 1234567;;

let shuffle : 'a list -> 'a list = fun l ->
  l
  |> Array.of_list
  |> (fun arr -> Array.shuffle ~rand:Random.int arr; arr)
  |> Array.to_list
;;

let printf = Printf.printf
let sprintf = Printf.sprintf

module List = ListLabels
module String = StringLabels

let list_product xs ys =
  List.concat_map xs ~f:(fun x ->
    List.map ys ~f:(fun y -> (x, y)))

module Nonempty_list = struct
  type 'a t = ( :: ) of 'a * 'a list

  let of_list = function
    | [] -> None
    | x :: xs -> Some (x :: xs)
  ;;

  let to_list (x :: xs) : _ list = x :: xs

  let map t ~f = of_list (List.map (to_list t) ~f) |> Option.get
end

let rec list_enumeration enumeration_of_x =
  Seq.cons [] (fun () ->
    let f =
      Seq.product (list_enumeration enumeration_of_x) enumeration_of_x
      |> Seq.map (fun (xs, x) -> x :: xs)
    in
    f ())
;;

(* The difference between [list_enumeration (List.to_seq xs)]
   and [list_enumeration_of_list xs] is that [list_enumeration_of_list xs]
   more eagerly cycles through elements of [xs] (given that it knows it's
   finite). [list_enumeration] has to round-robin because the sequence
   it's given might be infinite.
*)
let rec list_enumeration_of_list list =
  Seq.cons [] (fun () ->
    let f =
      list_enumeration_of_list list
      |> Seq.concat_map (fun xs ->
          List.to_seq (List.map list ~f:(fun x -> x :: xs)))
    in
    f ())
;;

let nonempty_list_enumeration enumeration_of_x =
  list_enumeration enumeration_of_x |> Seq.filter_map Nonempty_list.of_list
;;

let nonempty_list_enumeration_of_list xs =
  list_enumeration_of_list xs |> Seq.filter_map Nonempty_list.of_list
;;

type flat_element =
  | Float64
  | Float32
  | Float
  | Bits32
  | Bits64
  | Word
  | Vec128
  | Vec256

let allowed_in_flat_float_block = function
  | Float64 | Float -> true
  | Float32 | Bits32 | Bits64 | Word | Vec128 | Vec256 -> false

let flat_element_is_unboxed = function
  | Float64 | Float32 | Bits32 | Bits64 | Word | Vec128 | Vec256 -> true
  | Float -> false

let flat_element_is = ((=) : flat_element -> flat_element -> bool)
let flat_element_is_not = ((<>) : flat_element -> flat_element -> bool)

let all_flat_elements_bytecode = [ Float64; Float32; Float; Bits32; Bits64; Word ]

let all_of_flat_element ~bytecode = if bytecode then all_flat_elements_bytecode else Vec256 :: Vec128 :: all_flat_elements_bytecode

type value_element =
  | Str
  | Float
  | Imm

let value_element_is = ((=) : value_element -> value_element -> bool)

let all_of_value_element = [ Str; Float; Imm ]

type mutability =
  | Mutable
  | Immutable

let all_of_mutability = [ Mutable; Immutable ]

let is_mutable = function
  | Mutable -> true
  | Immutable -> false
;;

type prefix = (value_element * mutability) list
type suffix = (flat_element * mutability) Nonempty_list.t

let enumeration_of_prefix all_of_mutability =
  list_enumeration_of_list
    (list_product all_of_value_element all_of_mutability)
  |> Seq.filter (fun prefix ->
    match List.rev prefix with
    | [] -> true
    | (Imm, _) :: _ -> false
    | ((Str | Float), _) :: _ -> true)
;;

let enumeration_of_suffix_except_all_floats_mixed ~bytecode
    all_of_mutability
  : _ Seq.t
  =
  nonempty_list_enumeration_of_list
    (list_product (all_of_flat_element ~bytecode) all_of_mutability)
  |> Seq.filter (fun suffix ->
    List.exists (Nonempty_list.to_list suffix) ~f:(fun (elem, _) ->
      flat_element_is_unboxed elem))
;;

let enumeration_of_all_floats_mixed_suffix ~bytecode =
  let float_flat_element =
    all_of_flat_element ~bytecode
    |> List.filter ~f:allowed_in_flat_float_block
  in
  nonempty_list_enumeration_of_list
    (list_product float_flat_element all_of_mutability)
  |> Seq.filter (fun suffix ->
      let suffix = Nonempty_list.to_list suffix in
      List.exists suffix ~f:(fun (elem, _) -> flat_element_is Float64 elem)
      && List.exists suffix ~f:(fun (elem, _) -> flat_element_is Float elem))

type block =
  { prefix : prefix
  ; suffix : suffix
  }

let enumeration_of_mixed_blocks_except_all_floats_mixed ~bytecode
    all_of_mutability
  =
  Seq.product
   (enumeration_of_prefix all_of_mutability)
   (enumeration_of_suffix_except_all_floats_mixed ~bytecode all_of_mutability)
  |> Seq.filter (fun (prefix, suffix) ->
      let all_float_u_suffix =
        List.for_all (Nonempty_list.to_list suffix)
          ~f:(fun (elem, _) -> flat_element_is Float64 elem)
      in
      let all_float_prefix =
        List.for_all prefix
          ~f:(fun (elem, _) -> value_element_is Float elem)
      in
      not all_float_u_suffix || not all_float_prefix)

type constructor =
  | Cstr_tuple of
      { cstr_prefix : value_element list;
        cstr_suffix : flat_element Nonempty_list.t;
      }
  | Cstr_record of block

type variant = constructor Nonempty_list.t

let enumeration_of_cstr_tuples ~bytecode =
  let all_of_mutability = [ `Mutability_not_relevant ] in
  Seq.product
    (enumeration_of_prefix all_of_mutability)
    (enumeration_of_suffix_except_all_floats_mixed ~bytecode all_of_mutability)
  |> Seq.map (fun (prefix, suffix) ->
      let ignore_mut (x, `Mutability_not_relevant) = x in
      Cstr_tuple
        { cstr_prefix = List.map prefix ~f:ignore_mut;
          cstr_suffix = Nonempty_list.map suffix ~f:ignore_mut;
        })

let enumeration_of_cstr_records ~bytecode =
  Seq.product
    (enumeration_of_prefix all_of_mutability)
    (enumeration_of_suffix_except_all_floats_mixed ~bytecode all_of_mutability)
  |> Seq.map (fun (prefix, suffix) ->
      Cstr_record { prefix; suffix })

let enumeration_of_mixed_variants ~bytecode =
  Seq.interleave (enumeration_of_cstr_tuples ~bytecode) (enumeration_of_cstr_records ~bytecode)
  |> nonempty_list_enumeration

let enumeration_of_mixed_blocks_except_all_floats_mixed ~bytecode =
  enumeration_of_mixed_blocks_except_all_floats_mixed ~bytecode
    all_of_mutability
  |> Seq.map (fun (prefix, suffix) -> { prefix; suffix })


let enumeration_of_all_floats_mixed_blocks ~bytecode =
  enumeration_of_all_floats_mixed_suffix ~bytecode
  |> Seq.map (fun suffix -> { prefix = []; suffix })
;;

type field_or_arg_type =
  | Imm
  | Float
  | Float64
  | Float32
  | Str
  | Bits32
  | Bits64
  | Word
  | Vec128
  | Vec256

let type_to_creation_function = function
  | Imm -> "create_int ()"
  | Float -> "create_float ()"
  | Float64 -> "create_float_u ()"
  | Float32 -> "create_float32_u ()"
  | Bits32 -> "create_int32_u ()"
  | Bits64 -> "create_int64_u ()"
  | Word -> "create_nativeint_u ()"
  | Str -> "create_string ()"
  | Vec128 -> "create_int64x2 ()"
  | Vec256 -> "create_int64x4 ()"

let type_to_string = function
  | Imm -> "int"
  | Float -> "float"
  | Float64 -> "float#"
  | Float32 -> "float32#"
  | Bits32 -> "int32#"
  | Bits64 -> "int64#"
  | Word -> "nativeint#"
  | Str -> "string"
  | Vec128 -> "int64x2#"
  | Vec256 -> "int64x4#"

let type_to_field_integrity_check type_ ~access1 ~access2 ~message =
  let checker, transformation =
    match type_ with
    | Str -> "check_string", None
    | Imm -> "check_int", None
    | Float -> "check_float", None
    | Float64 -> "check_float", Some "Stdlib_upstream_compatible.Float_u.to_float"
    | Float32 -> "check_float32", Some "Stdlib_stable.Float32_u.to_float32"
    | Bits32 -> "check_int32", Some "Stdlib_upstream_compatible.Int32_u.to_int32"
    | Bits64 -> "check_int64", Some "Stdlib_upstream_compatible.Int64_u.to_int64"
    | Word -> "check_int", Some "Stdlib_upstream_compatible.Nativeint_u.to_int"
    | Vec128 -> "check_int64x2", Some "box_int64x2"
    | Vec256 -> "check_int64x4", Some "box_int64x4"
  in
  let transform access =
    match transformation with
    | None -> access
    | Some f -> sprintf "(%s %s)" f access
  in
  sprintf
    "%s %s %s ~message:\"%s\";"
    checker
    (transform access1)
    (transform access2)
    (String.escaped message)

let value_element_to_type : value_element -> field_or_arg_type = function
  | Imm -> Imm
  | Float -> Float
  | Str -> Str

let flat_element_to_type : flat_element -> field_or_arg_type = function
  | Float -> Float
  | Float64 -> Float64
  | Float32 -> Float32
  | Bits64 -> Bits64
  | Bits32 -> Bits32
  | Word -> Word
  | Vec128 -> Vec128
  | Vec256 -> Vec256

module Mixed_record = struct
  type field =
    { type_ : field_or_arg_type
    ; name : string
    ; mutable_ : bool
    }

  type t =
    { index : int
    ; fields : field list
    }

  let is_all_floats t =
    List.for_all t.fields ~f:(fun field ->
        match field.type_ with
        | Imm | Str | Float32 | Bits32 | Bits64 | Word | Vec128 | Vec256 -> false
        | Float | Float64 -> true)

  let of_block index { prefix; suffix } =
    let num_fields, prefix_fields =
      List.fold_left_map
        prefix
        ~init:0
        ~f:(fun i ((elem : value_element), mutability) ->
          let mutable_ = is_mutable mutability in
          let name =
            match elem with
            | Imm -> "imm"
            | Float -> "float"
            | Str -> "str"
          in
          let field =
            { type_ = value_element_to_type elem;
              name = sprintf "%s%i" name i;
              mutable_;
            }
          in
          i+1, field)
    in
    let _, suffix_fields =
      List.fold_left_map
        (Nonempty_list.to_list suffix)
        ~init:num_fields
        ~f:(fun i ((elem : flat_element), mutability) ->
          let mutable_ = is_mutable mutability in
          let name =
            match elem with
            | Bits32 -> "i32_"
            | Bits64 -> "i64_"
            | Word -> "n"
            | Float -> "float"
            | Float64 -> "float_u"
            | Float32 -> "float32_u"
            | Vec128 -> "int64x2_u"
            | Vec256 -> "int64x4_u"
          in
          let field =
            { type_ = flat_element_to_type elem;
              name = sprintf "%s%i" name i;
              mutable_;
            }
          in
          i+1, field)
    in
    let fields = shuffle (prefix_fields @ suffix_fields) in
    { fields; index }
  ;;

  let value ?(base = "t") { index; _ } = sprintf "%s%d" base index
  let type_ { index; _ } = sprintf "t%d" index

  let fields_to_type_decl fields =
    String.concat
      ~sep:"; "
      (List.map fields ~f:(fun { type_; name; mutable_ } ->
           sprintf
             "%s%s : %s"
             (if mutable_ then "mutable " else "")
             name
             (type_to_string type_)))

  let type_decl t =
    sprintf
      "type %s = { %s }"
      (type_ t)
      (fields_to_type_decl t.fields)
  ;;

  let record_value_of_fields fields =
    String.concat
      ~sep:"; "
      (List.map fields ~f:(fun { type_; name; mutable_ = _ } ->
         sprintf
           "%s = %s"
           name
           (type_to_creation_function type_)))
    |> sprintf "{ %s }"
  ;;

  let record_value t = record_value_of_fields t.fields

  let check_field_integrity t =
    List.map t.fields ~f:(fun field ->
      type_to_field_integrity_check
        field.type_
        ~access1:(sprintf "%s.%s" (value t) field.name)
        ~access2:(sprintf "%s.%s" (value t ~base:"t_orig") field.name)
        ~message:(sprintf "%s.%s" (value t) field.name))
  ;;
end

module Mixed_variant = struct
  type args =
    | Args_tuple of field_or_arg_type list
    | Args_record of Mixed_record.field list

  type constructor =
    { name : string;
      index : int;
      args : args;
    }

  type t =
    { index : int
    ; constructors : constructor list
    }

  let of_constructor name cstr index =
    let args =
      match cstr with
      | Cstr_tuple { cstr_prefix; cstr_suffix } ->
          let prefix_args = List.map cstr_prefix ~f:value_element_to_type in
          let suffix_args =
            List.map
              (Nonempty_list.to_list cstr_suffix)
              ~f:flat_element_to_type
          in
          Args_tuple (shuffle (prefix_args @ suffix_args))
      | Cstr_record record ->
          let { fields; _ } : Mixed_record.t = Mixed_record.of_block index record in
          Args_record fields
    in
    { name; args; index }
  ;;

  let of_variant index cstrs =
    let constructors =
      Nonempty_list.to_list cstrs
      |> List.mapi ~f:(fun i cstr ->
        let name = sprintf "%c" (Char.chr (Char.code 'A' + i)) in
        of_constructor name cstr i)
    in
    { index; constructors; }

  let constructor_decl cstr =
    sprintf
      "  | %s of %s"
      cstr.name
      (match cstr.args with
       | Args_tuple args ->
           String.concat ~sep:" * " (List.map args ~f:type_to_string)
       | Args_record fields ->
           sprintf "{ %s }" (Mixed_record.fields_to_type_decl fields))
  ;;

  let type_ { index; _ } = sprintf "t%d" index
  let value ?(base = "t") { name; _ } ~index = sprintf "%s%d_%s" base index name

  let constructor_value { name; args } =
    sprintf "(%s %s)"
      name
      (match args with
       | Args_record fields -> Mixed_record.record_value_of_fields fields
       | Args_tuple args ->
           let args_str =
             String.concat
               ~sep:", "
               (List.map args ~f:type_to_creation_function)
           in
           match args with
           | [] -> args_str
           | _ -> sprintf "(%s)" args_str)

  let type_decl t =
    sprintf
      "type %s =\n%s"
      (type_ t)
      (String.concat ~sep:"\n" (List.map t.constructors ~f:constructor_decl))
  ;;

  let check_field_integrity cstr ~index ~catchall =
    let value = value ~index in
    let arg_var i ~base = sprintf "%s%i" base i in
    let arg_vars cstr ~base =
      match cstr.args with
      | Args_record _ -> base
      | Args_tuple args ->
          sprintf "(%s)"
            (String.concat ~sep:", "
              (List.mapi args ~f:(fun i _ ->
                  arg_var i ~base)))
    in
    sprintf {|let () = match %s, %s with
      | %s %s, %s %s -> %s
      %s
    in|}
    (value cstr)
    (value cstr ~base:"t_orig")
    cstr.name (arg_vars cstr ~base:"a")
    cstr.name (arg_vars cstr ~base:"b")
    (String.concat ~sep:"\n"
       (match cstr.args with
        | Args_record fields ->
            List.map fields ~f:(fun (field : Mixed_record.field) ->
                type_to_field_integrity_check
                  field.type_
                  ~access1:(sprintf "a.%s" field.name)
                  ~access2:(sprintf "b.%s" field.name)
                  ~message:(sprintf "%s.%s" (value cstr) field.name))
        | Args_tuple args ->
            List.mapi args ~f:(fun i type_ ->
                type_to_field_integrity_check
                  type_
                  ~access1:(arg_var i ~base:"a")
                  ~access2:(arg_var i ~base:"b")
                  ~message:(sprintf "%s.%i" (value cstr) i))))
    (if catchall then "| _ -> assert false" else "")
  ;;
end

module Value = struct
  type t =
    | Record of Mixed_record.t
    | Constructor of Mixed_variant.t * Mixed_variant.constructor

  let value ?base = function
    | Record x -> Mixed_record.value ?base x
    | Constructor (v, c) -> Mixed_variant.value ?base c ~index:v.index

  let type_ = function
    | Record x -> Mixed_record.type_ x
    | Constructor (x, _) -> Mixed_variant.type_ x

  let construction = function
    | Record x -> Mixed_record.record_value x
    | Constructor (_, x) -> Mixed_variant.constructor_value x

  let is_all_floats = function
    | Record x -> Mixed_record.is_all_floats x
    | Constructor _ -> false

  let index = function
    | Record x -> x.index
    | Constructor (x, _) -> x.index

  let check_field_integrity = function
    | Record x -> Mixed_record.check_field_integrity x
    | Constructor (v, c) ->
        [ Mixed_variant.check_field_integrity c ~index:v.index
            ~catchall:(List.length v.constructors > 1)
        ]

  let tag = function
    | Record _ -> 0
    | Constructor (_, c) -> c.index
end

module Type = struct
  type t =
    | Record of Mixed_record.t
    | Variant of Mixed_variant.t

  let type_decl = function
    | Record x -> Mixed_record.type_decl x
    | Variant x -> Mixed_variant.type_decl x

  let index = function
    | Record x -> x.index
    | Variant x -> x.index

  let values : t -> Value.t list = function
    | Record x -> [ Record x ]
    | Variant x ->
        List.map x.constructors ~f:(fun cstr -> Value.Constructor (x, cstr))

  let record_of_block i block =
    Record (Mixed_record.of_block i block)

  let variant_of_block i block =
    Variant (Mixed_variant.of_variant i block)
end

let main n ~bytecode =
  (* Don't overrepresent all-float mixed blocks. *)
  let n_all_floats_mixed = n / 4 in
  let records =
    Seq.append
      (Seq.take n_all_floats_mixed
        (enumeration_of_all_floats_mixed_blocks ~bytecode))
      (Seq.take (n - n_all_floats_mixed)
        (enumeration_of_mixed_blocks_except_all_floats_mixed ~bytecode))
    |> List.of_seq
    |> List.mapi ~f:Type.record_of_block
  in
  let variants =
    Seq.take n (enumeration_of_mixed_variants ~bytecode)
    |> List.of_seq
    |> List.mapi ~f:(fun i x -> Type.variant_of_block (i+n) x)
  in
  let types = records @ variants in
  let values = List.concat_map types ~f:Type.values in
  let line ?(indent = 0) fmt =
    Printf.ksprintf
      (fun s ->
         let indent = Seq.init indent (fun _ -> ' ') |> String.of_seq in
         print_endline (indent ^ s))
      fmt
  in
  let _print_in_test ?indent s =
    line ?indent {|let () = print_endline "%s";;|} (String.escaped s)
  in
  let seq_print_in_test ?indent s =
    line ?indent {|print_endline "%s";|} (String.escaped s)
  in
  let do_gc ?indent () =
    seq_print_in_test ?indent " - Doing GC";
    line ?indent "Gc.full_major ();"
  in
  let per_type f = List.iter types ~f in
  let per_value f = List.iter values ~f in
  (* Iterate over the list of values zipped with itself (staggered by 1) *)
  let per_value_staircase f =
    List.iter2 values (List.tl values @ [ List.hd values]) ~f
  in
  line {|(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;|};
  if bytecode then (
    line {| bytecode;|};
  ) else (
    line {| modules = "stubs.c";|};
    line {| ocamlopt_flags = "-extension simd_beta -cc '${cc} -mavx' -ccopt '${cflags}'";|};
    line {| flambda2;|};
    line {| arch_amd64;|};
    line {| native;|};
  );
  line {|*)|};
  line "(** This is code generated by [generate_mixed_blocks_code.ml]. *)";
  line "";
  line "(* Helper functions for manipulating the fields of a mixed record *)";
  line {|let create_string () = String.make (Random.int 100) 'a'|};
  line {|let create_int () = Random.int 0x3FFF_FFFF|};
  line {|let create_float () = Random.float Float.max_float|};
  line {|let create_float32 () = Stdlib_stable.Float32.of_float (Random.float Float.max_float)|};
  line {|let create_float_u () = Stdlib_upstream_compatible.Float_u.of_float (create_float ())|};
  line {|let create_float32_u () = Stdlib_stable.Float32_u.of_float32 (create_float32 ())|};
  line {|let create_int32_u () = Stdlib_upstream_compatible.Int32_u.of_int32 (Random.int32 0x7FFF_FFFFl)|};
  line {|let create_int64_u () = Stdlib_upstream_compatible.Int64_u.of_int64 (Random.int64 0x7FFF_FFFF_FFFF_FFFFL)|};
  line {|let create_nativeint_u () = Stdlib_upstream_compatible.Nativeint_u.of_nativeint (Random.nativeint 0x7FFF_FFFF_FFFF_FFFFn)|};
  if not bytecode then (
    line {|external box_int64x2 : int64x2# -> int64x2 = "%%box_vec128"|};
    line {|external unbox_int64x2 : int64x2 -> int64x2# = "%%unbox_vec128"|};
    line {|external box_int64x4 : int64x4# -> int64x4 = "%%box_vec256"|};
    line {|external unbox_int64x4 : int64x4 -> int64x4# = "%%unbox_vec256"|};
    line {|external interleave_low_64 : int64x2# -> int64x2# -> int64x2# = "" "caml_simd_vec128_interleave_low_64" [@@unboxed] [@@builtin]|};
    line {|external interleave_high_64 : int64x2# -> int64x2# -> int64x2# = "" "caml_simd_vec128_interleave_high_64" [@@unboxed] [@@builtin]|};
    line {|external int64x2_of_int64 : int64 -> int64x2# = "" "caml_int64x2_low_of_int64" [@@unboxed] [@@builtin]|};
    line {|external int64_of_int64x2 : int64x2# -> int64 = "" "caml_int64x2_low_to_int64" [@@unboxed] [@@builtin]|};
    line {|let create_int64x2 () =
      let a = int64x2_of_int64 (Random.int64 0x7FFF_FFFF_FFFF_FFFFL) in
      let b = int64x2_of_int64 (Random.int64 0x7FFF_FFFF_FFFF_FFFFL) in
      interleave_low_64 a b|};
    line {|let int64x2_equal a b =
      let a = unbox_int64x2 a in
      let b = unbox_int64x2 b in
      let al = int64_of_int64x2 a in
      let ah = int64_of_int64x2 (interleave_high_64 a a) in
      let bl = int64_of_int64x2 b in
      let bh = int64_of_int64x2 (interleave_high_64 b b) in
      Int64.equal al bl && Int64.equal ah bh|};
    line {|let int64x2_to_string a =
      let a = unbox_int64x2 a in
      let l = int64_of_int64x2 a in
      let h = int64_of_int64x2 (interleave_high_64 a a) in
      Int64.to_string h ^ ":" ^ Int64.to_string l|};
    line {|
external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4
  = "" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]
external int64x4_first_int64 : int64x4 -> int64 = "" "vec256_first_int64"
  [@@noalloc] [@@unboxed]
external int64x4_second_int64 : int64x4 -> int64 = "" "vec256_second_int64"
  [@@noalloc] [@@unboxed]
external int64x4_third_int64 : int64x4 -> int64 = "" "vec256_third_int64"
  [@@noalloc] [@@unboxed]
external int64x4_fourth_int64 : int64x4 -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]
let create_int64x4 () =
      let a = Random.int64 0x7FFF_FFFF_FFFF_FFFFL in
      let b = Random.int64 0x7FFF_FFFF_FFFF_FFFFL in
      let c = Random.int64 0x7FFF_FFFF_FFFF_FFFFL in
      let d = Random.int64 0x7FFF_FFFF_FFFF_FFFFL in
      int64x4_of_int64s a b c d |> unbox_int64x4
let int64x4_equal a b =
      Int64.equal (int64x4_first_int64 a) (int64x4_first_int64 b) &&
      Int64.equal (int64x4_second_int64 a) (int64x4_second_int64 b) &&
      Int64.equal (int64x4_third_int64 a) (int64x4_third_int64 b) &&
      Int64.equal (int64x4_fourth_int64 a) (int64x4_fourth_int64 b)
let int64x4_to_string a =
      Int64.to_string (int64x4_first_int64 a) ^ ":" ^
      Int64.to_string (int64x4_second_int64 a) ^ ":" ^
      Int64.to_string (int64x4_third_int64 a) ^ ":" ^
      Int64.to_string (int64x4_fourth_int64 a)|};
  );
  line
    {|let check_gen ~equal ~to_string ~message y1 y2 =
  if equal y1 y2 then () else
    failwith
      (Printf.sprintf "%%s: %%s <> %%s" message (to_string y1) (to_string y2))
|};
  line
   {|let check_string = check_gen ~equal:String.equal ~to_string:(fun x -> x)|};
  line {|let check_int = check_gen ~equal:Int.equal ~to_string:Int.to_string|};
  line
   {|let check_float =
  check_gen ~equal:Float.equal ~to_string:Float.to_string|};
  line
   {|let check_float32 =
  check_gen ~equal:Stdlib_stable.Float32.equal ~to_string:Stdlib_stable.Float32.to_string|};
  line
   {|let check_int32 =
  check_gen ~equal:Int32.equal ~to_string:Int32.to_string|};
  line
   {|let check_int64 =
  check_gen ~equal:Int64.equal ~to_string:Int64.to_string|};
  if not bytecode then (
    line
   {|let check_int64x2 =
  check_gen ~equal:int64x2_equal ~to_string:int64x2_to_string|};
  line
   {|let check_int64x4 =
  check_gen ~equal:int64x4_equal ~to_string:int64x4_to_string|};
  );
  line "";
  line "(* Helper functions for testing polymorphic copying. *)";
  line
    {|
let copy_via_weak x =
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some x);
  Weak.get_copy weak 0 |> Option.get|};
  line
    {|
let copy_via_tag x =
  let obj = Obj.repr x in
  Obj.with_tag (Obj.tag obj) obj |> Obj.obj;;|};
  line "";
  line {|(* Helper functions for testing polymorphic operations. *)|};
  line {|let oc = Out_channel.open_bin "/dev/null"|};
  line {|exception Unexpected_success|};
  line {|type forget = T : _ -> forget|};
  line
    {|
let expect_failure f =
  try f (); raise Unexpected_success with
  | Unexpected_success -> assert false
  | _ -> ()

let hash_expect_failure x =
  expect_failure (fun () -> ignore (Hashtbl.hash x : int))

let compare_expect_failure x y =
  expect_failure (fun () -> ignore (compare (T x) (T y) : int));
  expect_failure (fun () -> ignore ((T x) = (T y) : bool))

let compare_expect_success x y =
  ignore (compare (T x) (T y) : int);
  ignore ((T x) = (T y) : bool)

let marshal_expect_failure t =
  expect_failure (fun () -> output_value oc t)|};
  line
    {|
let check_reachable_words expected actual message =
  if expected <> actual
  then failwith (Printf.sprintf "%%s: %%d <> %%d" message expected actual)
;;|};
  line "";
  line "(* Type declarations *)";
  per_type (fun t -> line "%s" (Type.type_decl t));
  print_endline "";
  line {|external opaque_ignore : ('a [@local_opt]) -> unit = "%%opaque"|};
  print_endline "";
  print_endline "let[@opaque] run () =";
  print_endline "(* Let declarations *)";
  seq_print_in_test "Creating values";
  per_value (fun t ->
    line
      "let %s : %s = %s in"
      (Value.value t)
      (Value.type_ t)
      (Value.construction t));
  do_gc ();
  line "";
  line "(* Copies *)";
  seq_print_in_test "Copying values using [with] record update";
  per_value (fun t ->
    match t with
    | Constructor _ ->
        line
          "let %s = %s in"
          (Value.value t ~base:"t_orig")
          (Value.value t)
    | Record record ->
      let field = List.hd record.fields in
      line
        "let %s = { %s%s = %s.%s } in"
        (Value.value t ~base:"t_orig")
        (match record.fields with
         | [_] -> ""
         | _ -> sprintf "%s with " (Value.value t))
        field.name
        (Value.value t)
        field.name);
  print_endline "";
  print_endline "(* Checks *)";
  let () =
    let indent = 2 in
    let line ?(indent = indent) = line ~indent in
    let seq_print_in_test ?(indent = indent) = seq_print_in_test ~indent in
    line
      "let run_checks %s ="
      (List.map values ~f:(fun t ->
         sprintf "(%s : %s)" (Value.value t) (Value.type_ t))
       |> String.concat ~sep:" ");
    seq_print_in_test "    - Marshaling";
    per_value (fun t -> line "marshal_expect_failure %s;" (Value.value t));
    seq_print_in_test "    - Hashing";
    per_value (fun t -> line "hash_expect_failure %s;" (Value.value t));
    if n > 1
    then (
      seq_print_in_test "    - Comparing";
      per_value_staircase (fun t1 t2 ->
        let fn_name =
          (* Polymorphic compare only raises an exception if
             the tags don't match.
          *)
          if Value.tag t1 = Value.tag t2
          then "compare_expect_failure"
          else "compare_expect_success"
       in
        line "%s %s %s;" fn_name
          (Value.value t1)
          (Value.value t2)));
    seq_print_in_test "    - Checking field values";
    per_value (fun t ->
        List.iter (Value.check_field_integrity t) ~f:(line "%s"));
    seq_print_in_test "    - Checking [Obj.reachable_words]";
    per_value (fun t ->
      match t with
      | Constructor _ -> ()
      | Record t ->
        let is_all_floats = Mixed_record.is_all_floats t in
        line
        {|check_reachable_words (Obj.reachable_words (Obj.repr %s)) (%d%s) "Reachable words %d";|}
          (Mixed_record.value t)
          (List.length t.fields + 1)
          (List.map t.fields ~f:(fun (field : Mixed_record.field) ->
            match field.type_ with
            | Imm -> ""
            | Vec128 -> assert (not bytecode); " + 1"
            | Vec256 -> assert (not bytecode); " + 3"
            | Float64 ->
                (* In bytecode, these fields aren't boxed and thus contribute
                    two words to the reachable words (the header and the
                    single-field payload).
                *)
                if not bytecode then "" else " + 2"
            | Float32 | Bits64 | Bits32 | Word ->
              (* Same as float64, except these are custom blocks in bytecode,
                 which involve still another field. *)
                if not bytecode then "" else " + 3"
            | Float ->
                (* The bytecode condition is the same as commented for [Float64].
                    Additionally, if the record is not all floats, then this field
                    is stored boxed.
                *)
                if is_all_floats && not bytecode then "" else " + 2"
            | Str ->
                sprintf " + Obj.reachable_words (Obj.repr t%d.%s)"
                  t.index field.name)
          |> String.concat ~sep:"")
          t.index);
    line "()"
  in
  print_endline "in";
  let run_checks ?indent () =
    seq_print_in_test " - Running checks";
    line
      ?indent
      "let () = run_checks %s in"
      (List.map values ~f:Value.value |> String.concat ~sep:" ")
  in
  run_checks ();
  do_gc ();
  run_checks ();
  seq_print_in_test "Copying values via [Stdlib.Weak]";
  per_value (fun t ->
    line
      "let %s : %s = copy_via_weak %s in"
      (Value.value t)
      (Value.type_ t)
      (Value.value t));
  run_checks ();
  do_gc ();
  run_checks ();
  seq_print_in_test "Copying values via [Obj.with_tag]";
  per_value (fun t ->
    line
      "let %s : %s = copy_via_tag %s in"
      (Value.value t)
      (Value.type_ t)
      (Value.value t));
  run_checks ();
  do_gc ();
  run_checks ();
  line "";
  line "(* Testing local allocation *)";
  print_endline "let go () =";
  let () =
    let indent = 2 in
    let line ?(indent = indent) = line ~indent in
    per_value (fun t ->
      line
        "let local_ %s : %s = %s in"
        (Value.value t)
        (Value.type_ t)
        (Value.construction t));
    do_gc () ~indent;
    per_value (fun t -> line "opaque_ignore %s;" (Value.value t));
    line "()"
  in
  print_endline "in";
  seq_print_in_test "Testing local allocations";
  print_endline "go ();;";
  print_endline "let () = run ();;"
;;

let () =
  let n, bytecode =
    match Sys.argv with
    | [| _; n; "native" |] -> n, false
    | [| _; n; "byte" |] -> n, true
    | _ -> failwith (Printf.sprintf "Usage: %s N <byte|native>" Sys.argv.(0))
  in
  main (int_of_string n) ~bytecode
