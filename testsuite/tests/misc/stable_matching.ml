(* TEST
flags = "-I ${ocamlsrcdir}/utils";
expect; *)


let left_size = 10
let right_size = 11

let trace ~on =
  if on then Format.printf else Format.ifprintf Format.std_formatter

module Full = struct
  let random ~debug hdist left_len r =
    let trace x = trace ~on:debug x in
    let a = Array.init left_len Fun.id in
    trace "%2d|" r;
    let () = Array.shuffle ~rand:Random.int a in
    let a = Array.mapi (fun d l ->
        (* avoid ties on the left side *)
        let rank = d * right_size + r in
        trace " %d " l;
        Hashtbl.replace hdist (l,r) rank;
        (l,rank))
        a
    in
    trace "@,";
    a
end

module Partial = struct
  let random ~debug hdist left_len r =
    let trace x = trace ~on:debug x in
    let len = Random.int left_len in
    let a = Array.init len Fun.id in
    trace "%2d|" r;
    let () = Array.shuffle ~rand:Random.int a in
    let a = Array.mapi (fun d l ->
        trace " %d" l;
        let rank = d * right_size + r in
        Hashtbl.replace hdist (l,r) rank;
        (l,rank))
        a
    in
    trace "@,";
    a
end

module Partial_and_ties = struct
  let random ~debug hdist left_len r =
    let trace x = trace ~on:debug x in
    let len = Random.int left_len in
    let a = Array.init len Fun.id in
    trace "%2d|" r;
    let () = Array.shuffle ~rand:Random.int a in
    let new_tie_size () = Random.int 2 in
    let tie_counter = ref (new_tie_size (), 0) in
    let a = Array.mapi (fun _ l ->
        let tie_size, rank = !tie_counter in
        let rank =
          if tie_size < 0 then begin
            tie_counter := new_tie_size (), rank + 1;
            trace " | %d" l;
            rank + 1
          end
          else begin
            trace "   %d" l;
            tie_counter := tie_size - 1 , rank;
            rank
          end
        in
        Hashtbl.replace hdist (l,r) rank;
        (l,rank))
        a
    in
    trace "@,";
    a
end

let random_matrice ~debug line left_len right_len =
  let h = Hashtbl.create (left_len * right_len) in
  Array.init right_len (fun r -> line ~debug h left_len r),
  h


let compare_matching ppf (x,y) =
  let pp_pair ppf = function
    | None -> Format.fprintf ppf       "        "
    | Some (x,y) -> Format.fprintf ppf "%2d ⇐ %2d" x y
  in
  let rec equalize x y = match x, y with
    | [], [] -> []
    | a :: x, b :: y -> (Some a, Some b) :: equalize x y
    | a :: x, [] -> (Some a, None) :: equalize x []
    | [], b :: y -> (None, Some b) :: equalize [] y
  in
  let compare (x,y) (x',y') =
    let c = compare y y' in
    if c = 0 then compare x x' else c
  in
  let x = List.sort compare x.Stable_matching.pairs in
  let y = List.sort compare y.Stable_matching.pairs in
  let l = equalize x y in
  Format.fprintf ppf "@,@[<v>";
  List.iter (fun (l,r) ->
      Format.fprintf ppf "%a | %a@," pp_pair l pp_pair r
      ) l;
  Format.fprintf ppf "@]"

let test ~debug line =
  let trace x = trace ~on:debug x in
  let m, h = random_matrice ~debug line left_size right_size in
  let compatible _ _ = true in
  let preferences r = m.(r) in
  let distance i j = match Hashtbl.find h (i,j) with
    | exception Not_found -> Int.max_int
    | x -> x
  in
  let size = left_size, right_size in
  let gs_matches = Stable_matching.Gale_Shapley.matches ~compatible ~preferences ~size in
  let opt_matches = Stable_matching.matches ~compatible ~preferences ~size in
  let eq x y =
    let sort x =  List.sort Stdlib.compare x in
    let open Stable_matching in
    let r = sort x.pairs = sort y.pairs in
    if not r then trace "%a@," compare_matching (x,y);
    r
  in
  (List.length gs_matches.pairs, Stable_matching.stable_matches ~distance gs_matches),
  (List.length opt_matches.pairs, Stable_matching.stable_matches ~distance opt_matches),
  eq gs_matches opt_matches


let pp_single ppf (size,stable) =
  let pp_pair ppf ((l,r), (l',r')) = Format.fprintf ppf "(%d<=>%d, %d<=>%d)" l r l' r' in
  let other ((l,r), (l',r')) = ((l', r), (l,r')) in
  let pp_rank ppf (l,r) = Format.fprintf ppf "(%d,%d)" l r in
  match stable with
  | Ok () -> Format.fprintf ppf "%d/%d matched" size (min left_size right_size)
  | Error (e:Stable_matching.unstable_matching) ->
      let pair = e.first, e.second in
      Format.fprintf ppf "FAIL %a %a vs %a %a"
        pp_pair pair pp_rank e.current_rank
        pp_pair (other pair) pp_rank e.optimal

let pp_test ppf (gs,opt,same) =
  if same then
    Format.fprintf ppf "SAME %a" pp_single gs
  else
    Format.fprintf ppf "@[%a,@ %a@]"
      pp_single gs
      pp_single opt

let printed_test r line =
  Format.printf "@[<v>@,----------------@,test %d:@," r;
  let r = test ~debug:true line in
  Format.printf "%a@]@." pp_test r


[%%expect {|
val left_size : int = 10
val right_size : int = 11
val trace : on:bool -> ('a, Format.formatter, unit) format -> 'a = <fun>
module Full :
  sig
    val random :
      debug:bool ->
      (int * int, int) Hashtbl.t -> int -> int -> (int * int) array
  end
module Partial :
  sig
    val random :
      debug:bool ->
      (int * int, int) Hashtbl.t -> int -> int -> (int * int) array
  end
module Partial_and_ties :
  sig
    val random :
      debug:bool ->
      (int * int, int) Hashtbl.t -> int -> int -> (int * int) array
  end
val random_matrice :
  debug:'a ->
  (debug:'a -> ('b, 'c) Hashtbl.t -> int -> int -> 'd) ->
  int -> int -> 'd array * ('b, 'c) Hashtbl.t = <fun>
val compare_matching :
  Format.formatter ->
  ('a, int) Stable_matching.matches * ('b, int) Stable_matching.matches ->
  unit = <fun>
val test :
  debug:bool ->
  (debug:bool ->
   (int * int, int) Hashtbl.t ->
   int -> int -> (Stable_matching.left_index * Stable_matching.rank) array) ->
  (int * (unit, Stable_matching.unstable_matching) Result.t) *
  (int * (unit, Stable_matching.unstable_matching) Result.t) * bool = <fun>
val pp_single :
  Format.formatter ->
  int * (unit, Stable_matching.unstable_matching) result -> unit = <fun>
val pp_test :
  Format.formatter ->
  (int * (unit, Stable_matching.unstable_matching) result) *
  (int * (unit, Stable_matching.unstable_matching) result) * bool ->
  unit = <fun>
val printed_test :
  int ->
  (debug:bool ->
   (int * int, int) Hashtbl.t ->
   int -> int -> (Stable_matching.left_index * Stable_matching.rank) array) ->
  unit = <fun>
|}]

let () =
  Random.init 123;
  for n = 0 to 4 do printed_test n Full.random done;
  for n = 0 to 4 do printed_test n Partial.random done
[%%expect {|

----------------
test 0:
 0| 2  1  4  6  8  7  3  9  0  5
 1| 7  6  0  5  9  8  3  4  1  2
 2| 7  2  5  1  4  0  6  3  8  9
 3| 2  0  3  6  4  9  5  7  1  8
 4| 2  9  8  4  1  3  0  5  6  7
 5| 3  7  1  4  8  0  9  6  2  5
 6| 0  8  2  6  7  1  4  5  9  3
 7| 1  5  7  6  2  3  4  0  9  8
 8| 0  4  8  9  1  6  5  3  7  2
 9| 8  4  1  6  9  3  5  2  0  7
10| 0  4  3  2  9  6  1  7  8  5
SAME 10/10 matched

----------------
test 1:
 0| 6  5  8  1  4  0  2  7  3  9
 1| 1  5  4  7  8  0  6  3  9  2
 2| 5  2  3  1  7  0  8  9  4  6
 3| 3  4  2  6  9  0  8  1  5  7
 4| 5  6  1  0  3  8  4  7  9  2
 5| 2  4  8  9  3  5  6  0  1  7
 6| 1  7  9  6  0  8  2  3  5  4
 7| 2  1  6  4  3  5  8  7  9  0
 8| 6  5  3  0  4  8  1  2  9  7
 9| 2  7  8  3  4  1  9  0  5  6
10| 2  1  3  7  0  5  8  9  4  6
SAME 10/10 matched

----------------
test 2:
 0| 4  2  5  6  3  0  1  8  9  7
 1| 8  9  3  4  2  0  5  1  6  7
 2| 7  4  8  1  5  0  9  6  3  2
 3| 1  8  6  7  2  0  4  9  3  5
 4| 1  3  9  0  7  2  4  8  6  5
 5| 0  1  3  9  8  7  4  6  2  5
 6| 8  0  7  3  1  2  9  5  6  4
 7| 3  4  7  5  6  0  1  9  8  2
 8| 8  2  9  0  1  6  3  7  5  4
 9| 8  3  9  5  1  4  7  0  6  2
10| 4  2  9  6  0  8  5  1  7  3
SAME 10/10 matched

----------------
test 3:
 0| 2  8  9  0  1  5  3  7  6  4
 1| 1  9  3  5  7  0  8  4  2  6
 2| 2  1  7  4  8  6  3  9  0  5
 3| 6  1  8  4  0  9  7  2  3  5
 4| 4  5  9  1  6  7  2  0  8  3
 5| 0  4  7  5  9  6  2  8  3  1
 6| 6  1  2  9  7  5  4  3  8  0
 7| 6  4  7  1  3  9  8  0  5  2
 8| 1  7  4  2  9  3  0  8  5  6
 9| 7  4  2  9  1  6  8  5  3  0
10| 8  3  4  6  2  7  1  5  0  9
SAME 10/10 matched

----------------
test 4:
 0| 1  0  9  5  3  4  2  6  7  8
 1| 0  1  3  7  4  6  9  8  2  5
 2| 8  3  4  2  1  9  7  5  0  6
 3| 7  9  0  5  4  1  6  8  3  2
 4| 7  1  5  2  8  3  4  9  0  6
 5| 7  8  5  9  0  2  6  3  1  4
 6| 2  9  6  0  1  3  8  7  5  4
 7| 6  4  0  7  2  3  5  8  9  1
 8| 5  4  7  0  1  8  9  2  3  6
 9| 4  0  8  1  9  3  7  6  2  5
10| 6  2  4  7  5  8  0  9  1  3
SAME 10/10 matched

----------------
test 0:
 0| 0 1
 1|
 2| 1 2 0 3
 3| 3 4 0 6 7 5 2 1
 4| 3 1 4 0 2 5
 5| 1 6 7 8 5 0 2 3 4
 6| 8 6 1 5 0 2 4 3 7
 7|
 8| 0 4 2 3 1
 9| 2 3 4 0 1
10| 5 0 1 6 2 4 3
SAME 8/10 matched

----------------
test 1:
 0| 0
 1| 1 2 3 4 0 5
 2| 0
 3| 3 4 1 2 5 0
 4| 2 1 0
 5|
 6| 0
 7| 1 3 0 4 2
 8| 1 4 6 3 0 7 2 5
 9| 1 0
10| 0 1
SAME 5/10 matched

----------------
test 2:
 0| 4 1 3 2 0
 1| 5 0 3 6 7 8 2 1 4
 2| 6 4 5 2 1 3 0
 3| 0
 4|
 5| 2 4 1 3 5 0
 6| 3 0 2 1
 7| 3 0 2 1
 8| 2 4 3 0 1
 9| 5 4 7 3 6 1 0 2
10| 6 0 1 4 2 3 5
SAME 8/10 matched

----------------
test 3:
 0| 1 0
 1|
 2| 0 1
 3| 4 5 1 3 2 0
 4| 1 2 7 0 3 4 6 5
 5|
 6| 4 7 0 3 2 5 8 6 1
 7| 1 2 0 3
 8| 4 5 8 1 0 2 7 3 6
 9| 0
10| 1 2 3 4 0
SAME 7/10 matched

----------------
test 4:
 0|
 1| 0 5 3 2 4 7 6 1
 2| 1 3 2 0
 3| 1 4 7 6 0 3 2 5
 4| 4 5 2 6 7 0 3 1
 5| 3 5 4 2 1 0
 6| 3 5 0 4 2 1
 7| 4 7 8 1 6 0 3 2 5
 8| 6 3 4 7 0 5 1 2
 9| 1 2 0
10| 1 0 2
SAME 8/10 matched
|}]


let () =
  for n = 0 to 9 do printed_test n Partial_and_ties.random done

[%%expect {|

----------------
test 0:
 0|   0
 1|   5   6 | 3   2   4 | 7   0 | 1
 2|   6   7 | 1   3   4 | 2   0   5
 3|   1   6 | 8   2   5 | 0   4   3 | 7
 4|   3   1 | 2   0
 5|   1   4 | 2   3 | 0
 6|   1 | 2   0
 7|   7 | 4   0   3 | 6   2   5 | 1
 8|   5 | 4   0 | 1   3 | 2
 9|   0   1 | 2
10|   0

 0 ⇐  0 |  5 ⇐  1
 5 ⇐  1 |  6 ⇐  2
 6 ⇐  2 |  1 ⇐  3
 1 ⇐  3 |  3 ⇐  4
 3 ⇐  4 |  4 ⇐  5
 4 ⇐  5 |  2 ⇐  6
 2 ⇐  6 |  7 ⇐  7
 7 ⇐  7 |  0 ⇐  9

8/10 matched, 8/10 matched

----------------
test 1:
 0|   0 | 2   1
 1|   0
 2|   6   4 | 3   0   5 | 2   1   7
 3|   4 | 3   1 | 2   5   0
 4|   0
 5|   0 | 1
 6|   6 | 3   0   4 | 1   5   2
 7|   0
 8|   0   1
 9|   1   2 | 0
10|   1   0 | 2

 0 ⇐  0 |  0 ⇐  1
 6 ⇐  2 |  5 ⇐  2
 4 ⇐  3 |  4 ⇐  3
 3 ⇐  6 |  6 ⇐  6
 1 ⇐  9 |  2 ⇐  9
 2 ⇐ 10 |  1 ⇐ 10

FAIL (2<=>10, 1<=>9) (1,0) vs (1<=>10, 2<=>9) (0,0), 6/10 matched

----------------
test 2:
 0|   3 | 4   2   0 | 1
 1|
 2|   6   5 | 2   4 | 1   0 | 3   7
 3|   0 | 3   6   1 | 4   2 | 5
 4|
 5|   0 | 2   1
 6|   0
 7|   3 | 4   2 | 0   1
 8|   2   0 | 3   4 | 1
 9|   5   0 | 2   4 | 7   8 | 3   1   6
10|

 3 ⇐  0 |  3 ⇐  0
 6 ⇐  2 |  6 ⇐  2
 0 ⇐  3 |  1 ⇐  3
 1 ⇐  5 |  0 ⇐  6
 4 ⇐  7 |  4 ⇐  7
 2 ⇐  8 |  2 ⇐  8
 5 ⇐  9 |  5 ⇐  9

7/10 matched, 7/10 matched

----------------
test 3:
 0|   0
 1|   2 | 1   7 | 0   6   3 | 4   8   5
 2|   1 | 2   0
 3|   0 | 2   1
 4|   0   2 | 1   3   4
 5|
 6|   0   1
 7|   0   3 | 2   1
 8|   0
 9|   2   1 | 0
10|   1   0 | 3   2

 0 ⇐  0 |  7 ⇐  1
 2 ⇐  1 |  0 ⇐  3
 1 ⇐  2 |  4 ⇐  4
 4 ⇐  4 |  1 ⇐  6
 3 ⇐  7 |  3 ⇐  7
         |  2 ⇐  9

5/10 matched, 6/10 matched

----------------
test 4:
 0|
 1|   1   0
 2|   0 | 2   1
 3|   0 | 5   6   1 | 2   4 | 3
 4|   7 | 6   3   5 | 1   4 | 0   8 | 2
 5|   0
 6|   3 | 0   2 | 4   1 | 5
 7|   0 | 2   1
 8|   5 | 4   2   1 | 6   0   3
 9|   3   2 | 4   6   1 | 0   7 | 5
10|   6 | 4   3 | 5   0   1 | 2

 1 ⇐  1 |  1 ⇐  1
 0 ⇐  2 |  4 ⇐  3
 4 ⇐  3 |  7 ⇐  4
 7 ⇐  4 |  0 ⇐  5
 3 ⇐  6 |  3 ⇐  6
 5 ⇐  8 |  5 ⇐  8
 2 ⇐  9 |  2 ⇐  9
 6 ⇐ 10 |  6 ⇐ 10

8/10 matched, 8/10 matched

----------------
test 5:
 0|   2   3 | 1   0
 1|   0
 2|   2 | 0   1
 3|   1 | 0   6   5 | 8   3   7 | 4   2
 4|   2   0 | 1
 5|   1   2 | 0   3
 6|   2 | 0   1
 7|   4 | 1   6 | 3   5 | 0   2 | 7
 8|   2 | 0   1   6 | 4   3 | 7   5
 9|   4 | 5   6   2 | 1   0   3
10|   3   4 | 1   0 | 5   2

 2 ⇐  0 |  0 ⇐  1
 0 ⇐  1 |  7 ⇐  3
 1 ⇐  3 |  2 ⇐  4
 4 ⇐  7 |  1 ⇐  5
 6 ⇐  8 |  4 ⇐  7
 5 ⇐  9 |  6 ⇐  8
 3 ⇐ 10 |  5 ⇐  9
         |  3 ⇐ 10

7/10 matched, 8/10 matched

----------------
test 6:
 0|   2 | 7   5 | 3   4   6 | 1   0
 1|   0   7 | 1   4 | 3   5 | 2   8 | 6
 2|   1   3 | 5   0   6 | 4   2
 3|   7 | 0   3 | 2   1   4 | 5   6
 4|   0   1 | 2
 5|   3   4 | 0   2   5 | 1
 6|   0   1
 7|   2   1 | 3   4 | 0
 8|   6   2 | 5   3 | 1   0   4 | 7   8
 9|   1   3 | 0   2
10|   3   2 | 0   1

 2 ⇐  0 |  5 ⇐  0
 0 ⇐  1 |  8 ⇐  1
 1 ⇐  2 |  7 ⇐  3
 7 ⇐  3 |  0 ⇐  4
 3 ⇐  5 |  4 ⇐  5
 4 ⇐  7 |  2 ⇐  7
 6 ⇐  8 |  6 ⇐  8
         |  1 ⇐  9
         |  3 ⇐ 10

7/10 matched, 9/10 matched

----------------
test 7:
 0|   7 | 6   3 | 5   0 | 1   8 | 2   4
 1|   1   0 | 3   4   2
 2|   4   2 | 3   0 | 1
 3|   2   3 | 5   1 | 0   4
 4|   2   0 | 3   1
 5|   3   1 | 2   0   4 | 5
 6|   2   0 | 1   3 | 4   5
 7|   0   5 | 3   6 | 4   2   1
 8|   7   1 | 4   6 | 3   2   5 | 0   8
 9|   3   1 | 0   2
10|   0 | 4   1   3 | 5   2 | 6

 7 ⇐  0 |  7 ⇐  0
 1 ⇐  1 |  1 ⇐  1
 4 ⇐  2 |  4 ⇐  2
 2 ⇐  3 |  2 ⇐  3
 3 ⇐  5 |  5 ⇐  7
 0 ⇐  7 |  6 ⇐  8
 6 ⇐  8 |  3 ⇐  9
 5 ⇐ 10 |  0 ⇐ 10

FAIL (5<=>10, 0<=>7) (2,0) vs (0<=>10, 5<=>7) (0,0), 8/10 matched

----------------
test 8:
 0|
 1|   0   1
 2|   2 | 5   4 | 7   8 | 0   6 | 1   3
 3|
 4|   1 | 0   2 | 3
 5|
 6|   4   6 | 7   2   1 | 0   5 | 3
 7|   0
 8|   2   5 | 0   4   7 | 1   6   3
 9|   8   2 | 1   4   7 | 6   0   5 | 3
10|   0

 0 ⇐  1 |  1 ⇐  1
 2 ⇐  2 |  2 ⇐  2
 1 ⇐  4 |  3 ⇐  4
 4 ⇐  6 |  6 ⇐  6
 5 ⇐  8 |  5 ⇐  8
 8 ⇐  9 |  8 ⇐  9
         |  0 ⇐ 10

6/10 matched, 7/10 matched

----------------
test 9:
 0|   7   4 | 2   3   6 | 0   1 | 5
 1|   0
 2|
 3|   1 | 0   4 | 2   3
 4|   0
 5|   0 | 1
 6|
 7|
 8|   1 | 2   0   4 | 3
 9|   0 | 2   6 | 3   4 | 1   5 | 7
10|   5 | 2   1 | 0   6   3 | 4

 7 ⇐  0 |  4 ⇐  0
 0 ⇐  1 |  1 ⇐  3
 1 ⇐  3 |  0 ⇐  4
 4 ⇐  8 |  2 ⇐  8
 2 ⇐  9 |  6 ⇐  9
 5 ⇐ 10 |  5 ⇐ 10

6/10 matched, 6/10 matched
|}]
