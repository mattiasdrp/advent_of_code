open Mdrp_lib

module PMap = struct
  include Map.Make (Pair.Make (String) (String))

  let add (s1, s2) v t =
    if String.compare s1 s2 < 0 then add (s1, s2) v t else add (s2, s1) v t

  let find (s1, s2) t =
    if String.compare s1 s2 < 0 then find (s1, s2) t else find (s2, s1) t
end

let parse (towns, map) line =
  let re = Str.regexp {|\([a-zA-Z]+\) to \([a-zA-Z]+\) = \([0-9]+\)|} in
  if Str.string_match re line 0 then
    let town1 = Str.matched_group 1 line in
    let town2 = Str.matched_group 2 line in
    let dist = int_of_string @@ Str.matched_group 3 line in
    ( String.Set.add town1 (String.Set.add town2 towns),
      PMap.add (town1, town2) dist map )
  else assert false

let dist map l =
  let rec aux acc = function
    | [ _ ] -> acc
    | t1 :: (t2 :: _ as tl) ->
        let dist = PMap.find (t1, t2) map in
        aux (acc + dist) tl
    | _ -> assert false
  in
  aux 0 l

let part_1 file =
  let towns, map = Parse.fold_lines parse (String.Set.empty, PMap.empty) file in
  let permutations = Seq.permutations (String.Set.elements towns) in
  Seq.fold_left
    (fun min_dist l ->
      let dist = dist map l in
      min dist min_dist)
    max_int permutations

let part_2 file =
  let towns, map = Parse.fold_lines parse (String.Set.empty, PMap.empty) file in
  let permutations = Seq.permutations (String.Set.elements towns) in
  Seq.fold_left
    (fun max_dist l ->
      let dist = dist map l in
      max dist max_dist)
    0 permutations

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
