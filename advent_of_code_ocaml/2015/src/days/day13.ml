open Mdrp_lib
module PMap = Map.Make (Pair.Make (String) (String))

let parse (map, set) line =
  let re =
    Str.regexp
      {|\([a-zA-Z]+\) would \(gain\|lose\) \([0-9]+\) happiness units by sitting next to \([a-zA-Z]+\).|}
  in
  if Str.string_match re line 0 then
    let name1 = Str.matched_group 1 line in
    let happiness = Str.matched_group 3 line |> int_of_string in
    let happiness =
      match Str.matched_group 2 line with
      | "gain" -> happiness
      | "lose" -> -happiness
      | _ -> assert false
    in
    let name2 = Str.matched_group 4 line in
    (PMap.add (name1, name2) happiness map, String.Set.add name1 set)
  else assert false

let happiness map l =
  let rec aux acc = function
    | [ last ] ->
        let happiness1 = PMap.find (List.hd l, last) map in
        let happiness2 = PMap.find (last, List.hd l) map in
        acc + happiness1 + happiness2
    | t1 :: (t2 :: _ as tl) ->
        let happiness1 = PMap.find (t1, t2) map in
        let happiness2 = PMap.find (t2, t1) map in
        aux (acc + happiness1 + happiness2) tl
    | _ -> assert false
  in
  aux 0 l

let compute_max_happiness map names =
  let permutations = Seq.permutations (String.Set.elements names) in
  Seq.fold_left
    (fun max_happiness l ->
      let happiness = happiness map l in
      max happiness max_happiness)
    0 permutations

let part_1 file =
  let map, names = Parse.fold_lines parse (PMap.empty, String.Set.empty) file in
  compute_max_happiness map names

let part_2 file =
  let map, names = Parse.fold_lines parse (PMap.empty, String.Set.empty) file in
  let map =
    String.Set.fold
      (fun name map ->
        PMap.add ("mattias", name) 0 (PMap.add (name, "mattias") 0 map))
      names map
  in
  let names = String.Set.add "mattias" names in
  compute_max_happiness map names

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
