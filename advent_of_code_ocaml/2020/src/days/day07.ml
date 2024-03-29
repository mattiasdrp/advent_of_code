open Mdrp_lib

let bag_contains map bags s =
  let rec aux bags =
    String.Map.exists
      (fun bag _cpt -> String.equal bag s || aux (String.Map.find bag map))
      bags
  in
  aux bags

let nb_containers s map =
  String.Map.fold
    (fun _bag bags cpt -> if bag_contains map bags s then cpt + 1 else cpt)
    map 0

let parse file =
  (* light red bags contain 1 bright white bag, 2 muted yellow bags. *)
  let re = Str.regexp {|\([a-z ]+\) bags contain \([0-9a-z, ]+\).|} in
  (* 1 bright white bag *)
  let re_bag = Str.regexp {| *\([0-9]+\) \([a-z ]+\) bags?|} in
  Parse.fold_lines
    (fun acc_map line ->
      if Str.string_match re line 0 then
        let container = Str.matched_group 1 line in
        let contains = Str.matched_group 2 line in
        let map =
          List.fold_left
            (fun map bag ->
              if Str.string_match re_bag bag 0 then
                String.Map.add (Str.matched_group 2 bag)
                  (int_of_string (Str.matched_group 1 bag))
                  map
              else map)
            String.Map.empty
            (String.split_on_char ',' contains)
        in
        String.Map.add container map acc_map
      else failwith "Wrong regexp, should not happen")
    String.Map.empty file

let part_1 file = parse file |> nb_containers "shiny gold"

let count_bags s map =
  let rec aux bags acc =
    String.Map.fold
      (fun bag cpt acc ->
        let res = aux (String.Map.find bag map) 0 in
        acc + (cpt * (res + 1)))
      bags acc
  in
  aux (String.Map.find s map) 0

let part_2 file = parse file |> count_bags "shiny gold"
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
