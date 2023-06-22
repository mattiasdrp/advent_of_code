open Mdrp_lib

let part_1 file =
  let ci = open_in file in
  let rec aux_parse set acc =
    match input_line ci with
    | "" -> aux_parse Char.Set.empty (acc + Char.Set.cardinal set)
    | s -> aux_parse (String.fold (fun set c -> Char.Set.add c set) set s) acc
    | exception End_of_file -> acc + Char.Set.cardinal set
  in
  aux_parse Char.Set.empty 0 |> Format.printf "%d@."

let part_2 file =
  let ci = open_in file in
  let rec aux_parse first set acc =
    match input_line ci with
    | "" -> aux_parse true Char.Set.empty (acc + Char.Set.cardinal set)
    | s ->
        let set =
          if first then Char.Set.of_list (String.to_list s)
          else Char.Set.inter set (Char.Set.of_list (String.to_list s))
        in
        aux_parse false set acc
    | exception End_of_file -> acc + Char.Set.cardinal set
  in
  aux_parse true Char.Set.empty 0 |> Format.printf "%d@."

let run part file =
  match part with 1 -> part_1 file | 2 -> part_2 file | _ -> ()
