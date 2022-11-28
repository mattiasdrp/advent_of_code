open Mdrp_lib

let split (low, high) = function
  | 'F' | 'L' -> (low, (low + high) / 2)
  | 'B' | 'R' -> ((low + high) / 2, high)
  | _ -> assert false

let parse s =
  String.foldi
    (fun i (row, col) c ->
      if i < 7 then (split row c, col) else (row, split col c))
    ((0, 127), (0, 7))
    s

let part_1 file =
  let ci = open_in file in
  let rec aux_parse acc =
    match input_line ci with
    | s ->
        let (_, row), (_, col) = parse s in
        aux_parse (max acc ((row * 8) + col))
    | exception End_of_file -> acc
  in
  aux_parse 0 |> Format.printf "%d@."

exception Found of int

let part_2 file =
  let ci = open_in file in
  let rec aux_parse acc =
    match input_line ci with
    | s ->
        let (_, row), (_, col) = parse s in
        aux_parse (Int.Set.add ((row * 8) + col) acc)
    | exception End_of_file -> acc
  in
  let set = aux_parse Int.Set.empty in
  try
    ignore
      (Int.Set.fold
         (fun e prev -> if e = prev + 1 then e else raise (Found (e - 1)))
         set
         (Int.Set.min_elt set - 1))
  with Found i -> Format.printf "%d@." i

let () =
  let part = try Sys.argv.(1) with _ -> "2" in
  let file = try Sys.argv.(2) with _ -> "input" in
  match part with "1" -> part_1 file | "2" -> part_2 file | _ -> ()
