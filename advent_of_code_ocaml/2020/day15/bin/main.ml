open Mdrp_lib

let fill_init_map l =
  List.fold_lefti (fun acc i n -> Int.Map.add n (i + 1) acc) Int.Map.empty l

let memorize max l last =
  let rec aux turn map last =
    if turn = max then last
    else
      match Int.Map.find last map with
      | v -> aux (turn + 1) (Int.Map.add last turn map) (turn - v)
      | exception Not_found ->
          aux (turn + 1)
            (Int.Map.update last
               (function None -> Some turn | Some t -> Some t)
               map)
            0
  in
  aux (List.length l + 1) (fill_init_map l) last

let parse file =
  let ci = open_in file in
  match input_line ci with
  | s ->
      let l = String.split_on_char ',' s |> List.map int_of_string in
      (l, 0)
  | exception End_of_file -> assert false

let part_1 file =
  let l, last = parse file in
  memorize 2020 l last |> Format.printf "%d@."

let part_2 file =
  let l, last = parse file in
  memorize 30_000_000 l last |> Format.printf "%d@."

let () =
  let part = try Sys.argv.(1) with _ -> "2" in
  let file = try Sys.argv.(2) with _ -> "input" in
  match part with "1" -> part_1 file | "2" -> part_2 file | _ -> ()
