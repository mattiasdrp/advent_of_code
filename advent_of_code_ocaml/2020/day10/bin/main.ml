open Mdrp_lib

let parse file =
  let ci = open_in file in
  let rec aux_parse acc =
    match input_line ci with
    | s -> aux_parse (Int.Set.add (int_of_string s) acc)
    | exception End_of_file ->
        Int.Set.add (Int.Set.max_elt acc + 3) acc
        |> Int.Set.add 0 |> Int.Set.elements
  in
  aux_parse Int.Set.empty

let part_1 file =
  let rec aux (ones, threes) = function
    | hd1 :: (hd2 :: _ as tl) ->
        let diff = hd2 - hd1 in
        let ones, threes =
          if diff = 1 then (ones + 1, threes)
          else if diff = 3 then (ones, threes + 1)
          else (ones, threes)
        in
        aux (ones, threes) tl
    | _ ->
        Format.eprintf "%d, %d@." ones threes;
        ones * threes
  in
  aux (0, 0) (parse file) |> Format.printf "%d@."

let part_2 file =
  let tbl = Hashtbl.create 32 in
  let rec aux l =
    match Hashtbl.find tbl l with
    | v -> v
    | exception Not_found ->
        let res =
          match l with
          | [] -> 1
          | hd :: tl -> (
              let res = aux tl in
              res
              +
              match tl with
              | _ :: (hdb :: _ as tl) when hdb - hd <= 3 -> (
                  let res = aux tl in
                  res
                  +
                  match tl with
                  | _ :: (hdb :: _ as tl) when hdb - hd <= 3 -> aux tl
                  | _ -> 0)
              | _ -> 0)
        in
        Hashtbl.add tbl l res;
        res
  in
  aux (parse file) |> Format.printf "%d@."

let () =
  let part = try Sys.argv.(1) with _ -> "2" in
  let file = try Sys.argv.(2) with _ -> "input" in
  match part with "1" -> part_1 file | "2" -> part_2 file | _ -> ()
