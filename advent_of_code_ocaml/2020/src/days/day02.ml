open Mdrp_lib

exception Found of int

let part_1 file =
  let ci = open_in file in
  let rec aux_parse cpt =
    match input_line ci with
    | s -> (
        match String.split_on_char ' ' s with
        | [ itvl; char; password ] ->
            let min, max =
              match String.split_on_char '-' itvl with
              | [ min; max ] -> (int_of_string min, int_of_string max)
              | _ -> assert false
            in
            let char = char.[0] in
            let res =
              String.fold
                (fun acc c -> if c = char then acc + 1 else acc)
                0 password
            in
            let cpt = if res < min || res > max then cpt else cpt + 1 in
            aux_parse cpt
        | _ -> assert false)
    | exception End_of_file -> cpt
  in
  aux_parse 0

let part_2 file =
  let ci = open_in file in
  let rec aux_parse cpt =
    match input_line ci with
    | s -> (
        match String.split_on_char ' ' s with
        | [ itvl; char; password ] ->
            let pos1, pos2 =
              match String.split_on_char '-' itvl with
              | [ pos1; pos2 ] ->
                  (int_of_string pos1 - 1, int_of_string pos2 - 1)
              | _ -> assert false
            in
            let char = char.[0] in
            let cpt =
              match (password.[pos1] = char, password.[pos2] = char) with
              | true, false | false, true -> cpt + 1
              | _ -> cpt
            in
            aux_parse cpt
        | _ -> assert false)
    | exception End_of_file -> cpt
  in
  aux_parse 0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
