open Mdrp_lib

let read reversed l =
  let rec aux prev cpt acc = function
    | [] -> if reversed then cpt :: prev :: acc else prev :: cpt :: acc
    | hd :: tl when hd = prev -> aux prev (cpt + 1) acc tl
    | hd :: tl ->
        aux hd 1
          (if reversed then cpt :: prev :: acc else prev :: cpt :: acc)
          tl
  in
  aux (List.hd l) 1 [] (List.tl l)

let loop n l =
  let rec aux i reversed l =
    if i = n then List.length l
    else aux (i + 1) (not reversed) (read reversed l)
  in
  aux 0 false l

let parse file =
  let ci = open_in file in
  let line =
    input_line ci
    |> String.fold_left (fun acc c -> Char.to_digit c :: acc) []
    |> List.rev
  in
  close_in ci;
  line

let part_1 file = loop 40 (parse file)
let part_2 file = loop 50 (parse file)
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
