open Mdrp_lib

let next = function
  | 0 -> [ 1 ]
  | n ->
      let s = string_of_int n in
      if String.length s mod 2 = 0 then
        [
          int_of_string (String.sub s 0 (String.length s / 2));
          int_of_string
            (String.sub s (String.length s / 2) (String.length s / 2));
        ]
      else [ n * 2024 ]

let next_level map =
  Int.Map.fold
    (fun stone quantity map ->
      let stones = next stone in
      List.fold_left
        (fun map stone ->
          Int.Map.update stone
            (function Some q -> Some (q + quantity) | None -> Some quantity)
            map)
        map stones)
    map Int.Map.empty

let blink times stones =
  let rec aux i map =
    if i = times then map
    else
      let map = next_level map in
      aux (i + 1) map
  in
  aux 0
    (List.fold_left
       (fun map stone -> Int.Map.add stone 1 map)
       Int.Map.empty stones)

let common_part count file =
  let list =
    let ci = open_in file in
    let list =
      input_line ci
      |> String.split_on_char_non_empty ' '
      |> List.map int_of_string
    in
    close_in ci;
    list
  in
  let map = blink count list in
  Format.eprintf "@[<v 0>%a@." Int.Map.(pp Int.pp) map;
  Int.Map.fold (fun _ quantity acc -> acc + quantity) map 0

let run part file =
  match part with 1 -> common_part 25 file | _ -> common_part 75 file
