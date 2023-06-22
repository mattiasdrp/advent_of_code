open Mdrp_lib

exception Found of int

let part_1 file =
  let ci = open_in file in
  let rec aux_parse acc =
    match input_line ci with
    | s -> aux_parse (Int.Set.add (int_of_string s) acc)
    | exception End_of_file -> acc
  in
  let set = aux_parse Int.Set.empty in
  try
    Int.Set.iter
      (fun i ->
        let res = 2020 - i in
        if Int.Set.mem res set then raise (Found (i * res)))
      set
  with Found i -> Format.printf "%d@." i

let part_2 file =
  let ci = open_in file in
  let rec aux_parse acc =
    match input_line ci with
    | s -> aux_parse (Int.Set.add (int_of_string s) acc)
    | exception End_of_file -> acc
  in
  let set = aux_parse Int.Set.empty in
  try
    Int.Set.iter
      (fun i ->
        let res = 2020 - i in
        Int.Set.iter
          (fun j ->
            let res = res - j in
            if Int.Set.mem res set then raise (Found (i * j * res)))
          set)
      set
  with Found i -> Format.printf "%d@." i

let run part file =
  match part with 1 -> part_1 file | 2 -> part_2 file | _ -> ()
