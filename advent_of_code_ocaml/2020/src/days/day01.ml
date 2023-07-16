open Mdrp_lib

exception Found of int

let parse file =
  Parse.fold_lines
    (fun acc line -> Int.Set.add (int_of_string line) acc)
    Int.Set.empty file

let part_1 file =
  let set = parse file in
  try
    Int.Set.iter
      (fun i ->
        let res = 2020 - i in
        if Int.Set.mem res set then raise (Found (i * res)))
      set;
    assert false
  with Found i -> i

let part_2 file =
  let set = parse file in
  try
    Int.Set.iter
      (fun i ->
        let res = 2020 - i in
        Int.Set.iter
          (fun j ->
            let res = res - j in
            if Int.Set.mem res set then raise (Found (i * j * res)))
          set)
      set;
    assert false
  with Found i -> i

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
