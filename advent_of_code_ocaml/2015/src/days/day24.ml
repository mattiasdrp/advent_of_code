open Mdrp_lib

let best_group list weight =
  let rec aux total sub_group ~min_size ~min_quantum rest list =
    if total = weight then
      let size, quantum =
        (List.length sub_group, List.fold_left ( * ) 1 sub_group)
      in
      if size < min_size then (size, quantum)
      else if size = min_size then
        if quantum < min_quantum then (size, quantum) else (size, min_quantum)
      else (min_size, min_quantum)
    else
      match list with
      | hd :: tl ->
          if (hd * min_size) + total < weight then (min_size, min_quantum)
          else
            let min_size, min_quantum =
              if hd + total <= weight && List.length sub_group < min_size then
                aux (total + hd) (hd :: sub_group) ~min_size ~min_quantum rest
                  tl
              else (min_size, min_quantum)
            in
            aux total sub_group ~min_size ~min_quantum (hd :: rest) tl
      | _ -> (min_size, min_quantum)
  in
  aux 0 [] ~min_size:(List.length list) ~min_quantum:max_int [] list

let parse list line = int_of_string line :: list

let parse file =
  Parse.fold_lines parse [] file
  |> List.fast_sort (fun i1 i2 -> Int.compare i2 i1)

let part_1 file =
  let packages = parse file in
  let total_weight = List.fold_left ( + ) 0 packages / 3 in
  let _min_size, min_quantum = best_group packages total_weight in
  min_quantum

let part_2 file =
  let packages = parse file in
  let total_weight = List.fold_left ( + ) 0 packages / 4 in
  let _min_size, min_quantum = best_group packages total_weight in
  min_quantum

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
