open Mdrp_lib

let parse list line = int_of_string line :: list

let combinations n l =
  let rec aux n = function
    | _ when n = 0 -> [ [] ]
    | [] -> []
    | hd :: tl ->
        let with_hd =
          if n >= hd then List.map (fun comb -> hd :: comb) (aux (n - hd) tl)
          else []
        in
        let without_hd = aux n tl in

        List.rev_append with_hd without_hd
  in
  aux n l

let part_1 file =
  let list = Parse.fold_lines parse [] file |> List.fast_sort Int.compare in
  let combs = combinations 150 list in
  List.length combs

let part_2 file =
  let list = Parse.fold_lines parse [] file |> List.fast_sort Int.compare in
  let combs = combinations 150 list in
  List.fold_left
    (fun (min, nb) comb ->
      let length = List.length comb in
      if length < min then (length, 1)
      else if length = min then (min, nb + 1)
      else (min, nb))
    (max_int, 0) combs
  |> snd

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
