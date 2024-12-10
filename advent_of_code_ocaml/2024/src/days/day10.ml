open Mdrp_lib
module PairSet = Set.Make (Pair.Make (Int) (Int))

module Matrix = struct
  type t = int Array.t Array.t

  let of_list l =
    let padded_l = List.map (fun row -> "." ^ row ^ ".") l in
    let frow = List.hd padded_l in
    let length = String.length frow in
    let padded_row = String.init length (fun _ -> '.') in
    (padded_row :: padded_l) @ [ padded_row ]
    |> List.map (fun string ->
           String.fold_left
             (fun acc -> function
               | '.' -> -1 :: acc
               | c -> Char.to_digit c :: acc)
             [] string
           |> List.rev |> Array.of_list)
    |> Array.of_list

  let pp ppf t =
    Format.(
      pp_print_array ~pp_sep:pp_print_cut
        (pp_print_array ~pp_sep:(fun _ppf () -> ()) pp_print_int)
        ppf)
      t

  let zeros t =
    Array.fold_lefti
      (fun row set string ->
        Array.fold_lefti
          (fun col set -> function 0 -> PairSet.add (row, col) set | _ -> set)
          set string)
      PairSet.empty t
end

module PairMap = Map.Make (Pair.Make (Int) (Int))

let bfs matrix (row, col) =
  let rec aux nrow ncol value set =
    if value = 9 then PairSet.add (nrow, ncol) set
    else
      next (nrow + 1) ncol value set
      |> next (nrow - 1) ncol value
      |> next nrow (ncol + 1) value
      |> next nrow (ncol - 1) value
  and next nrow ncol value set =
    if matrix.(nrow).(ncol) = value + 1 then aux nrow ncol (value + 1) set
    else set
  in
  aux row col 0 PairSet.empty

let part_1 file =
  let matrix = Parse.lines file |> Matrix.of_list in
  let zeros = Matrix.zeros matrix in
  let map =
    PairSet.fold
      (fun pos map -> PairMap.add pos (bfs matrix pos) map)
      zeros PairMap.empty
  in
  PairMap.fold (fun _ set acc -> acc + PairSet.cardinal set) map 0

let bfs_part2 matrix (row, col) map =
  let rec aux nrow ncol value map =
    if value = 9 then
      PairMap.update (row, col)
        (function Some v -> Some (v + 1) | None -> Some 1)
        map
    else
      next (nrow + 1) ncol value map
      |> next (nrow - 1) ncol value
      |> next nrow (ncol + 1) value
      |> next nrow (ncol - 1) value
  and next nrow ncol value map =
    if matrix.(nrow).(ncol) = value + 1 then aux nrow ncol (value + 1) map
    else map
  in
  aux row col 0 map

let part_2 file =
  let matrix = Parse.lines file |> Matrix.of_list in
  let zeros = Matrix.zeros matrix in
  let map =
    PairSet.fold (fun pos map -> bfs_part2 matrix pos map) zeros PairMap.empty
  in
  PairMap.fold (fun _ value acc -> acc + value) map 0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
