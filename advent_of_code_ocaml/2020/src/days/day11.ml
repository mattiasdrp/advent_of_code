open Mdrp_lib

module Cell = struct
  type t = Empty | Floor | Occupied

  let of_char = function '.' -> Floor | 'L' -> Empty | _ -> assert false

  let pp ppf c =
    Format.fprintf ppf "%s"
      (match c with Empty -> "L" | Occupied -> "#" | Floor -> ".")
end

let ferry file =
  Parse.fold_lines (fun acc s -> String.to_array Cell.of_char s :: acc) [] file
  |> List.rev |> Array.of_list

let less_than_occupied occupied matrix seq =
  let rec aux acc seq =
    if acc >= occupied then false
    else
      match seq () with
      | Seq.Nil -> true
      | Cons ((i, j), seq) ->
          aux (if matrix.(i).(j) = Cell.Occupied then acc + 1 else acc) seq
  in
  aux 0 seq

let step nghbrs_seq occupied matrix =
  let open Cell in
  let matrix' =
    Array.init (Array.length matrix) (fun _ ->
        Array.init (Array.length matrix.(0)) (fun _ -> Cell.Floor))
  in

  let change =
    Array.Matrix.fold
      (fun change i j c ->
        match c with
        | Empty ->
            let nbrs = nghbrs_seq matrix i j in
            if Seq.for_all (fun (i, j) -> matrix.(i).(j) <> Occupied) nbrs then (
              matrix'.(i).(j) <- Occupied;
              true)
            else (
              matrix'.(i).(j) <- Empty;
              change)
        | Occupied ->
            let nbrs = nghbrs_seq matrix i j in
            if less_than_occupied occupied matrix nbrs then (
              matrix'.(i).(j) <- Occupied;
              change)
            else (
              matrix'.(i).(j) <- Empty;
              true)
        | Floor -> change)
      false matrix
  in
  (change, matrix')

let count_occupied matrix =
  Array.Matrix.fold
    (fun acc _ _ -> function Cell.Occupied -> acc + 1 | _ -> acc)
    0 matrix

let loop nghbrs_seq occupied matrix =
  let rec aux matrix =
    let change, matrix = step nghbrs_seq occupied matrix in
    if change then aux matrix else count_occupied matrix
  in
  aux matrix

let part_1 file =
  ferry file |> loop Array.Matrix.moore_neighbourhood 4 |> Format.eprintf "%d@."

let part_2 file =
  ferry file
  |> loop (Array.Matrix.queen_move (fun m i j -> m.(i).(j) = Cell.Floor)) 5
  |> Format.eprintf "%d@."

let run part file =
  match part with 1 -> part_1 file | 2 -> part_2 file | _ -> ()
