open Mdrp_lib

let flash a =
  let rec flash i j =
    let value = a.(i).(j) in
    if value > 9 then (
      a.(i).(j) <- -1;
      Seq.iter
        (fun (i, j) ->
          let v = a.(i).(j) in
          if v > -1 then a.(i).(j) <- v + 1;
          flash i j)
        (Array.Matrix.moore_neighbourhood a i j))
  in
  Array.iteri
    (fun i a' ->
      Array.iteri
        (fun j v ->
          if v > -1 then (
            a.(i).(j) <- v + 1;
            flash i j))
        a')
    a

(* I can either store in each cell if they've flashed in this step *)
(* already and don't touch them if they did: (int * bool) array array *)
(* Or, since bool are not optimised in OCaml, I can store -1 when a cell flashes *)
(* And browse the matrix just once to set each -1 to 0 *)
(* Another solution is to perform the flash check when v = 10, not v > 9, and let cells *)
(* be incremented above 10 then set to 0 each cell > 10 *)
let step a =
  let rec aux i flashes =
    if i > 100 then flashes
    else (
      flash a;
      (* For each -1, increase flashes and returns the total number of flashes *)
      Array.fold_lefti
        (fun i flashes a' ->
          Array.fold_lefti
            (fun j flashes v ->
              let v, flashes =
                if v = -1 then (0, flashes + 1) else (v, flashes)
              in
              a.(i).(j) <- v;
              flashes)
            flashes a')
        flashes a
      |> aux (i + 1))
  in
  aux 1 0

let part_1 a = step a

let step a =
  let rec aux i =
    flash a;
    (* I don't want to write a for_alli right now so just do it with a fold_lefti *)
    Array.fold_lefti
      (fun i flashes a' ->
        Array.fold_lefti
          (fun j flashes v ->
            let v, flashes = if v = -1 then (0, flashes) else (v, false) in
            a.(i).(j) <- v;
            flashes)
          flashes a')
      true a
    |> function
    | true -> i
    | false -> aux (i + 1)
  in
  aux 1

let part_2 a = step a

let run part file =
  let a =
    Parse.fold_lines
      (fun acc s -> String.to_array Char.to_digit s :: acc)
      [] file
    |> List.rev |> Array.of_list
  in
  match part with 1 -> part_1 a | _ -> part_2 a
