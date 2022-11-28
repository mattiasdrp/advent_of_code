open Mdrp_lib
module PS = Int.PairSet

let fold_y fy (x, y) = if y < fy then (x, y) else (x, fy + fy - y)
let fold_x fx (x, y) = if x < fx then (x, y) else (fx + fx - x, y)

let fold = function
  | "x", v -> fold_x v
  | "y", v -> fold_y v
  | _ -> assert false

let part_1 dots folds =
  let fold = fold (List.hd folds) in
  PS.map fold dots |> PS.cardinal |> Format.printf "%d@."

let part_2 dots folds =
  let dots =
    List.fold_left
      (fun dots inst ->
        let fold = fold inst in
        PS.map fold dots)
      dots folds
  in
  (* I could get maxx with PS.max_elt but since I'm iterating on it, better do it once *)
  let maxx, maxy =
    PS.fold (fun (x, y) (maxx, maxy) -> (max x maxx, max y maxy)) dots (0, 0)
  in
  for y = 0 to maxy do
    for x = 0 to maxx do
      Format.eprintf "%c" (if PS.mem (x, y) dots then '#' else ' ')
    done;
    Format.eprintf "@."
  done

let () =
  let part = try Sys.argv.(1) with _ -> "2" in
  let file = try Sys.argv.(2) with _ -> "input" in
  let dots, folds =
    Parse.fold_lines
      (fun ((dots, folds) as acc) s ->
        match String.split_on_char ',' s with
        | [ x; y ] -> (PS.add (int_of_string x, int_of_string y) dots, folds)
        | _ -> (
            match String.split_on_char '=' s with
            | [ text; value ] -> (
                match String.split_on_char ' ' text with
                | [ _; _; axis ] -> (dots, (axis, int_of_string value) :: folds)
                | _ -> acc)
            | _ -> acc))
      (PS.empty, []) file
  in
  let folds = List.rev folds in
  match part with
  | "1" -> part_1 dots folds
  | "2" -> part_2 dots folds
  | _ -> ()
