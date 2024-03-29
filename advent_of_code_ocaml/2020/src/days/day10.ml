open Mdrp_lib

let parse file =
  let set =
    Parse.fold_lines
      (fun acc line -> Int.Set.add (int_of_string line) acc)
      Int.Set.empty file
  in
  Int.Set.add (Int.Set.max_elt set + 3) set |> Int.Set.add 0 |> Int.Set.elements

let part_1 file =
  let rec aux (ones, threes) = function
    | hd1 :: (hd2 :: _ as tl) ->
        let diff = hd2 - hd1 in
        let ones, threes =
          if diff = 1 then (ones + 1, threes)
          else if diff = 3 then (ones, threes + 1)
          else (ones, threes)
        in
        aux (ones, threes) tl
    | _ ->
        Format.eprintf "%d, %d@." ones threes;
        ones * threes
  in
  aux (0, 0) (parse file)

let part_2 file =
  let tbl = Hashtbl.create 32 in
  let rec aux l =
    match Hashtbl.find tbl l with
    | v -> v
    | exception Not_found ->
        let res =
          match l with
          | [] -> 1
          | hd :: tl -> (
              let res = aux tl in
              res
              +
              match tl with
              | _ :: (hdb :: _ as tl) when hdb - hd <= 3 -> (
                  let res = aux tl in
                  res
                  +
                  match tl with
                  | _ :: (hdb :: _ as tl) when hdb - hd <= 3 -> aux tl
                  | _ -> 0)
              | _ -> 0)
        in
        Hashtbl.add tbl l res;
        res
  in
  aux (parse file)

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
