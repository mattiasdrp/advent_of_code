open Mdrp_lib
module Corrupted = Set.Make (Pair.Make (Int) (Int))

exception Max_steps of Corrupted.t

let size =
  match Sys.argv.(4) with value -> int_of_string value | exception _ -> 70

module Coord = struct
  type t = { row : int; col : int }

  let compare = compare
  let pp ppf { row; col } = Format.fprintf ppf "(%d, %d)" row col
end

module Location = struct
  type t = { coord : Coord.t }

  let compare t1 t2 = Coord.compare t1.coord t2.coord
  let pp ppf t = Coord.pp ppf t.coord

  let move t (drow, dcol) =
    let row = t.coord.row + drow in
    let col = t.coord.col + dcol in
    { coord = { row; col } }
end

module Queue = struct
  include Set.Make (struct
    type t = Location.t * int * Coord.t list

    let compare (loc1, score1, _) (loc2, score2, _) =
      let c1 = compare score1 score2 in
      if c1 = 0 then Location.compare loc1 loc2 else c1

    let pp _ _ = ()
  end)

  let pop queue =
    let elt = min_elt queue in
    (elt, remove elt queue)

  let add_if_empty location score path grid t =
    let Coord.{ row; col } = location.Location.coord in
    if
      Corrupted.mem (row, col) grid
      || row < 0 || row > size || col < 0 || col > size
    then t
    else add (location, score, location.coord :: path) t
end

module Visited = Map.Make (Coord)

let is_end Location.{ coord; _ } = coord.row = size && coord.col = size

let traverse grid start =
  let rec aux queue visited =
    if Queue.is_empty queue then raise Exit
    else
      let (loc, score, path), queue = Queue.pop queue in
      if is_end loc then path
      else
        let prev_score =
          Visited.find_opt loc.coord visited |> Option.value ~default:max_int
        in
        let visited, queue =
          if prev_score > score then
            let visited = Visited.add loc.coord score visited in
            let score = score + 1 in
            let queue =
              Queue.add_if_empty
                (Location.move loc (1, 0))
                score path grid queue
              |> Queue.add_if_empty (Location.move loc (0, 1)) score path grid
              |> Queue.add_if_empty (Location.move loc (-1, 0)) score path grid
              |> Queue.add_if_empty (Location.move loc (0, -1)) score path grid
            in
            (visited, queue)
          else (visited, queue)
        in
        aux queue visited
  in
  aux (Queue.singleton (start, 0, [ start.coord ])) Visited.empty

let pp path ppf corrupted =
  Format.fprintf ppf "@[<v 0>";
  for row = 0 to size do
    for col = 0 to size do
      Format.fprintf ppf "%s"
        (if Corrupted.mem (row, col) corrupted then "#"
         else if Corrupted.mem (row, col) path then "0"
         else ".")
    done;
    Format.fprintf ppf "@,"
  done

let corrupted lines =
  Array.fold_left
    (fun set line ->
      match String.split_on_char ',' line with
      | [ col; row ] -> Corrupted.add (int_of_string row, int_of_string col) set
      | _ -> assert false)
    Corrupted.empty lines

let part_1 file =
  let max_bytes =
    match Sys.argv.(3) with value -> int_of_string value | exception _ -> 1024
  in
  let lines = Parse.lines file |> Array.of_list in
  let corrupted = corrupted (Array.sub lines 0 max_bytes) in
  let path = traverse corrupted { coord = { row = 0; col = 0 } } in
  List.length path - 1

let part_2 file =
  let start = Location.{ coord = { row = 0; col = 0 } } in
  let lines = Parse.lines file |> Array.of_list in
  let max_length = Array.length lines in
  let rec dicho min_bytes max_bytes =
    if min_bytes > max_bytes then lines.(max_bytes)
    else
      let middle = (min_bytes + max_bytes) / 2 in
      let corrupted = corrupted (Array.sub lines 0 middle) in
      match traverse corrupted start with
      | _ -> dicho (middle + 1) max_bytes
      | exception Exit -> dicho min_bytes (middle - 1)
  in
  let line = dicho 0 max_length in
  Format.eprintf "%s@." line;
  0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
