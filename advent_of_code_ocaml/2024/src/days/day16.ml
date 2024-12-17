open Mdrp_lib

module Cell = struct
  type t = Empty | Wall | Reindeer | End

  let of_char = function
    | '.' -> Empty
    | '#' -> Wall
    | 'S' -> Reindeer
    | 'E' -> End
    | _ -> assert false

  let pp ppf t =
    Format.fprintf ppf "%c"
      (match t with Empty -> '.' | Wall -> '#' | Reindeer -> 'S' | End -> 'E')
end

module Dir = struct
  type t = North | East | South | West

  let get_deltas = function
    | North -> (-1, 0)
    | East -> (0, 1)
    | South -> (1, 0)
    | West -> (0, -1)

  let rotate_counterclockwise dir =
    match dir with
    | North -> West
    | East -> North
    | South -> East
    | West -> South

  let rotate_clockwise dir =
    match dir with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

  let compare = Stdlib.compare

  let pp ppf t =
    Format.fprintf ppf "%s"
      (match t with
      | North -> "North"
      | West -> "West"
      | East -> "East"
      | South -> "South")
end

module Coord = struct
  type t = { row : int; col : int }

  let compare = compare
  let pp ppf { row; col } = Format.fprintf ppf "(%d, %d)" row col
end

module Location = struct
  open Coord

  type t = { coord : Coord.t; dir : Dir.t; coords : Coord.t list }

  let compare t1 t2 =
    let c1 = Coord.compare t1.coord t2.coord in
    if c1 = 0 then Dir.compare t1.dir t2.dir else c1

  let move { coord = { row; col }; dir; coords } =
    let drow, dcol = Dir.get_deltas dir in
    let coord = { row = row + drow; col = col + dcol } in
    { coord; dir; coords = coord :: coords }

  let move_clockwise t = move { t with dir = Dir.rotate_clockwise t.dir }

  let move_counterclockwise t =
    move { t with dir = Dir.rotate_counterclockwise t.dir }

  let pp ppf { coord = { row; col }; dir; coords } =
    Format.fprintf ppf "(%d, %d, %a) %a" row col Dir.pp dir
      List.(pp Coord.pp)
      coords
end

module Matrix = struct
  include Array.Matrix

  type t = Cell.t Array.t Array.t

  let pp ppf t =
    Format.(
      pp_print_array ~pp_sep:pp_print_cut
        (pp_print_array ~pp_sep:(fun _ppf () -> ()) Cell.pp)
        ppf)
      t

  let parse_line acc line = String.to_array Cell.of_char line :: acc

  let parse file : t =
    Mdrp_lib.Parse.fold_lines parse_line [] file |> List.rev |> Array.of_list

  let find_start t =
    let row, col =
      fold_lefti
        (fun pos_reindeer ~row ~col cell ->
          match cell with Cell.Reindeer -> (row, col) | _ -> pos_reindeer)
        (0, 0) t
    in
    let coord = Coord.{ row; col } in
    Location.{ coord; dir = East; coords = [ coord ] }
end

module Visited = Map.Make (Location)

module Queue = struct
  include Set.Make (struct
    type t = Location.t * int

    let compare (location1, score1) (location2, score2) =
      let c1 = compare score1 score2 in
      if c1 = 0 then Location.compare location1 location2 else c1

    let pp ppf (loc, score) =
      Format.fprintf ppf "(%a: %d)" Location.pp loc score
  end)

  let add ((loc, score) as elt) t =
    match find elt t with
    | loc', _ ->
        add
          ({ loc with coords = List.rev_append loc.coords loc'.coords }, score)
          (remove elt t)
    | exception _ -> add elt t

  let pop queue =
    let elt = min_elt queue in
    (elt, remove elt queue)
end

let is_end grid location =
  grid.(location.Location.coord.row).(location.coord.col) = Cell.End

let add_if_empty location score grid queue =
  if grid.(location.Location.coord.row).(location.coord.col) != Cell.Wall then
    Queue.add (location, score) queue
  else queue

module Spots = Set.Make (Coord)

let traverse grid start =
  let rec aux queue visited all_paths registered_score =
    if Queue.is_empty queue then (registered_score, all_paths)
    else
      let (location, score), queue = Queue.pop queue in
      if score > registered_score then (registered_score, all_paths)
      else if is_end grid location then
        let registered_score = score in
        let all_paths =
          List.fold_left
            (fun spots coord -> Spots.add coord spots)
            all_paths location.coords
        in
        aux queue visited all_paths registered_score
      else
        let prev_score =
          Visited.find_opt location visited |> Option.value ~default:max_int
        in
        let visited, queue =
          if prev_score >= score then
            let visited = Visited.add location score visited in
            let queue =
              add_if_empty (Location.move location) (score + 1) grid queue
              |> add_if_empty
                   (Location.move_clockwise location)
                   (score + 1001) grid
              |> add_if_empty
                   (Location.move_counterclockwise location)
                   (score + 1001) grid
            in
            (visited, queue)
          else (visited, queue)
        in
        aux queue visited all_paths registered_score
  in
  aux (Queue.singleton (start, 0)) Visited.empty Spots.empty max_int

let part_1 file =
  let grid = Matrix.parse file in
  let start = Matrix.find_start grid in
  let min_score, _ = traverse grid start in
  min_score

let pp spots ppf t =
  Array.iteri
    (fun row line ->
      Array.iteri
        (fun col cell ->
          Format.fprintf ppf "%s"
            (if Spots.mem { row; col } spots then "0"
             else Format.asprintf "%a" Cell.pp cell))
        line;
      Format.fprintf ppf "@,")
    t

let part_2 file =
  let grid = Matrix.parse file in
  let start = Matrix.find_start grid in
  let _, spots = traverse grid start in
  Spots.cardinal spots

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
