open Mdrp_lib

module Cell = struct
  type t = Empty of int | Wall | Reindeer | End

  let of_char = function
    | '.' -> Empty max_int
    | '#' -> Wall
    | 'S' -> Reindeer
    | 'E' -> End
    | _ -> assert false

  let pp ppf t =
    Format.fprintf ppf "%c"
      (match t with
      | Empty _ -> '.'
      | Wall -> '#'
      | Reindeer -> 'S'
      | End -> 'E')
end

module Coords = struct
  type t = { row : int; col : int }

  let compare = compare
  let move { row; col } (drow, dcol) = { row = row + drow; col = col + dcol }
  let pp ppf { row; col } = Format.fprintf ppf "(%d, %d)" row col
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
    Coords.{ row; col }
end

module Dir = struct
  type t = North | East | South | West

  let rotate_left dir =
    match dir with
    | North -> ((0, -1), West)
    | East -> ((-1, 0), North)
    | South -> ((0, 1), East)
    | West -> ((1, 0), South)

  let rotate_right dir =
    match dir with
    | North -> ((0, 1), East)
    | East -> ((1, 0), South)
    | South -> ((0, -1), West)
    | West -> ((-1, 0), North)

  let stay = function
    | North -> (-1, 0)
    | East -> (0, 1)
    | South -> (1, 0)
    | West -> (0, -1)

  let compare = Stdlib.compare
  let all_dirs dir = (stay dir, rotate_left dir, rotate_right dir)

  let pp ppf t =
    Format.fprintf ppf "%s"
      (match t with
      | North -> "North"
      | West -> "West"
      | East -> "East"
      | South -> "South")
end

module Coord_dir = struct
  type t = Coords.t * Dir.t

  let pp _ _ = ()
  let compare = compare
end

module Visited = Set.Make (Coord_dir)

module Element = struct
  type t = { dist : int; pos : Coords.t; dir : Dir.t; path : Coord_dir.t list }

  let pp ppf { dist; pos; dir; _ } =
    Format.fprintf ppf "{dist: %d; pos: %a; dir: %a}" dist Coords.pp pos Dir.pp
      dir

  let compare t1 t2 =
    let c = compare t1.dist t2.dist in
    if c = 0 then
      let c1 = compare t1.pos t2.pos in
      if c1 = 0 then
        let c2 = compare t1.dir t2.dir in
        if c2 = 0 then List.compare Coords.compare t1.path t2.path else c2
      else c1
    else c
end

module Queue = Set.Make (Element)
module Min_paths = Set.Make (Coord_dir)

let traverse grid start =
  let open Element in
  let rec aux queue visited ((min_dist, min_paths) as mins) =
    (* Format.eprintf "%a@." Coords.pp pos; *)
    (* Format.eprintf "Queue: @[<v 0>%a@." *)
    (*   Queue.(pp ~pp_sep:Format.pp_print_cut ()) *)
    (*   queue; *)
    if Queue.is_empty queue then mins
    else
      let next = Queue.min_elt queue in
      (* Format.eprintf "Min_dist: %d@.Next:@.  %a@." min_dist Element.pp next; *)
      let queue = Queue.remove next queue in
      let queue, visited, mins =
        if grid.(next.pos.row).(next.pos.col) = Cell.End then
          (* Format.eprintf "Reached end!@."; *)
          let mins =
            if next.dist < min_dist then
              (* Format.eprintf "New min!@."; *)
              (next.dist, Min_paths.of_list next.path)
            else if next.dist = min_dist then
              ( (* Format.eprintf "Adding to min!@."; *)
                min_dist,
                List.fold_left
                  (fun acc pos -> Min_paths.add pos acc)
                  min_paths next.path )
            else mins
          in
          (queue, visited, mins)
        else if
          Visited.mem (next.pos, next.dir) visited
          && Min_paths.mem (next.pos, next.dir) min_paths
        then
          ( queue,
            visited,
            ( min_dist,
              List.fold_left
                (fun acc pos -> Min_paths.add pos acc)
                min_paths next.path ) )
        else if
          next.dist > min_dist
          || grid.(next.pos.row).(next.pos.col) = Cell.Wall
          || Visited.mem (next.pos, next.dir) visited
        then (queue, visited, mins)
        else
          let visited = Visited.add (next.pos, next.dir) visited in
          (* Format.eprintf "%a@." Cell.pp grid.(row).(col); *)
          let stay_delta, (left_delta, left_dir), (right_delta, right_dir) =
            Dir.all_dirs next.dir
          in
          let path = (next.pos, next.dir) :: next.path in
          let queue =
            Queue.add
              {
                next with
                pos = Coords.move next.pos stay_delta;
                dist = next.dist + 1;
                path;
              }
              queue
            |> Queue.add
                 {
                   pos = Coords.move next.pos left_delta;
                   dist = next.dist + 1001;
                   dir = left_dir;
                   path;
                 }
            |> Queue.add
                 {
                   pos = Coords.move next.pos right_delta;
                   dist = next.dist + 1001;
                   dir = right_dir;
                   path;
                 }
          in
          (queue, visited, mins)
      in
      aux queue visited mins
  in
  aux
    (Queue.singleton { dist = 0; pos = start; dir = East; path = [] })
    Visited.empty (max_int, Min_paths.empty)

let part_1 file =
  let grid = Matrix.parse file in
  Format.eprintf "@[<v 0>%a@." Matrix.pp grid;
  let start = Matrix.find_start grid in
  let min_score, _ = traverse grid start in
  min_score

module Spots = Set.Make (Coords)

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
  (* Format.eprintf "@[<v 0>%a@." Matrix.pp grid; *)
  let start = Matrix.find_start grid in
  let min_dist, min_paths = traverse grid start in
  let best_spots =
    Min_paths.fold
      (fun (coord, _) acc -> Spots.add coord acc)
      min_paths Spots.empty
  in
  Format.eprintf "@[<v 0>%a@.%d@." (pp best_spots) grid min_dist;
  Spots.cardinal best_spots

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
