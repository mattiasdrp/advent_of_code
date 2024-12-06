open Mdrp_lib

module Coord = struct
  include Pair.Make (Int) (Int)

  let rotate (row, col) = (col, -row)
  let move (row, col) (dir_row, dir_col) = (row + dir_row, col + dir_col)

  let ouf_of_bounds (row, col) width height =
    row < 0 || row > height || col < 0 || col > width
end

module CoordSet = Set.Make (Coord)

module Visited = Set.Make (struct
  type t = Coord.t * Coord.t

  let compare (p1, d1) (p2, d2) =
    let c = Coord.compare p1 p2 in
    if c = 0 then Coord.compare d1 d2 else c

  let pp ppf (p, d) = Format.fprintf ppf "%a:%a" Coord.pp p Coord.pp d
end)

type exit = Loop | Out of CoordSet.t

let traverse pos width height obstacles =
  let rec aux pos dir explored visited =
    let npos = Coord.move pos dir in
    if Coord.ouf_of_bounds npos width height then Out explored
    else if CoordSet.mem npos obstacles then
      aux pos (Coord.rotate dir) explored visited
    else if Visited.mem (npos, dir) visited then Loop
    else
      aux npos dir
        (CoordSet.add npos explored)
        (Visited.add (npos, dir) visited)
  in
  let dir = (-1, 0) in
  aux pos dir (CoordSet.singleton pos) Visited.(singleton (pos, dir))

let parse_grid file =
  let height, (obstacles, init_pos), width =
    Parse.fold_lines
      (fun (row, (obstacles, init), _) line ->
        ( row + 1,
          String.foldi
            (fun col (obstacles, init) char ->
              match char with
              | '#' -> (CoordSet.add (row, col) obstacles, init)
              | '^' -> (obstacles, (row, col))
              | _ -> (obstacles, init))
            (obstacles, init) line,
          String.length line ))
      (0, (CoordSet.empty, (0, 0)), 0)
      file
  in
  let width, height = (width - 1, height - 1) in
  (width, height, obstacles, init_pos)

let part_1 file =
  let width, height, obstacles, init_pos = parse_grid file in
  match traverse init_pos width height obstacles with
  | Out explored -> CoordSet.cardinal explored
  | Loop -> assert false

let part_2 file =
  let width, height, obstacles, init_pos = parse_grid file in
  let explored =
    match traverse init_pos width height obstacles with
    | Out explored -> explored
    | Loop -> assert false
  in
  let explored = CoordSet.remove init_pos explored in
  CoordSet.fold
    (fun pos cpt ->
      match traverse init_pos width height (CoordSet.add pos obstacles) with
      | Out _ -> cpt
      | Loop -> cpt + 1)
    explored 0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
