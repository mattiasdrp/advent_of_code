open Mdrp_lib

module Coord = struct
  include Pair.Make (Int) (Int)

  let rotate (row, col) = (col, -row)
  let move (row, col) (dir_row, dir_col) = (row + dir_row, col + dir_col)

  let ouf_of_bounds (row, col) width height =
    row < 0 || row > height || col < 0 || col > width
end

module CoordSet = Set.Make (Coord)

let traverse pos width height obstacles =
  let rec aux pos dir explored =
    Format.eprintf "%a, %a@." Coord.pp pos Coord.pp dir;
    let npos = Coord.move pos dir in
    if Coord.ouf_of_bounds npos width height then explored
    else if CoordSet.mem npos obstacles then aux pos (Coord.rotate dir) explored
    else aux npos dir (CoordSet.add npos explored)
  in
  aux pos (-1, 0) (CoordSet.singleton pos)

let part_1 file =
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
  Format.eprintf "%a@.%a@." Coord.pp init_pos CoordSet.(pp ()) obstacles;
  let explored = traverse init_pos width height obstacles in
  CoordSet.cardinal explored

let part_2 _file = failwith "TODO"
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
