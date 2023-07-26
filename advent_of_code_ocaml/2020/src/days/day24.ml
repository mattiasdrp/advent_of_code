open Mdrp_lib

module Hexagon = struct
  type t = int * int
  type direction = East | SouthEast | SouthWest | NorthWest | West | NorthEast

  let next_hexagon (x, y) = function
    | NorthEast -> (x + 1, y - 1)
    | East -> (x + 1, y)
    | SouthEast -> (x, y + 1)
    | SouthWest -> (x - 1, y + 1)
    | West -> (x - 1, y)
    | NorthWest -> (x, y - 1)

  let neighbours =
    let directions =
      [ East; SouthEast; SouthWest; NorthWest; West; NorthEast ]
    in
    fun tile ->
      Seq.unfold
        (function [] -> None | dir :: t -> Some (next_hexagon tile dir, t))
        directions

  let to_int (x, y) = ((x + 100) * 200) + y + 100
  let of_int i = ((i / 200) - 100, (i mod 200) - 100)
  let () = ()
  let hash = to_int
  let equal (x1, y1) (x2, y2) = x1 = x2 && y1 = y2
end

module TileSet = struct
  include Int.Set

  let add elt t = add (Hexagon.to_int elt) t
  let remove elt t = remove (Hexagon.to_int elt) t
  let find elt t = find elt t |> Hexagon.of_int
  let mem elt t = mem (Hexagon.to_int elt) t
  let fold f t acc = fold (fun elt acc -> f (Hexagon.of_int elt) acc) t acc
  let iter f t = iter (fun elt -> f (Hexagon.of_int elt)) t
end

module TileHashtbl = Hashtbl.Make (Hexagon)

let black_tiles file =
  Parse.fold_lines
    (fun black_tiles line ->
      let rec aux i acc =
        match line.[i] with
        | 's' -> (
            match line.[i + 1] with
            | 'e' -> aux (i + 2) Hexagon.(next_hexagon acc SouthEast)
            | 'w' -> aux (i + 2) Hexagon.(next_hexagon acc SouthWest)
            | _ -> assert false)
        | 'n' -> (
            match line.[i + 1] with
            | 'e' -> aux (i + 2) Hexagon.(next_hexagon acc NorthEast)
            | 'w' -> aux (i + 2) Hexagon.(next_hexagon acc NorthWest)
            | _ -> assert false)
        | 'e' -> aux (i + 1) Hexagon.(next_hexagon acc East)
        | 'w' -> aux (i + 1) Hexagon.(next_hexagon acc West)
        | _ -> assert false
        | exception Invalid_argument _ -> acc
      in
      let to_flip = aux 0 (0, 0) in
      if TileSet.mem to_flip black_tiles then TileSet.remove to_flip black_tiles
      else TileSet.add to_flip black_tiles)
    TileSet.empty file

let part_1 file = black_tiles file |> TileSet.cardinal

let nb_of_black_tiles tiles tile =
  Seq.fold_left
    (fun acc neighbour -> if TileSet.mem neighbour tiles then acc + 1 else acc)
    0 (Hexagon.neighbours tile)

let game_of_life_v1 black_tiles =
  let next_black_tiles =
    TileSet.fold
      (fun tile acc ->
        Seq.fold_left
          (fun acc tile ->
            if nb_of_black_tiles black_tiles tile = 2 then TileSet.add tile acc
            else acc)
          acc (Hexagon.neighbours tile))
      black_tiles TileSet.empty
  in
  TileSet.fold
    (fun tile acc ->
      let nbbt = nb_of_black_tiles black_tiles tile in
      if nbbt = 0 || nbbt > 2 then acc else TileSet.add tile acc)
    black_tiles next_black_tiles

let loop black_tiles max_days =
  let rec aux day black_tiles =
    if day > max_days then TileSet.cardinal black_tiles
    else
      let black_tiles = game_of_life_v1 black_tiles in
      aux (day + 1) black_tiles
  in
  aux 1 black_tiles

let part_2 file =
  let black_tiles = black_tiles file in
  let res = loop black_tiles 100 in
  res

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
