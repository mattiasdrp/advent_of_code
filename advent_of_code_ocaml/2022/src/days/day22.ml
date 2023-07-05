open Mdrp_lib

module Square = struct
  module Side = struct
    type t = North | West | East | South

    let compare = compare

    let pp ppf t =
      Format.fprintf ppf "%s"
        (match t with North -> "N" | South -> "S" | East -> "E" | West -> "W")

    let to_int = function North -> 0 | West -> 1 | South -> 2 | East -> 3

    let of_int = function
      | 0 -> North
      | 1 -> West
      | 2 -> South
      | 3 -> East
      | _ -> assert false

    let rotation t1 t2 =
      match (t1, t2) with
      | North, South | South, North | West, East | East, West -> 0
      | North, West | West, South | South, East | East, North -> 1
      | North, North | West, West | East, East | South, South -> 2
      | West, North | North, East | East, South | South, West -> 3

    let travel square_size t1 t2 ~pos:(col, row) ~deltas:(delta_col, delta_row)
        =
      match (t1, t2) with
      | North, West | West, North | South, East | East, South ->
          ((row, col), (-delta_row, -delta_col))
      | North, East | East, North | South, West | West, South ->
          ( (square_size - row - 1, square_size - col - 1),
            (delta_row, delta_col) )
      | North, South | South, North ->
          ((col, square_size - row - 1), (delta_col, delta_row))
      | West, East | East, West ->
          ((square_size - col - 1, row), (delta_col, delta_row))
      | North, North | South, South ->
          ((square_size - col - 1, row), (-delta_col, -delta_row))
      | West, West | East, East ->
          ((col, square_size - row - 1), (-delta_col, -delta_row))

    let rotate t r = (to_int t + r) mod 4 |> of_int
  end

  module Sides = Map.Make (Side)

  type t = {
    id : int;
    size : int;
    content : char Array.t Array.t;
    sides : (int * Side.t) Sides.t;
    row : int;
    col : int;
  }

  let pp ppf { sides; row; col; _ } =
    Format.fprintf ppf "@[<v 0>row, col: %d, %d@,sides: @[<v 1>%a@]@]"
      (* Array.Matrix.(pp Char.pp) *)
      (* content *)
      row col
      Sides.(pp (Pair.pp Int.pp Side.pp))
      sides

  let new_square squares side t ~pos ~deltas =
    let id_newt, side_newt = Sides.find side t.sides in
    let newt_side = Int.Map.find id_newt squares in
    let pos', deltas' = Side.travel t.size side side_newt ~pos ~deltas in
    (newt_side, pos', deltas')

  let travel squares ~pos ~deltas ~dist square =
    let rec aux ~pos ~deltas t dist =
      let col, row = pos in
      let delta_col, delta_row = deltas in
      if dist = 0 then (t, pos, deltas)
      else
        let square_size = t.size in
        let dist = dist - 1 in

        let col' = col + delta_col in
        let newt, ((col', row') as pos'), deltas' =
          if col' < 0 then new_square squares West t ~pos ~deltas
          else if col' = square_size then new_square squares East t ~pos ~deltas
          else
            let row' = row + delta_row in
            if row' < 0 then new_square squares North t ~pos ~deltas
            else if row' = square_size then
              new_square squares South t ~pos ~deltas
            else (t, (col', row'), deltas)
        in
        if newt.content.(row').(col') = '#' then (t, pos, deltas)
        else aux ~pos:pos' ~deltas:deltas' newt dist
    in

    aux ~pos ~deltas square dist
end

let dimensions file =
  let exception End of (int * int) in
  let width, height =
    try
      Parse.fold_lines
        (fun (width, height) s ->
          if s = "" then raise (End (width, height))
          else (max width (String.length s), height + 1))
        (0, 0) file
    with End res -> res
  in
  Int.Decimal.gcd width height

let fill_squares line row square_size acc =
  let module IM = Int.Map in
  let slices = String.slices line square_size in
  let acc =
    Seq.fold_lefti
      (fun ((squares, map, max_id) as acc) slice_id slice ->
        (* If the slice starts with ' ', don't do anything *)
        if slice.[0] = ' ' then acc
        else
          (* Otherwise, get the square corresponding to the position *)
          (* For this we associate each subsquare to an unique im_id *)
          (* and each im_id is associated to a filled square *)
          (* (or the association is created when first meeting one) *)
          let im_id = slice_id + (row / square_size * square_size) in
          let id, map, max_id =
            try (IM.find im_id map |> fst, map, max_id)
            with Not_found ->
              (max_id, IM.add im_id (max_id, (slice_id, row)) map, max_id + 1)
          in
          let square = IM.find id squares in
          let squares =
            if row mod square_size = 0 then
              let square =
                Square.{ square with row; col = slice_id * square_size }
              in
              IM.add id square squares
            else squares
          in

          (* This is not purely functional but we know what we're doing *)
          String.iteri
            (fun col c ->
              (* Matrices in OCaml are indexed by row and the column *)
              square.Square.content.(row mod square_size).(col) <- c)
            slice;
          (squares, map, max_id))
      acc slices
  in
  acc

(** The following algorithm connects sides that share a common square
   If we have:

   [  |1|
  |2|3|]

   1 and 2 have 3 in common
   The north part of 2 is then linked to the west part of 1

   Now, suppose we have:

   [  |4|
  |1|
|2|3|]

   This is the same as

   [  |4|
|2|1|
  |3|]

   But with the north part of 2 being merged with the west part of 1.
   So 2 is rotated by 90°, clockwise.
   By the same algorithm, its west part (which is its upper part in this figure)
   is linked to the west part of 4.

   The algorithm checks that for each square there is are squares attached to the following pairs (NW, NE, SW, SE). For each pair that has two squares attached, we check that the pair was not already checked (rotation-wise) and apply the linking formula (rotation-wise).

   By doing this recursively, all the parts have their 4 sides linked
*)
let connect_squares =
  let open Square in
  let square_ids = List.init 6 Fun.id in
  let list_sides =
    Side.[ (North, West); (North, East); (South, West); (South, East) ]
  in
  fun squares ->
    let rec aux squares =
      let fixpoint, squares =
        List.fold_left
          (fun (fixpoint, squares) id ->
            let square = Int.Map.find id squares in
            List.fold_left
              (fun (fixpoint, squares) (side1, side2) ->
                match
                  (Sides.find side1 square.sides, Sides.find side2 square.sides)
                with
                | (id1, side_sq1), (id2, side_sq2) ->
                    let square1 = Int.Map.find id1 squares in
                    if
                      Sides.mem
                        Side.(rotate side2 (rotation side_sq1 side1))
                        square1.sides
                    then (fixpoint, squares)
                    else
                      let square2 = Int.Map.find id2 squares in
                      let square1 =
                        {
                          square1 with
                          sides =
                            Sides.add
                              Side.(rotate side2 (rotation side_sq1 side1))
                              ( id2,
                                Side.(rotate side1 (rotation side_sq2 side2)) )
                              square1.sides;
                        }
                      in
                      let square2 =
                        {
                          square2 with
                          sides =
                            Sides.add
                              Side.(rotate side1 (rotation side_sq2 side2))
                              ( id1,
                                Side.(rotate side2 (rotation side_sq1 side1)) )
                              square2.sides;
                        }
                      in
                      ( false,
                        Int.Map.add id1 square1
                          (Int.Map.add id2 square2 squares) )
                | exception Not_found -> (fixpoint, squares))
              (fixpoint, squares) list_sides)
          (true, squares) square_ids
      in
      if fixpoint then squares else aux squares
    in
    aux squares

let connect_all_squares square_size pos_map squares =
  (* After this line the squares are all filled with their proper content *)
  (* Now we need to find their neighbours. *)
  (* Luckily, we have a map that associates to each position in the grid if a square is *)
  (* present or not so the first neighbours are easy to find. *)
  let normalize_row row = row / square_size * square_size in
  let squares =
    let open Square in
    let open Side in
    Int.Map.fold
      (fun _ (id, (col, row)) squares ->
        let square = Int.Map.find id squares in

        (* 0 - 1 - 2 - 3 *)
        (* In this configuration, the square at 0 and the one at 3 touch *)
        let max_col = 3 in
        let square =
          let im_id =
            (if col = 0 then max_col else col - 1) + normalize_row row
          in
          match Int.Map.find im_id pos_map with
          | id, _ ->
              Square.
                { square with sides = Sides.add West (id, East) square.sides }
          | exception Not_found -> square
        in

        let square =
          let im_id =
            (if col = max_col then 0 else col + 1) + normalize_row row
          in
          match Int.Map.find im_id pos_map with
          | id, _ ->
              Square.
                { square with sides = Sides.add East (id, West) square.sides }
          | exception Not_found -> square
        in

        (* 0 - ss - 2ss - 3ss *)
        (* In this configuration, the square at 0 and the one at 3ss touch *)
        let max_row = 3 * square_size in
        let square =
          let im_id =
            col + normalize_row (if row = 0 then max_row else row - square_size)
          in
          match Int.Map.find im_id pos_map with
          | id, _ ->
              Square.
                { square with sides = Sides.add North (id, South) square.sides }
          | exception Not_found -> square
        in

        let square =
          let im_id =
            col + normalize_row (if row = max_row then 0 else row + square_size)
          in
          match Int.Map.find im_id pos_map with
          | id, _ ->
              Square.
                { square with sides = Sides.add South (id, North) square.sides }
          | exception Not_found -> square
        in
        Int.Map.add id square squares)
      pos_map squares
  in
  connect_squares squares

let parse_squares file =
  let square_size = dimensions file in
  let squares =
    let l =
      List.init 6 (fun i ->
          ( i,
            Square.
              {
                id = i;
                content = Array.make_matrix square_size square_size ' ';
                size = square_size;
                sides = Sides.empty;
                row = 0;
                col = 0;
              } ))
    in
    Int.Map.of_list l
  in
  let path, _, _, (squares, pos_map, _) =
    Parse.fold_lines
      (fun (path, last, row, acc) s ->
        if s = "" then (path, true, row, acc)
        else if last then (s, last, row, acc)
        else
          let acc = fill_squares s row square_size acc in
          (path, last, row + 1, acc))
      ("", false, 0, (squares, Int.Map.empty, 0))
      file
  in
  (connect_all_squares square_size pos_map squares, path)

module Path = struct
  type t = { content : string; index : int }
  type res = Move of int | Rotate of char

  let rec until_next_rotation t acc =
    match t.content.[t.index] with
    | 'L' | 'R' -> (t, acc)
    | c ->
        until_next_rotation
          { t with index = t.index + 1 }
          ((10 * acc) + Char.to_digit c)
    | exception Invalid_argument _ -> (t, acc)

  let next_move t =
    match t.content.[t.index] with
    | ('L' | 'R') as c -> (Rotate c, { t with index = t.index + 1 })
    | _ ->
        let t, res = until_next_rotation t 0 in
        (Move res, t)
end

let rec travel_path squares square ~pos ~deltas path =
  match Path.next_move path with
  | Move dist, path ->
      let square, pos, deltas =
        Square.travel squares ~pos ~deltas ~dist square
      in
      travel_path squares square ~pos ~deltas path
  | Rotate r, path ->
      let deltas =
        match r with
        | 'R' -> (-snd deltas, fst deltas)
        | 'L' -> (snd deltas, -fst deltas)
        | _ -> assert false
      in
      travel_path squares square ~pos ~deltas path
  | exception Invalid_argument _ -> (square, pos, deltas)

let part_1 _file = failwith "TODO"

let facing deltas =
  match deltas with
  | 1, 0 -> 0
  | 0, 1 -> 1
  | -1, 0 -> 2
  | 0, -1 -> 3
  | _ -> assert false

let part_2 file =
  let squares, path = parse_squares file in
  let path = Path.{ content = path; index = 0 } in
  let square = Int.Map.find 0 squares in
  let pos =
    ( Array.find_index (function '.' -> true | _ -> false) square.content.(0)
      |> Option.get,
      0 )
  in
  let square, (col, row), deltas =
    travel_path squares square ~pos ~deltas:(1, 0) path
  in
  let col = square.col + col + 1 in
  let row = square.row + row + 1 in
  let facing = facing deltas in
  (1000 * row) + (4 * col) + facing

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
