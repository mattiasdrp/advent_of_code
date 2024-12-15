open Mdrp_lib

module Cell = struct
  type t = Empty | Box | Wall | Robot | LeftBox | RightBox

  let of_char = function
    | '.' -> Empty
    | 'O' -> Box
    | '#' -> Wall
    | '[' -> LeftBox
    | ']' -> RightBox
    | '@' -> Robot
    | _ -> assert false

  let pp ppf t =
    Format.fprintf ppf "%c"
      (match t with
      | Empty -> '.'
      | Box -> 'O'
      | Wall -> '#'
      | Robot -> '@'
      | LeftBox -> '['
      | RightBox -> ']')
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
end

module Move = struct
  type t = Left | Up | Right | Down

  let of_char = function
    | '>' -> Right
    | '<' -> Left
    | '^' -> Up
    | 'v' -> Down
    | _ -> raise Exit
end

let score grid =
  Matrix.fold_lefti
    (fun acc ~row ~col -> function
      | Cell.Box | Cell.LeftBox -> (100 * row) + col + acc
      | _ -> acc)
    0 grid

let part_1 file =
  let ci = open_in file in
  let rec parse_grid grid =
    match input_line ci with
    | "" -> List.rev grid
    | line -> parse_grid (String.to_array Cell.of_char line :: grid)
  in
  let grid = parse_grid [] |> Array.of_list in
  let robot =
    Matrix.fold_lefti
      (fun acc ~row ~col cell ->
        match cell with Cell.Robot -> (row, col) | _ -> acc)
      (0, 0) grid
  in

  let aux_move (row, col) (drow, dcol) =
    let rec aux ((row, col) as pos') =
      match grid.(row).(col) with
      | Wall -> None
      | Empty -> Some pos'
      | Box -> aux (row + drow, col + dcol)
      | _ -> assert false
    in
    let next_row, next_col = (row + drow, col + dcol) in
    match aux (next_row, next_col) with
    | Some (ending_row, ending_col) ->
        (* Robot is going here *)
        let next_cell = grid.(next_row).(next_col) in
        grid.(row).(col) <- Empty;
        (* The cell where robot is going is going to this cell
           which is the next empty cell *)
        grid.(ending_row).(ending_col) <- next_cell;
        (* We then move the robot *)
        grid.(next_row).(next_col) <- Robot;
        (next_row, next_col)
    | None -> (row, col)
  in

  let rec move pos =
    let pos =
      match Move.of_char (input_char ci) with
      | Move.Left -> aux_move pos (0, -1)
      | Move.Right -> aux_move pos (0, 1)
      | Move.Up -> aux_move pos (-1, 0)
      | Move.Down -> aux_move pos (1, 0)
      | exception Exit -> pos
    in
    move pos
  in
  (match move robot with
  | exception End_of_file -> close_in ci
  | _ -> assert false);
  score grid

let part_2 file =
  let ci = open_in file in

  let double line =
    String.to_list line
    |> List.fold_left
         (fun acc c ->
           match c with
           | ('#' | '.') as c -> c :: c :: acc
           | 'O' -> ']' :: '[' :: acc
           | '@' -> '.' :: '@' :: acc
           | _ -> assert false)
         []
    |> List.rev |> List.to_seq |> String.of_seq
  in
  let rec parse_grid grid =
    match input_line ci with
    | "" -> List.rev grid
    | line -> parse_grid (String.to_array Cell.of_char (double line) :: grid)
  in
  let grid = parse_grid [] |> Array.of_list in
  let robot =
    Matrix.fold_lefti
      (fun acc ~row ~col cell ->
        match cell with Cell.Robot -> (row, col) | _ -> acc)
      (0, 0) grid
  in

  let check_valid (row, col) (drow, dcol) vertical =
    let rec aux (row, col) =
      match grid.(row).(col) with
      | Empty -> true
      | Wall -> false
      | LeftBox ->
          aux (row + drow, col + dcol)
          && ((not vertical) || aux (row + drow, col + 1 + dcol))
      | RightBox ->
          aux (row + drow, col + dcol)
          && ((not vertical) || aux (row + drow, col - 1 + dcol))
      | Box | Robot -> assert false
    in
    aux (row + drow, col + dcol)
  in

  let move_for_real (row, col) (drow, dcol) vertical =
    let rec aux cell (row, col) =
      match grid.(row).(col) with
      | Empty -> grid.(row).(col) <- cell
      | LeftBox ->
          grid.(row).(col) <- cell;
          aux LeftBox (row + drow, col + dcol);
          if vertical then (
            grid.(row).(col + 1) <- Empty;
            aux RightBox (row + drow, col + 1 + dcol))
      | RightBox ->
          grid.(row).(col) <- cell;
          aux RightBox (row + drow, col + dcol);
          if vertical then (
            grid.(row).(col - 1) <- Empty;
            aux LeftBox (row + drow, col - 1 + dcol))
      | Box | Robot | Wall -> assert false
    in
    grid.(row).(col) <- Empty;
    aux Robot (row + drow, col + dcol);
    (row + drow, col + dcol)
  in
  let rec move pos =
    let delta, vertical =
      let rec aux () =
        match Move.of_char (input_char ci) with
        | Move.Left -> ((0, -1), false)
        | Move.Right -> ((0, 1), false)
        | Move.Up -> ((-1, 0), true)
        | Move.Down -> ((1, 0), true)
        | exception Exit -> aux ()
      in
      aux ()
    in
    let valid = check_valid pos delta vertical in
    let pos = if valid then move_for_real pos delta vertical else pos in
    move pos
  in
  (match move robot with
  | exception End_of_file -> close_in ci
  | _ -> assert false);
  score grid

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
