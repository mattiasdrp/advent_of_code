open Mdrp_lib

module Cell = struct
  type dir = East | South
  type t = { dir : dir; row : int; col : int }

  let is_south c = c = South
  let is_east c = c = East

  let compare t1 t2 =
    let c1 = Int.compare t1.row t2.row in
    if c1 = 0 then
      let c2 = Int.compare t1.col t2.col in
      if c2 = 0 then Stdlib.compare t1.dir t2.dir else c2
    else c1

  let pp ppf c =
    Format.fprintf ppf "%s" (match c.dir with East -> ">" | South -> "v")

  let pp_det ppf c =
    Format.fprintf ppf "{%s;%d;%d}"
      (match c.dir with East -> ">" | South -> "v")
      c.row c.col
end

module CSet = Set.Make (Cell)

module Grid = struct
  type content = Cell of Cell.t | Empty
  type t = { grid : content array array; columns : int; rows : int }

  let init file =
    let ci = open_in file in
    let rec aux_parse row acc =
      match input_line ci with
      | s ->
          aux_parse (row + 1)
            (Array.init (String.length s) (fun col ->
                 match s.[col] with
                 | '.' -> Empty
                 | '>' -> Cell { dir = East; row; col }
                 | 'v' -> Cell { dir = South; row; col }
                 | _ -> assert false)
            :: acc)
      | exception End_of_file ->
          close_in ci;
          List.rev acc |> Array.of_list
    in
    let t = aux_parse 0 [] in
    let res =
      Array.fold_lefti
        (fun _ (east, south) a ->
          Array.fold_lefti
            (fun _ ((east, south) as acc) c ->
              match c with
              | Cell ({ dir = East; _ } as c) -> (CSet.add c east, south)
              | Cell ({ dir = South; _ } as c) -> (east, CSet.add c south)
              | Empty -> acc)
            (east, south) a)
        (CSet.empty, CSet.empty) t
    in
    let rows, columns = Array.Matrix.rows_columns t in
    ( { grid = t; columns; rows },
      res,
      { grid = Array.make_matrix rows columns Empty; columns; rows },
      Array.make columns Empty,
      Array.make rows Empty )

  let pp_cell ppf c =
    match c with
    | Cell c -> Format.fprintf ppf "%a" Cell.pp c
    | Empty -> Format.fprintf ppf "."

  let pp ppf { grid; _ } =
    Format.fprintf ppf "@[<v 0>";
    Array.iter
      (fun a ->
        Array.iter (Format.fprintf ppf "%a" pp_cell) a;
        Format.fprintf ppf "@,")
      grid;
    Format.fprintf ppf "@]"

  let pp_lt ppf lt = Array.iter (Format.fprintf ppf "%a" pp_cell) lt

  let move cell set t new_t left top =
    let open Cell in
    (* Possible new cell *)
    let n_row, n_col =
      match cell.dir with
      | East -> (cell.row, (cell.col + 1) mod t.columns)
      | South -> ((cell.row + 1) mod t.rows, cell.col)
    in
    (* Handling east so we check if the original array is empty or not *)
    (* If ni, nj is already occupied, don't go there *)
    let nc_row, nc_col, changed =
      if
        t.grid.(n_row).(n_col) <> Empty
        || new_t.grid.(n_row).(n_col) <> Empty
        || (n_row < cell.row && top.(cell.col) <> Empty)
        || (n_col < cell.col && left.(cell.row) <> Empty)
      then (cell.row, cell.col, false)
      else (n_row, n_col, true)
    in
    if cell.col = 0 && is_east cell.dir then
      left.(cell.row) <- t.grid.(cell.row).(cell.col);
    if cell.row = 0 && is_south cell.dir then
      top.(cell.col) <- t.grid.(cell.row).(cell.col);
    (* Format.eprintf "Left: %a@." pp_lt left; *)
    (* Format.eprintf "Top: %a@." pp_lt top; *)
    t.grid.(cell.row).(cell.col) <- Empty;
    let cell = { cell with row = nc_row; col = nc_col } in
    new_t.grid.(nc_row).(nc_col) <- Cell cell;
    (CSet.add cell set, changed)

  let reset lt =
    for i = 0 to Array.length lt - 1 do
      lt.(i) <- Empty
    done

  let step (east, south) t t' left top =
    let east, changed =
      CSet.fold
        (fun c (east, changed) ->
          let east, mc = move c east t t' left top in
          (east, changed || mc))
        east (CSet.empty, false)
    in
    let south, changed =
      CSet.fold
        (fun c (south, changed) ->
          let south, mc = move c south t t' left top in
          (south, changed || mc))
        south (CSet.empty, changed)
    in
    reset left;
    reset top;
    (t', t, (east, south), changed)
end

let part_1 grid sets empty left top =
  let rec aux grid empty sets i =
    (* Format.eprintf "%a@." List.(pp Cell.pp_det) (CSet.elements east); *)
    (* Format.eprintf "%a@." List.(pp Cell.pp_det) (CSet.elements south); *)
    (* Format.eprintf "%d: @.%a@." i Grid.pp grid; *)
    (* Format.eprintf "%d: @.%a@." i Grid.pp empty; *)
    let grid, empty, sets, changed = Grid.step sets grid empty left top in
    if changed then aux grid empty sets (i + 1) else i
  in
  aux grid empty sets 1

let run file =
  let grid, sets, empty, top, left = Grid.init file in
  part_1 grid sets empty left top
