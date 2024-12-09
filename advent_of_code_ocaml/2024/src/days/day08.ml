open Mdrp_lib
module Grid = Map.Make (Char)
module RevGrid = Map.Make (Pair.Make (Int) (Int))

type 'a grid = { width : int; height : int; grid : 'a }

let add_to_grid (row, col) c grid =
  if row >= 0 && row < grid.height && col >= 0 && col < grid.width then
    ({ grid with grid = RevGrid.add (row, col) c grid.grid }, true)
  else (grid, false)

let add_points part point (drow, dcol) c op rev_grid =
  let rec aux (row, col) rev_grid =
    let p = (op row drow, op col dcol) in
    let rev_grid, added = add_to_grid p c rev_grid in
    if part = 2 && added then aux p rev_grid else rev_grid
  in
  aux point rev_grid

let fill_revgrid part c rev_grid list =
  let rec aux rev_grid = function
    | [] -> rev_grid
    | (row1, col1) :: tl ->
        aux
          (List.fold_left
             (fun rev_grid (row2, col2) ->
               let delta = (row1 - row2, col1 - col2) in
               let rev_grid =
                 add_points part (row1, col1) delta c ( + ) rev_grid
               in
               add_points part (row2, col2) delta c ( - ) rev_grid)
             rev_grid tl)
          tl
  in
  aux rev_grid list

let parse file =
  let height, (grid, rev_grid) =
    Parse.fold_lines
      (fun (row, (grid, rev_grid)) string ->
        ( row + 1,
          String.foldi
            (fun col (grid, rev_grid) -> function
              | '.' -> (grid, rev_grid)
              | c ->
                  ( Grid.update c
                      (function
                        | None -> Some [ (row, col) ]
                        | Some l -> Some ((row, col) :: l))
                      grid,
                    RevGrid.add (row, col) c rev_grid ))
            (grid, rev_grid) string ))
      (0, (Grid.empty, RevGrid.empty))
      file
  in
  let width =
    let input = open_in file in
    let width = String.length (input_line input) in
    close_in input;
    width
  in
  (width, height, grid, rev_grid)

let pp ppf grid =
  Format.fprintf ppf "@[<v 0>";
  for row = 0 to grid.height - 1 do
    for col = 0 to grid.width - 1 do
      match RevGrid.find_opt (row, col) grid.grid with
      | Some c -> Format.fprintf ppf "%c" c
      | None -> Format.fprintf ppf "."
    done;
    Format.fprintf ppf "@,"
  done;
  Format.fprintf ppf "@]"

let common_part part file =
  let width, height, grid, rev_grid = parse file in
  let rev_grid =
    Grid.fold
      (fun c list rev_grid -> fill_revgrid part c rev_grid list)
      grid
      { width; height; grid = (if part = 1 then RevGrid.empty else rev_grid) }
  in
  RevGrid.cardinal rev_grid.grid

let run part file = common_part part file
