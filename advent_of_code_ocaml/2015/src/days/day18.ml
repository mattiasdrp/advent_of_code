open Mdrp_lib

let parse list line = String.to_array (fun x -> x) line :: list

let step_game_of_life part2 grid =
  let rows, columns = Array.Matrix.rows_columns grid in
  Array.init rows (fun row ->
      Array.init columns (fun col ->
          if
            part2
            && ((row = 0 && col = 0)
               || (row = rows - 1 && col = 0)
               || (row = 0 && col = columns - 1)
               || (row = rows - 1 && col = columns - 1))
          then '#'
          else
            let alive =
              Seq.fold_left
                (fun alive (row, col) ->
                  match grid.(row).(col) with '#' -> alive + 1 | _ -> alive)
                0
                (Array.Matrix.moore_neighbourhood grid ~row ~col)
            in
            match grid.(row).(col) with
            | '#' when alive = 2 || alive = 3 -> '#'
            | '.' when alive = 3 -> '#'
            | _ -> '.'))

let game_of_life part2 grid =
  let rec aux i grid =
    if i = 100 then
      Array.Matrix.fold_left
        (fun acc -> function '#' -> acc + 1 | _ -> acc)
        0 grid
    else aux (i + 1) (step_game_of_life part2 grid)
  in
  aux 0 grid

let part_1 file =
  let grid = Parse.fold_lines parse [] file |> List.rev |> Array.of_list in
  game_of_life false grid

let part_2 file =
  let grid = Parse.fold_lines parse [] file |> List.rev |> Array.of_list in
  let rows, columns = Array.Matrix.rows_columns grid in
  grid.(0).(0) <- '#';
  grid.(rows - 1).(0) <- '#';
  grid.(0).(columns - 1) <- '#';
  grid.(rows - 1).(columns - 1) <- '#';
  game_of_life true grid

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
