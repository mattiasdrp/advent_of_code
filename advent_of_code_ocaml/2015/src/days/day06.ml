open Mdrp_lib

module Grid = struct
  let create e = Array.make_matrix 1000 1000 e

  let turn_on (x1, y1) (x2, y2) t =
    for x = x1 to x2 do
      for y = y1 to y2 do
        t.(x).(y) <- true
      done
    done

  let turn_off (x1, y1) (x2, y2) t =
    for x = x1 to x2 do
      for y = y1 to y2 do
        t.(x).(y) <- false
      done
    done

  let toggle (x1, y1) (x2, y2) t =
    for x = x1 to x2 do
      for y = y1 to y2 do
        t.(x).(y) <- not t.(x).(y)
      done
    done

  let parse_line line =
    let re =
      Str.regexp
        {|\(turn on\|turn off\|toggle\) \([0-9]+\),\([0-9]+\) through \([0-9]+\),\([0-9]+\)|}
    in
    if Str.string_match re line 0 then
      let order = Str.matched_group 1 line in
      let x1 = int_of_string @@ Str.matched_group 2 line in
      let y1 = int_of_string @@ Str.matched_group 3 line in
      let x2 = int_of_string @@ Str.matched_group 4 line in
      let y2 = int_of_string @@ Str.matched_group 5 line in
      (order, (x1, y1), (x2, y2))
    else assert false

  let execute_basic line t =
    let order, c1, c2 = parse_line line in
    match order with
    | "turn on" -> turn_on c1 c2 t
    | "turn off" -> turn_off c1 c2 t
    | _ -> toggle c1 c2 t

  let count_basic t =
    Array.fold_left
      (fun acc a ->
        Array.fold_left (fun acc e -> if e then acc + 1 else acc) acc a)
      0 t

  let turn_on (x1, y1) (x2, y2) t =
    for x = x1 to x2 do
      for y = y1 to y2 do
        t.(x).(y) <- t.(x).(y) + 1
      done
    done

  let turn_off (x1, y1) (x2, y2) t =
    for x = x1 to x2 do
      for y = y1 to y2 do
        t.(x).(y) <- max 0 (t.(x).(y) - 1)
      done
    done

  let toggle (x1, y1) (x2, y2) t =
    for x = x1 to x2 do
      for y = y1 to y2 do
        t.(x).(y) <- t.(x).(y) + 2
      done
    done

  let execute_advanced line t =
    let order, c1, c2 = parse_line line in
    match order with
    | "turn on" -> turn_on c1 c2 t
    | "turn off" -> turn_off c1 c2 t
    | _ -> toggle c1 c2 t

  let count_advanced t =
    Array.fold_left (fun acc a -> Array.fold_left ( + ) acc a) 0 t
end

let part_1 file =
  let grid = Grid.create false in
  Parse.fold_lines (fun () line -> Grid.execute_basic line grid) () file;
  Grid.count_basic grid

let part_2 file =
  let grid = Grid.create 0 in
  Parse.fold_lines (fun () line -> Grid.execute_advanced line grid) () file;
  Grid.count_advanced grid

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
