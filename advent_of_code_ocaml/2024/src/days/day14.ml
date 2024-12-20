open Mdrp_lib

module Robot = struct
  module Point = struct
    type t = { row : int; col : int }

    let pp ppf { row; col } = Format.fprintf ppf "(%d, %d)" row col
    let to_pair { row; col } = (row, col)
  end

  type t = { position : Point.t; velocity : Point.t }

  let pp ppf { position; velocity } =
    Format.fprintf ppf "{pos: %a; vel: %a}" Point.pp position Point.pp velocity

  let step ~height ~width { position; velocity } =
    let new_pos_row = position.row + velocity.row in
    let new_pos_col = position.col + velocity.col in
    let new_pos_row =
      if new_pos_row < 0 then height + new_pos_row
      else if new_pos_row >= height then new_pos_row - height
      else new_pos_row
    in
    let new_pos_col =
      if new_pos_col < 0 then width + new_pos_col
      else if new_pos_col >= width then new_pos_col - width
      else new_pos_col
    in
    { position = { row = new_pos_row; col = new_pos_col }; velocity }

  module PairMap = Map.Make (Pair.Make (Int) (Int))

  let pp_on_grid ~height ~width ppf robots =
    let robots =
      List.fold_left
        (fun map { position; _ } ->
          PairMap.update
            (position.row, position.col)
            (function Some c -> Some (c + 1) | None -> Some 1)
            map)
        PairMap.empty robots
    in
    Format.fprintf ppf "@[<v 0>";
    for row = 0 to height - 1 do
      for col = 0 to width - 1 do
        match PairMap.find (row, col) robots with
        | c -> Format.fprintf ppf "%d" c
        | exception Not_found -> Format.fprintf ppf " "
      done;
      Format.fprintf ppf "@,"
    done;
    Format.fprintf ppf "@]"

  let update (row, col) map =
    PairMap.update (row, col)
      (function Some c -> Some (c + 1) | None -> Some 1)
      map

  type quadrants = {
    up_left : int PairMap.t;
    up_right : int PairMap.t;
    down_left : int PairMap.t;
    down_right : int PairMap.t;
  }

  let quadrants ~height ~width robots =
    let mid_height = height / 2 in
    let mid_width = width / 2 in
    List.fold_left
      (fun quadrants { position; _ } ->
        if position.row < mid_height then
          (* upper quadrants *)
          if position.col < mid_width then
            {
              quadrants with
              up_left = update (Point.to_pair position) quadrants.up_left;
            }
          else if position.col > mid_width then
            {
              quadrants with
              up_right = update (Point.to_pair position) quadrants.up_right;
            }
          else quadrants
        else if position.row > mid_height then
          (* lower quadrants *)
          if position.col < mid_width then
            {
              quadrants with
              down_left = update (Point.to_pair position) quadrants.down_left;
            }
          else if position.col > mid_width then
            {
              quadrants with
              down_right = update (Point.to_pair position) quadrants.down_right;
            }
          else quadrants
        else quadrants)
      {
        up_left = PairMap.empty;
        up_right = PairMap.empty;
        down_left = PairMap.empty;
        down_right = PairMap.empty;
      }
      robots

  let quadrant_score quadrants =
    let nb_up_left =
      PairMap.fold (fun _ cpt acc -> acc + cpt) quadrants.up_left 0
    in
    let nb_up_right =
      PairMap.fold (fun _ cpt acc -> acc + cpt) quadrants.up_right 0
    in
    let nb_down_left =
      PairMap.fold (fun _ cpt acc -> acc + cpt) quadrants.down_left 0
    in
    let nb_down_right =
      PairMap.fold (fun _ cpt acc -> acc + cpt) quadrants.down_right 0
    in
    nb_up_left * nb_up_right * nb_down_left * nb_down_right
end

open Robot

let part_1 file =
  let height, width =
    if String.ends_with ~suffix:"example" file then (7, 11) else (103, 101)
  in
  let re = Re.compile (Re.Perl.re {|p=(\d+),(\d+) v=(-?\d+),(-?\d+)|}) in
  let robots =
    Parse.fold_lines
      (fun robots line ->
        let robot = Re.exec re line in
        let col, row =
          ( Re.Group.get robot 1 |> int_of_string,
            Re.Group.get robot 2 |> int_of_string )
        in
        let position = Point.{ row; col } in
        let vcol, vrow =
          ( Re.Group.get robot 3 |> int_of_string,
            Re.Group.get robot 4 |> int_of_string )
        in
        let velocity = Point.{ row = vrow; col = vcol } in
        { position; velocity } :: robots)
      [] file
  in
  let robots =
    let rec aux step robots =
      if step = 100 then robots
      else aux (step + 1) (List.map (Robot.step ~height ~width) robots)
    in
    aux 0 robots
  in
  Format.eprintf "@[<v 0>%a@." Robot.(pp_on_grid ~height ~width) robots;
  let quadrants = quadrants ~height ~width robots in
  quadrant_score quadrants

let part_2 file =
  let height, width =
    if String.ends_with ~suffix:"example" file then (7, 11) else (103, 101)
  in
  let re = Re.compile (Re.Perl.re {|p=(\d+),(\d+) v=(-?\d+),(-?\d+)|}) in
  let robots =
    Parse.fold_lines
      (fun robots line ->
        let robot = Re.exec re line in
        let col, row =
          ( Re.Group.get robot 1 |> int_of_string,
            Re.Group.get robot 2 |> int_of_string )
        in
        let position = Point.{ row; col } in
        let vcol, vrow =
          ( Re.Group.get robot 3 |> int_of_string,
            Re.Group.get robot 4 |> int_of_string )
        in
        let velocity = Point.{ row = vrow; col = vcol } in
        { position; velocity } :: robots)
      [] file
  in
  let memo = Hashtbl.create 19 in
  let rec aux step robots (min, robots_min, step_min) =
    let robots = List.map (Robot.step ~height ~width) robots in
    if Hashtbl.mem memo robots then (step_min, robots_min)
    else (
      Hashtbl.add memo robots ();
      let score =
        let quadrants = quadrants ~height ~width robots in
        quadrant_score quadrants
      in
      let min, robots_min, step_min =
        if score < min then (score, robots, step)
        else (min, robots_min, step_min)
      in
      aux (step + 1) robots (min, robots_min, step_min))
  in
  let step, robots = aux 1 robots (max_int, [], 0) in
  Format.eprintf "%a@." Robot.(pp_on_grid ~height ~width) robots;
  step

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
