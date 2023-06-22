open Mdrp_lib

module Dir = struct
  type t = N | S | E | W | L | R | F

  let pp ppf t =
    Format.fprintf ppf "%c"
      (match t with
      | N -> 'N'
      | S -> 'S'
      | E -> 'E'
      | W -> 'W'
      | L -> 'L'
      | R -> 'R'
      | F -> 'F')

  let of_char = function
    | 'N' -> N
    | 'S' -> S
    | 'E' -> E
    | 'W' -> W
    | 'L' -> L
    | 'R' -> R
    | 'F' -> F
    | _ -> assert false

  let to_int = function
    | E -> 0
    | N -> 90
    | W -> 180
    | S -> 270
    | _ -> assert false

  let of_int = function
    | 0 -> E
    | 90 -> N
    | 180 -> W
    | 270 -> S
    | _ -> assert false

  let rotate dir angle = (to_int dir + angle) mod 360 |> of_int
end

module Ferry = struct
  type t = { dir : Dir.t; x : int; y : int }

  let pp ppf { dir; x; y } = Format.fprintf ppf "%a, (%d, %d)" Dir.pp dir x y

  let rec move t dir value =
    match dir with
    | Dir.F -> move t t.dir value
    | N -> { t with y = t.y + value }
    | S -> { t with y = t.y - value }
    | W -> { t with x = t.x + value }
    | E -> { t with x = t.x - value }
    | R -> { t with dir = Dir.rotate t.dir (360 - value) }
    | L -> { t with dir = Dir.rotate t.dir value }

  let manhattan { x; y; _ } = abs x + abs y
end

let part_1 file =
  let ci = open_in file in
  let rec aux_parse ferry =
    match input_line ci with
    | s ->
        let dir = s.[0] |> Dir.of_char in
        let value = String.sub s 1 (String.length s - 1) |> int_of_string in
        aux_parse (Ferry.move ferry dir value)
    | exception End_of_file -> ferry
  in
  aux_parse { dir = Dir.E; x = 0; y = 0 }
  |> Ferry.manhattan |> Format.printf "%d@."

module FerWay = struct
  type t = { x : int; y : int }

  let pp ppf { x; y } = Format.fprintf ppf "(%d, %d)" x y

  let rotate { x; y } = function
    | 0 -> { x; y }
    | 90 -> { x = -y; y = x }
    | 180 -> { x = -x; y = -y }
    | 270 -> { x = y; y = -x }
    | _ -> assert false

  let move ferry waypoint dir value =
    match dir with
    | Dir.F ->
        ( {
            x = ferry.x + (value * waypoint.x);
            y = ferry.y + (value * waypoint.y);
          },
          waypoint )
    | N -> (ferry, { waypoint with y = waypoint.y + value })
    | S -> (ferry, { waypoint with y = waypoint.y - value })
    | E -> (ferry, { waypoint with x = waypoint.x + value })
    | W -> (ferry, { waypoint with x = waypoint.x - value })
    | R -> (ferry, rotate waypoint (360 - value))
    | L -> (ferry, rotate waypoint value)

  let manhattan { x; y } = abs x + abs y
end

let part_2 file =
  let ci = open_in file in
  let rec aux_parse (ferry, waypoint) =
    match input_line ci with
    | s ->
        let dir = s.[0] |> Dir.of_char in
        let value = String.sub s 1 (String.length s - 1) |> int_of_string in
        aux_parse (FerWay.move ferry waypoint dir value)
    | exception End_of_file -> ferry
  in
  aux_parse FerWay.({ x = 0; y = 0 }, { x = 10; y = 1 })
  |> FerWay.manhattan |> Format.printf "%d@."

let run part file =
  match part with 1 -> part_1 file | 2 -> part_2 file | _ -> ()
