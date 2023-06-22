open Mdrp_lib

module Cell = struct
  type dir = East | South
  type t = { dir : dir; i : int; j : int }

  let is_south c = c = South
  let is_east c = c = East

  let compare t1 t2 =
    let c1 = Int.compare t1.i t2.i in
    if c1 = 0 then
      let c2 = Int.compare t1.j t2.j in
      if c2 = 0 then Stdlib.compare t1.dir t2.dir else c2
    else c1

  let pp ppf c =
    Format.fprintf ppf "%s" (match c.dir with East -> ">" | South -> "v")

  let pp_det ppf c =
    Format.fprintf ppf "{%s;%d;%d}"
      (match c.dir with East -> ">" | South -> "v")
      c.i c.j
end

module CSet = Set.Make (Cell)

module Grid = struct
  type content = Cell of Cell.t | Empty
  type t = { grid : content array array; width : int; height : int }

  let init file =
    let ci = open_in file in
    let rec aux_parse i acc =
      match input_line ci with
      | s ->
          aux_parse (i + 1)
            (Array.init (String.length s) (fun j ->
                 match s.[j] with
                 | '.' -> Empty
                 | '>' -> Cell { dir = East; i; j }
                 | 'v' -> Cell { dir = South; i; j }
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
    let width, height = Array.Matrix.width_height t in
    ( { grid = t; width; height },
      res,
      { grid = Array.make_matrix height width Empty; width; height },
      Array.make width Empty,
      Array.make height Empty )

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
    let ni, nj =
      match cell.dir with
      | East -> (cell.i, (cell.j + 1) mod t.width)
      | South -> ((cell.i + 1) mod t.height, cell.j)
    in
    (* Handling east so we check if the original array is empty or not *)
    (* If ni, nj is already occupied, don't go there *)
    let nci, ncj, changed =
      if
        t.grid.(ni).(nj) <> Empty
        || new_t.grid.(ni).(nj) <> Empty
        || (ni < cell.i && top.(cell.j) <> Empty)
        || (nj < cell.j && left.(cell.i) <> Empty)
      then (cell.i, cell.j, false)
      else (ni, nj, true)
    in
    if cell.j = 0 && is_east cell.dir then
      left.(cell.i) <- t.grid.(cell.i).(cell.j);
    if cell.i = 0 && is_south cell.dir then
      top.(cell.j) <- t.grid.(cell.i).(cell.j);
    (* Format.eprintf "Left: %a@." pp_lt left; *)
    (* Format.eprintf "Top: %a@." pp_lt top; *)
    t.grid.(cell.i).(cell.j) <- Empty;
    let cell = { cell with i = nci; j = ncj } in
    new_t.grid.(nci).(ncj) <- Cell cell;
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
  aux grid empty sets 1 |> Format.printf "%d@."

let run file =
  let grid, sets, empty, top, left = Grid.init file in
  part_1 grid sets empty left top
