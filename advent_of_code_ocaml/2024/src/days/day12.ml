open Mdrp_lib
module PairSet = Set.Make (Pair.Make (Int) (Int))

module Matrix = struct
  type field = { char : Char.t; perimeter : int; area : int; cells : PairSet.t }
  type cell = { content : Char.t; field_id : int; row : int; col : int }
  type t = cell Array.t Array.t

  let next_id =
    let id = ref 0 in
    fun () ->
      incr id;
      !id

  let of_list l : t =
    List.mapi
      (fun row string ->
        String.to_seq string
        |> Seq.mapi (fun col c ->
               let field_id = next_id () in
               { content = c; field_id; row; col })
        |> List.of_seq |> Array.of_list)
      l
    |> Array.of_list

  let pp_field ppf { char; perimeter; area; _ } =
    Format.fprintf ppf "{%c; perim: %d; area: %d}" char perimeter area

  let pp_cell ppf { content; field_id; row; col } =
    Format.fprintf ppf "{%c(%d, %d); %d}" content row col field_id

  let pp ppf (t : t) =
    Format.(
      pp_print_array ~pp_sep:pp_print_cut
        (pp_print_array ~pp_sep:(fun _ppf () -> ()) pp_cell)
        ppf)
      t

  let rec find_repr i reprs =
    match Int.Map.find i reprs with
    | repr -> if i = repr then repr else find_repr repr reprs
    | exception Not_found -> i

  let union_find t =
    let height = Array.length t in
    let width = Array.length t.(0) in

    let merge map reprs cell1 cell2 =
      let repr1 = find_repr cell1.field_id reprs in
      let repr2 = find_repr cell2.field_id reprs in
      let field, map =
        let field = Int.Map.find (find_repr cell1.field_id reprs) map in
        if repr1 = repr2 then
          (* cell1 and cell2 are already in the same field but were not linked
             reduce parameter by 1 *)
          ({ field with perimeter = field.perimeter - 2 }, map)
        else
          (* cell1 and cell2 were not already in the same field, add their perimeter
             but remove 2 from their touching side and set the area to the sum of both
             their areas *)
          let field2 = Int.Map.find (find_repr cell2.field_id reprs) map in

          ( {
              field with
              perimeter = field.perimeter + field2.perimeter - 2;
              area = field.area + field2.area;
              cells = PairSet.union field.cells field2.cells;
            },
            Int.Map.remove (max repr1 repr2) map )
      in
      let repr = min repr1 repr2 in
      let cell1 = { cell1 with field_id = repr } in
      t.(cell1.row).(cell1.col) <- cell1;
      let cell2 = { cell2 with field_id = repr } in
      t.(cell2.row).(cell2.col) <- cell2;
      (Int.Map.add repr field map, Int.Map.add (max repr1 repr2) repr reprs)
    in

    let attach map reprs row col nrow ncol =
      if nrow = height || ncol = width then (map, reprs)
      else
        let cell = t.(row).(col) in
        let cell2 = t.(nrow).(ncol) in
        if cell.content = cell2.content then
          let map, reprs = merge map reprs cell cell2 in
          (map, reprs)
        else (map, reprs)
    in

    let rec aux map reprs row col field_id =
      if row = height then map
      else if col = width then aux map reprs (row + 1) 0 field_id
      else
        let map, reprs = attach map reprs row col row (col + 1) in
        let map, reprs = attach map reprs row col (row + 1) col in
        aux map reprs row (col + 1) field_id
    in

    let map =
      Array.fold_left
        (fun map array ->
          Array.fold_left
            (fun map cell ->
              Int.Map.add cell.field_id
                {
                  char = cell.content;
                  area = 1;
                  perimeter = 4;
                  cells = PairSet.singleton (cell.row, cell.col);
                }
                map)
            map array)
        Int.Map.empty t
    in
    aux map Int.Map.empty 0 0 0
end

let part_1 file =
  let fields = Parse.lines file |> Matrix.of_list |> Matrix.union_find in
  Int.Map.fold
    (fun _ Matrix.{ perimeter; area; _ } acc -> acc + (perimeter * area))
    fields 0

let check_and_add cells (row, col) (drow, dcol) (corners, double) =
  let mem pair = PairSet.mem pair cells in
  let cell_diag = mem (row + drow, col + dcol) in
  let cell_row = mem (row, col + dcol) in
  let cell_col = mem (row + drow, col) in
  let double =
    (* Corner case when we have
       AX
       XA
       This is the same corner but it's actually 2 corners *)
    if cell_diag && (not cell_col) && not cell_row then double + 1 else double
  in
  if
    (* if the cell is filled, it's a corner if one of its neighbour is empty *)
    (cell_diag && ((not cell_row) || not cell_col))
    (* if the cell is empty, it's a corner if both its neighbours are empty or both are filled *)
    || (not cell_diag)
       && ((cell_row && cell_col) || not (cell_row || cell_col))
  then
    let row = if drow = -1 then row else row + drow in
    let col = if dcol = -1 then col else col + dcol in
    (PairSet.add (row, col) corners, double)
  else (corners, double)

let sides cells =
  let check_and_add = check_and_add cells in
  let rec walk cell_l acc =
    match cell_l with
    | [] -> acc
    | (row, col) :: tl ->
        check_and_add (row, col) (-1, -1) acc
        |> check_and_add (row, col) (-1, 1)
        |> check_and_add (row, col) (1, -1)
        |> check_and_add (row, col) (1, 1)
        |> walk tl
  in
  walk (PairSet.elements cells) (PairSet.empty, 0)

let part_2 file =
  let fields = Parse.lines file |> Matrix.of_list |> Matrix.union_find in
  Int.Map.fold
    (fun _ Matrix.{ area; cells; _ } acc ->
      let corners, doubles = sides cells in
      (* Don't forget that doubles has been counted twice so divide it by 2 *)
      acc + ((PairSet.cardinal corners + (doubles / 2)) * area))
    fields 0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
