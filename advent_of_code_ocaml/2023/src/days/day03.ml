open Mdrp_lib

module Number = struct
  type t = { value : int; row : int; cols : int * int }

  let check_around t p =
    let minc = fst t.cols - 1 in
    let maxc = snd t.cols + 1 in
    let rec aux row col =
      row <= t.row + 1
      &&
      if col > maxc then aux (row + 1) minc else p row col || aux row (col + 1)
    in
    aux (t.row - 1) minc

  let attached_gears t p gears =
    let minc = fst t.cols - 1 in
    let maxc = snd t.cols + 1 in
    let rec aux gears row col =
      if row > t.row + 1 then gears
      else if col > maxc then aux gears (row + 1) minc
      else
        let gears =
          if p row col then
            Int.PairMap.update (row, col)
              (function
                | None -> Some [ t.value ] | Some l -> Some (t.value :: l))
              gears
          else gears
        in
        aux gears row (col + 1)
    in

    aux gears (t.row - 1) minc

  let pp ppf { value; row; cols } =
    Format.fprintf ppf "{v: %d, r: %d, c: %a}" value row
      Pair.(pp Int.pp Int.pp)
      cols
end

let get_infos row list symbols gears line =
  let in_nb, curr_nb, size, list, symbols, gears =
    String.foldi
      (fun col (in_nb, curr_nb, size, list, symbols, gears) char ->
        match char with
        | '0' .. '9' ->
            ( true,
              (curr_nb * 10) + Char.to_digit char,
              size + 1,
              list,
              symbols,
              gears )
        | _ ->
            let symbols, gears =
              match char with
              | '.' -> (symbols, gears)
              | '*' ->
                  ( Int.PairSet.add (row, col) symbols,
                    Int.PairSet.add (row, col) gears )
              | _ -> (Int.PairSet.add (row, col) symbols, gears)
            in

            ( false,
              0,
              0,
              (if in_nb then
                 Number.{ value = curr_nb; row; cols = (col - size, col - 1) }
                 :: list
               else list),
              symbols,
              gears ))
      (false, 0, 0, list, symbols, gears)
      line
  in
  if in_nb then
    ( Number.
        {
          value = curr_nb;
          row;
          cols = (String.length line - size, String.length line - 1);
        }
      :: list,
      symbols,
      gears )
  else (list, symbols, gears)

let part_1 file =
  let _, (list, symbols, _gears) =
    Parse.fold_lines
      (fun (row, (list, symbols, gears)) line ->
        (row + 1, get_infos row list symbols gears line))
      (0, ([], Int.PairSet.empty, Int.PairSet.empty))
      file
  in
  List.fold_left
    (fun acc n ->
      if
        Number.check_around n (fun row col ->
            Int.PairSet.mem (row, col) symbols)
      then acc + n.Number.value
      else acc)
    0 list

let part_2 file =
  let _, (list, _, gears) =
    Parse.fold_lines
      (fun (row, (list, symbols, gears)) line ->
        (row + 1, get_infos row list symbols gears line))
      (0, ([], Int.PairSet.empty, Int.PairSet.empty))
      file
  in
  let p row col = Int.PairSet.mem (row, col) gears in
  let gears =
    List.fold_left
      (fun gears n -> Number.attached_gears n p gears)
      Int.PairMap.empty list
  in
  Int.PairMap.fold
    (fun _ l acc -> match l with [ e1; e2 ] -> acc + (e1 * e2) | _ -> acc)
    gears 0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
