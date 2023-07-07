open Mdrp_lib

let part_1 file =
  let ci = open_in file in
  let depart = input_line ci |> int_of_string in
  let busses =
    input_line ci |> String.split_on_char ','
    |> List.filter_map (function "x" -> None | s -> Some (int_of_string s))
  in
  let id, earliest =
    List.fold_left
      (fun (ide, earliest) id ->
        let res = depart / id * id in
        let res = if res < depart then res + id else res in
        if res < earliest then (id, res) else (ide, earliest))
      (0, max_int) busses
  in
  (earliest - depart) * id

let part_2 file =
  let ci = open_in file in
  let _ = input_line ci in
  let busses =
    input_line ci |> String.split_on_char ','
    |> List.fold_left
         (fun (busses, cpt) -> function
           | "x" -> (busses, cpt + 1)
           | s -> ((-cpt, int_of_string s) :: busses, cpt + 1))
         ([], 0)
    |> fst |> List.rev
  in
  Int.Decimal.Modular_Arithmetic.chinese_remainder busses

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
