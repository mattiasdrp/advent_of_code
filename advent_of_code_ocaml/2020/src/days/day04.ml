open Mdrp_lib

let excpected_fields =
  [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"; "cid" ]
  |> String.Set.of_list

let cid = String.Set.singleton "cid"

let validity passport =
  let fields = String.split_on_char_non_empty ' ' passport in
  let remaining =
    List.fold_left
      (fun remaining s ->
        match String.split_on_char ':' s with
        | [ field; _value ] -> String.Set.remove field remaining
        | _ -> assert false)
      excpected_fields fields
  in
  if String.Set.subset remaining cid then 1 else 0

let part_1 file =
  let ci = open_in file in
  let rec aux_parse passport acc =
    match input_line ci with
    | "" -> aux_parse "" (acc + validity passport)
    | s -> aux_parse (passport ^ " " ^ s) acc
    | exception End_of_file -> acc + validity passport
  in
  aux_parse "" 0

let byr y =
  let y = int_of_string y in
  y >= 1920 && y <= 2002

let iyr y =
  let y = int_of_string y in
  y >= 2010 && y <= 2020

let eyr y =
  let y = int_of_string y in
  y >= 2020 && y <= 2030

let hgt s =
  try
    let h = String.sub s 0 (String.length s - 2) |> int_of_string in
    (String.ends_with ~suffix:"cm" s && h >= 150 && h <= 193)
    || (String.ends_with ~suffix:"in" s && h >= 59 && h <= 76)
  with _ -> false

let hcl h =
  String.starts_with ~prefix:"#" h
  &&
  try
    ignore (int_of_string ("0x" ^ String.sub h 1 (String.length h - 1)));
    String.length h = 7
  with _ -> assert false

let ecl =
  let colors =
    String.Set.of_list [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
  in
  fun e -> String.Set.mem e colors

let pid i =
  try
    ignore (int_of_string i);
    String.length i = 9
  with _ -> false

let cid _ = false

let excpected_fields =
  [
    ("byr", byr);
    ("iyr", iyr);
    ("eyr", eyr);
    ("hgt", hgt);
    ("hcl", hcl);
    ("ecl", ecl);
    ("pid", pid);
    ("cid", cid);
  ]
  |> String.Map.of_list

let validity passport =
  let fields = String.split_on_char_non_empty ' ' passport in
  let remaining, valids =
    List.fold_left
      (fun (remaining, valids) s ->
        match String.split_on_char ':' s with
        | [ field; value ] ->
            let validator = String.Map.find field remaining in
            let valids = if validator value then valids + 1 else valids in
            (String.Map.remove field remaining, valids)
        | _ -> assert false)
      (excpected_fields, 0) fields
  in
  if
    valids = 7
    && (String.Map.is_empty remaining
       || (String.Map.cardinal remaining = 1 && String.Map.mem "cid" remaining)
       )
  then 1
  else 0

let part_2 file =
  let ci = open_in file in
  let rec aux_parse passport acc =
    match input_line ci with
    | "" -> aux_parse "" (acc + validity passport)
    | s -> aux_parse (passport ^ " " ^ s) acc
    | exception End_of_file -> acc + validity passport
  in
  aux_parse "" 0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
