open Mdrp_lib

let part_1 file =
  let ci = open_in file in
  let rec aux acc curr =
    match input_char ci with
    | '-' -> aux acc "-"
    | '0' .. '9' as c -> aux acc (String.append_char curr c)
    | _ -> (
        match curr with
        | "" -> aux acc curr
        | _ -> aux (acc + int_of_string curr) "")
    | exception End_of_file ->
        close_in ci;
        acc
  in
  aux 0 ""

let part_2 file =
  let ci = open_in file in
  let line = input_line ci in
  close_in ci;
  let rec read_number i curr =
    match String.unsafe_get line i with
    | ('0' .. '9' | '-') as c -> read_number (i + 1) (String.append_char curr c)
    | _ -> (i, int_of_string curr)
  in
  let rec read_until_end ~i ~opened =
    match String.unsafe_get line i with
    | '{' -> read_until_end ~i:(i + 1) ~opened:(opened + 1)
    | '}' ->
        if opened = 0 then i + 1
        else read_until_end ~i:(i + 1) ~opened:(opened - 1)
    | _ -> read_until_end ~i:(i + 1) ~opened
  in
  let rec aux_object ~acc ~opened i =
    match String.unsafe_get line i with
    | '-' | '0' .. '9' ->
        let i, n = read_number i "" in
        aux_object ~acc:(acc + n) ~opened i
    | '{' ->
        let i, inter_obj = aux_object ~acc:0 ~opened:0 (i + 1) in
        aux_object ~acc:(acc + inter_obj) ~opened i
    | '}' -> (i + 1, acc)
    | ':' -> (
        match String.sub line (i + 2) 3 with
        | "red" ->
            let i = read_until_end ~i:(i + 1) ~opened in
            (i, 0)
        | _ | (exception Invalid_argument _) -> aux_object ~acc ~opened (i + 1))
    | _ -> aux_object ~acc ~opened (i + 1)
  in
  let length = String.length line in
  let rec aux acc i =
    if i = length then acc
    else
      match String.unsafe_get line i with
      | '-' | '0' .. '9' ->
          let i, n = read_number i "" in
          aux (acc + n) i
      | '{' ->
          let i, res = aux_object ~acc:0 ~opened:0 (i + 1) in
          aux (acc + res) i
      | _ -> aux acc (i + 1)
  in

  aux 0 0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
