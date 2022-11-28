open Mdrp_lib

let hexa_to_bin = function
  | '0' -> "0000"
  | '1' -> "0001"
  | '2' -> "0010"
  | '3' -> "0011"
  | '4' -> "0100"
  | '5' -> "0101"
  | '6' -> "0110"
  | '7' -> "0111"
  | '8' -> "1000"
  | '9' -> "1001"
  | 'A' -> "1010"
  | 'B' -> "1011"
  | 'C' -> "1100"
  | 'D' -> "1101"
  | 'E' -> "1110"
  | 'F' -> "1111"
  | _ -> assert false

module Value = struct
  type op =
    | Sum
    | Mult
    | Min
    | Max
    | Greater of (int -> int -> bool)
    | Less of (int -> int -> bool)
    | Equal of (int -> int -> bool)

  let pp_op ppf op =
    Format.fprintf ppf "%s"
      (match op with
      | Sum -> "+"
      | Mult -> "*"
      | Min -> "min"
      | Max -> "max"
      | Greater _ -> ">"
      | Less _ -> "<"
      | Equal _ -> "=")

  let of_int = function
    | 0 -> Sum
    | 1 -> Mult
    | 2 -> Min
    | 3 -> Max
    | 5 -> Greater ( > )
    | 6 -> Less ( < )
    | 7 -> Equal ( = )
    | _ -> assert false

  type value = Literal of int | Operator of op * t list
  and t = { version : int; value : value }

  let make version value = { version; value }

  let rec pp ppf { value; _ } =
    match value with
    | Literal value -> Format.fprintf ppf "Lit(%d)" value
    | Operator (op, vl) ->
        Format.fprintf ppf "Ope(%a, %a)" pp_op op (List.pp pp) vl

  let sum_version v =
    let rec aux acc { version; value } =
      match value with
      | Literal _ -> version + acc
      | Operator (_, l) -> List.fold_left aux (acc + version) l
    in
    aux 0 v

  let rec compute { value; _ } =
    match value with
    | Literal v -> v
    | Operator ((Greater op | Less op | Equal op), [ v1; v2 ]) ->
        if op (compute v1) (compute v2) then 1 else 0
    | Operator (op, l) -> (
        match op with
        | Sum -> List.fold_left (fun acc v -> acc + compute v) 0 l
        | Mult -> List.fold_left (fun acc v -> acc * compute v) 1 l
        | Min -> List.fold_left (fun acc v -> min acc (compute v)) max_int l
        | Max -> List.fold_left (fun acc v -> max acc (compute v)) 0 l
        | _ -> assert false)
end

open Value

let to_bits s = String.fold (fun acc c -> acc ^ hexa_to_bin c) "" s

let header s i =
  ( Int.Decimal.of_bin @@ String.sub s i 3,
    Int.Decimal.of_bin @@ String.sub s (i + 3) 3 )

(* Returns the computed literal and the next offset *)
let literal_value version s offset =
  let rec aux acc offset =
    let v = String.sub s (offset + 1) 4 in
    let acc = acc ^ v in
    if s.[offset] = '0' then
      (make version (Literal (Int.Decimal.of_bin acc)), offset + 5)
    else aux acc (offset + 5)
  in
  aux "" offset

(* 001 110 0 000000000011011 11010001010 01010010001001000000000 *)
(* VVV TTT I LLLLLLLLLLLLLLL AAAAAAAAAAA BBBBBBBBBBBBBBBB *)
(* 012 345 6 789012345678901 23456789012 3456789012345678 *)
(*           7  10        20 22      30  33     40 *)

let parse s =
  let s = to_bits s in
  let rec aux aoffset =
    try
      let version, type_id = header s aoffset in
      match type_id with
      | 4 -> Some (literal_value version s (aoffset + 6))
      | i -> (
          match s.[aoffset + 6] with
          | '0' ->
              let length = Int.Decimal.of_bin (String.sub s (aoffset + 7) 15) in
              let rec get_list acc goffset =
                (* 3 V bits + 3 T bits + 1 I bit + 15 Length bits = 22 bits of header *)
                if goffset - aoffset - 22 = length then acc
                else
                  match aux goffset with
                  | Some (v, new_offset) -> get_list (v :: acc) new_offset
                  | None -> acc
              in
              Some
                ( make version
                    (Operator (of_int i, List.rev (get_list [] (aoffset + 22)))),
                  aoffset + 22 + length )
          | '1' ->
              let packets =
                Int.Decimal.of_bin (String.sub s (aoffset + 7) 11)
              in
              let rec get_list acc cptp goffset =
                if packets = cptp then (acc, goffset)
                else
                  match aux goffset with
                  | Some (v, new_offset) ->
                      get_list (v :: acc) (cptp + 1) new_offset
                  | None -> (acc, goffset)
              in
              let list, offset = get_list [] 0 (aoffset + 18) in
              Some (make version (Operator (of_int i, List.rev list)), offset)
          | _ -> assert false)
    with Invalid_argument _ -> None
  in
  aux 0

let part_1 file =
  Parse.fold_lines
    (fun () s ->
      let v, _off = parse s |> Option.get in
      let vsum = sum_version v in
      Format.printf "%d@." vsum)
    () file

let part_2 file =
  Parse.fold_lines
    (fun () s ->
      let v, _off = parse s |> Option.get in
      let vsum = compute v in
      Format.printf "%d@." vsum)
    () file

let () =
  let part = try Sys.argv.(1) with _ -> "2" in
  let file = try Sys.argv.(2) with _ -> "input" in
  match part with "1" -> part_1 file | "2" -> part_2 file | _ -> ()
