open Mdrp_lib

type op = Acc | Jmp | Nop

let of_string = function
  | "nop" -> Nop
  | "acc" -> Acc
  | "jmp" -> Jmp
  | _ -> assert false

type inst = { inst : op * int }

let compute prog =
  let rec aux i acc seen =
    if Int.Set.mem i seen then acc
    else
      let seen = Int.Set.add i seen in
      match prog.(i).inst with
      | Nop, _ -> aux (i + 1) acc seen
      | Jmp, j -> aux (i + j) acc seen
      | Acc, v -> aux (i + 1) (acc + v) seen
  in
  aux 0 0 Int.Set.empty

let parse file =
  Parse.fold_lines
    (fun acc line ->
      let inst =
        match String.split_on_char ' ' line with
        | [ op; value ] -> { inst = (of_string op, int_of_string value) }
        | _ -> assert false
      in
      inst :: acc)
    [] file

let part_1 file =
  let prog = parse file |> List.rev |> Array.of_list in
  compute prog

exception Result of int

let compute_switch prog =
  let rec aux switched i acc seen =
    if Int.Set.mem i seen then ()
    else
      let seen = Int.Set.add i seen in
      match prog.(i).inst with
      | Nop, j ->
          aux switched (i + 1) acc seen;
          if switched || j = 0 then () else aux true (i + j) acc seen
      | Jmp, j ->
          aux switched (i + j) acc seen;
          if switched then () else (aux true (i + 1) acc) seen
      | Acc, v -> aux switched (i + 1) (acc + v) seen
      | exception Invalid_argument _ -> raise (Result acc)
  in
  try
    aux false 0 0 Int.Set.empty;
    assert false
  with Result acc -> acc

let part_2 file =
  let prog = parse file |> List.rev |> Array.of_list in
  compute_switch prog

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
