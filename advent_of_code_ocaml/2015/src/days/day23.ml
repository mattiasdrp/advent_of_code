open Mdrp_lib

type instruction =
  | Edit of (int * int -> int * int)
  | Jump of int
  | JumpIf of (int * int -> bool) * int

let eval program regs =
  let rec aux pc regs =
    match program.(pc) with
    | Edit f -> aux (pc + 1) (f regs)
    | Jump i -> aux (pc + i) regs
    | JumpIf (cond, i) ->
        if cond regs then aux (pc + i) regs else aux (pc + 1) regs
    | exception Invalid_argument _ -> snd regs
  in
  aux 0 regs

let parse list line =
  let instr =
    match String.split_on_char ' ' line with
    | [ inst; v ] -> (
        match inst with
        | "jmp" -> Jump (int_of_string v)
        | _ ->
            let edit =
              match inst with
              | "hlf" -> fun i -> i / 2
              | "tpl" -> fun i -> i * 3
              | "inc" -> fun i -> i + 1
              | _ -> assert false
            in
            let func =
              match v with
              | "a" -> fun (a, b) -> (edit a, b)
              | "b" -> fun (a, b) -> (a, edit b)
              | _ -> assert false
            in
            Edit func)
    | [ inst; v1; v2 ] -> (
        match inst with
        | "jie" ->
            let reg =
              let s = v1.[0] in
              match s with 'a' -> fst | 'b' -> snd | _ -> assert false
            in
            JumpIf ((fun registers -> reg registers mod 2 = 0), int_of_string v2)
        | "jio" ->
            let reg =
              let s = v1.[0] in
              match s with 'a' -> fst | 'b' -> snd | _ -> assert false
            in
            JumpIf ((fun registers -> reg registers = 1), int_of_string v2)
        | _ -> assert false)
    | _ -> assert false
  in
  instr :: list

let parse file = Parse.fold_lines parse [] file |> List.rev |> Array.of_list
let part_1 file = eval (parse file) (0, 0)
let part_2 file = eval (parse file) (1, 0)
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
