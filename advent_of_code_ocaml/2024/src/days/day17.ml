open Mdrp_lib

type registers = { a : int; b : int; c : int }

let pp_reg ppf { a; b; c } = Format.fprintf ppf "{a: %d; b: %d; c: %d}" a b c

type program = {
  program : int Array.t;
  registers : registers;
  inst_pointer : int;
  outputs : int list;
}

let pp ppf { program; registers; inst_pointer; outputs } =
  Format.fprintf ppf "{prog: %a; reg: %a; ptr: %d; outputs: %a}"
    Array.(pp Int.pp)
    program pp_reg registers inst_pointer
    List.(pp Int.pp)
    (List.rev outputs)

module Operand = struct
  type t = int

  let of_int ?registers i =
    match registers with
    | None -> i
    | Some registers -> (
        match i with
        | 4 -> registers.a
        | 5 -> registers.b
        | 6 -> registers.c
        | 7 -> raise Exit
        | i when 0 <= i && i <= 3 -> i
        | _ -> assert false)

  let debug ?registers i =
    match registers with
    | None -> string_of_int i
    | Some _ -> (
        match i with
        | 4 -> "a"
        | 5 -> "b"
        | 6 -> "c"
        | 7 -> raise Exit
        | i when 0 <= i && i <= 3 -> string_of_int i
        | _ -> assert false)
end

module Instr = struct
  type t =
    | ADivision0_cmb
    | Xor1_lit
    | Bst2_cmb
    | Jnz3_lit
    | Bxc4_ign
    | Out5_cmb
    | BDivision6_cmb
    | CDivision7_cmb

  let pp operand registers ppf = function
    | ADivision0_cmb ->
        Format.fprintf ppf "a <- a / 2^%s" (Operand.debug ~registers operand)
    | Xor1_lit -> Format.fprintf ppf "b <- b xor %s" (Operand.debug operand)
    | Bst2_cmb ->
        Format.fprintf ppf "b <- %s mod 8" (Operand.debug ~registers operand)
    | Jnz3_lit -> Format.fprintf ppf "jump if a != 0"
    | Bxc4_ign -> Format.fprintf ppf "b <- b xor c"
    | Out5_cmb ->
        Format.fprintf ppf "output %s" (Operand.debug ~registers operand)
    | BDivision6_cmb ->
        Format.fprintf ppf "b <- a / 2^%s" (Operand.debug ~registers operand)
    | CDivision7_cmb ->
        Format.fprintf ppf "c <- a / 2^%s" (Operand.debug ~registers operand)

  let next_inst program registers =
    { program with registers; inst_pointer = program.inst_pointer + 2 }

  let of_int = function
    | 0 -> ADivision0_cmb
    | 1 -> Xor1_lit
    | 2 -> Bst2_cmb
    | 3 -> Jnz3_lit
    | 4 -> Bxc4_ign
    | 5 -> Out5_cmb
    | 6 -> BDivision6_cmb
    | 7 -> CDivision7_cmb
    | _ -> assert false

  let eval program t operand =
    let registers = program.registers in
    Format.eprintf "%a@." (pp operand registers) t;
    let util_div () = 2. ** (float @@ Operand.of_int ~registers operand) in
    match t with
    | ADivision0_cmb ->
        next_inst program
          {
            registers with
            a = float registers.a /. util_div () |> int_of_float;
          }
    | Xor1_lit ->
        next_inst program
          { registers with b = registers.b lxor Operand.of_int operand }
    | Bst2_cmb ->
        next_inst program
          { registers with b = Operand.of_int ~registers operand mod 8 }
    | Jnz3_lit ->
        if registers.a = 0 then
          { program with inst_pointer = program.inst_pointer + 2 }
        else { program with inst_pointer = Operand.of_int operand }
    | Bxc4_ign ->
        next_inst program { registers with b = registers.b lxor registers.c }
    | Out5_cmb ->
        {
          program with
          outputs = (Operand.of_int ~registers operand mod 8) :: program.outputs;
          inst_pointer = program.inst_pointer + 2;
        }
    | BDivision6_cmb ->
        next_inst program
          {
            registers with
            b = float registers.a /. util_div () |> int_of_float;
          }
    | CDivision7_cmb ->
        next_inst program
          {
            registers with
            c = float registers.a /. util_div () |> int_of_float;
          }
end

let rec eval program =
  match program.program.(program.inst_pointer) with
  | instruction ->
      let program =
        Instr.eval program (Instr.of_int instruction)
          program.program.(program.inst_pointer + 1)
      in
      eval program
  | exception _ ->
      Format.printf "%a@."
        Format.(
          pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
            pp_print_int)
        (List.rev program.outputs)

let part_1 file =
  let ci = open_in file in
  let a =
    input_line ci
    |> String.split_on_char_non_empty ':'
    |> List.tl |> List.hd |> String.trim |> int_of_string
  in

  let b =
    input_line ci
    |> String.split_on_char_non_empty ':'
    |> List.tl |> List.hd |> String.trim |> int_of_string
  in

  let c =
    input_line ci
    |> String.split_on_char_non_empty ':'
    |> List.tl |> List.hd |> String.trim |> int_of_string
  in

  ignore (input_line ci);
  let program =
    input_line ci
    |> String.split_on_char_non_empty ':'
    |> List.tl |> List.hd
    |> String.split_on_char_non_empty ','
    |> List.map String.trim |> List.map int_of_string |> Array.of_list
  in
  let program =
    { program; registers = { a; b; c }; inst_pointer = 0; outputs = [] }
  in
  eval program;
  0

let part_2 _file = failwith "TODO"
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
