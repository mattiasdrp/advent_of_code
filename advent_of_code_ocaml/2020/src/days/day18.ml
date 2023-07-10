open Mdrp_lib

let part_1 _file = failwith "TODO"

let rec transfer_until_closing l1 l2 =
  match l1 with
  | [] -> assert false
  | '(' :: l1 -> (l1, l2)
  | op :: l1 -> transfer_until_closing l1 (op :: l2)

let precedence = function '(' -> 3 | '*' -> 1 | '+' -> 2 | _ -> assert false

let rec transfer_and_stack_until_lower op1 l1 l2 =
  match l1 with
  | [] -> ([ op1 ], l2)
  | '(' :: _ -> (op1 :: l1, l2)
  | op2 :: tl1 ->
      if precedence op2 >= precedence op1 then
        transfer_and_stack_until_lower op1 tl1 (op2 :: l2)
      else (op1 :: l1, l2)

let infix_to_postfix s =
  let stack, newl =
    String.fold_left
      (fun (stack, newl) c ->
        match c with
        | ' ' -> (stack, newl)
        | '(' -> ('(' :: stack, newl)
        | ')' -> transfer_until_closing stack newl
        | ('*' | '+') as op1 -> (
            match stack with
            | [] | '(' :: _ -> (op1 :: stack, newl)
            | op2 :: _ ->
                if precedence op1 > precedence op2 then (op1 :: stack, newl)
                else transfer_and_stack_until_lower op1 stack newl)
        | c -> (stack, c :: newl))
      ([], []) s
  in
  List.rev_append stack newl |> List.rev

let op_to_op = function '+' -> ( + ) | '*' -> ( * ) | _ -> assert false

let eval_postfix cl =
  List.fold_left
    (fun stack c ->
      match c with
      | '+' | '*' -> (
          match stack with
          | e1 :: e2 :: stack -> (op_to_op c) e1 e2 :: stack
          | _ -> assert false)
      | _ -> Char.to_digit c :: stack)
    [] cl
  |> List.hd

let part_2 file =
  Parse.fold_lines
    (fun acc s ->
      let res = infix_to_postfix s |> eval_postfix in
      res + acc)
    0 file

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
