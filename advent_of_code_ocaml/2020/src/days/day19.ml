open Mdrp_lib

(*
0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"
 *)

type rules = {
  a : int;
  b : int;
  rev_map : Int.Set.t Int.Map.t;
  rules : int list list Int.Map.t;
}

let parse line rules =
  if line = "" then None
  else
    let linebuf = Lexing.from_string line in
    let id, pattern = Day19_parser.main Day19_lexer.token linebuf in
    match pattern with
    | Day19_ast.Char 'a' -> Some { rules with a = id }
    | Char 'b' -> Some { rules with b = id }
    | Char _ -> assert false
    | Pattern ill ->
        let rev_map =
          List.fold_left
            (fun acc il ->
              List.fold_left
                (fun acc i ->
                  Int.Map.update i
                    (function
                      | None -> Some (Int.Set.singleton id)
                      | Some set -> Some (Int.Set.add id set))
                    acc)
                acc il)
            rules.rev_map ill
        in
        Some { rules with rev_map; rules = Int.Map.add id ill rules.rules }

type Node = Char of char | Id of int

let build_tree rules =
  let rec aux terminals =
    List.fold_left (fun acc terminal ->
        let ids = Int.Map.find terminal rules.rev_map in
        Int.Set.fold (fun id acc ->


let part_1 file =
  let ci = open_in file in
  let rec aux_parse rules =
    match input_line ci with
    | s -> (
        match parse s rules with None -> rules | Some rules -> aux_parse rules)
  in
  let rules =
    aux_parse { a = -1; b = -1; rev_map = Int.Map.empty; rules = Int.Map.empty }
  in
  Format.eprintf "a: %d, b:%d@.@[<v 0>%a@.@[<v 0>%a@." rules.a rules.b
    Int.Map.(pp Int.Set.pp)
    rules.rev_map
    Int.Map.(pp List.(pp List.(pp Int.pp)))
    rules.rules;
  0

let part_2 _file = failwith "TODO"
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
