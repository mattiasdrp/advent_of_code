open Mdrp_lib

module StringToParse = struct
  type token = char
  type t = string

  let get t i = try Some t.[i] with Invalid_argument _ -> None
  let eq_token tok1 tok2 = Char.equal tok1 tok2
  let compare_token tok1 tok2 = Char.compare tok1 tok2
  let equal_token tok1 tok2 = Char.equal tok1 tok2
  let pp_token ppf c = Format.fprintf ppf "%c" c
end

module Parser = Earley.MakeParser (StringToParse)

type rules = Parser.Symbol.t array list String.Map.t

let make_id id = Format.sprintf "id_%d" id
let make_nonterminal id = Parser.Symbol.NonTerminal (make_id id)

let parse part line rules =
  if line = "" then None
  else
    let linebuf = Lexing.from_string line in
    let id, pattern = Day19_parser.main Day19_lexer.token linebuf in
    let pattern =
      if part && id = 8 then Day19_ast.Pattern [ [ 42 ]; [ 42; 8 ] ]
      else if part && id = 11 then
        Day19_ast.Pattern [ [ 42; 31 ]; [ 42; 11; 31 ] ]
      else pattern
    in
    let id = make_id id in
    match pattern with
    | Day19_ast.Char 'a' ->
        Some (Parser.Rules.add id [ [| Parser.Symbol.Terminal 'a' |] ] rules)
    | Char 'b' ->
        Some (Parser.Rules.add id [ [| Parser.Symbol.Terminal 'b' |] ] rules)
    | Char _ -> assert false
    | Pattern ill ->
        let idll =
          List.map (fun il -> List.map make_nonterminal il |> Array.of_list) ill
        in
        Some (Parser.Rules.add id idll rules)

let run part file =
  let ci = open_in file in
  let rec aux_parse rules =
    let s = input_line ci in
    match parse part s rules with
    | None -> rules
    | Some rules -> aux_parse rules
  in
  let rules = aux_parse Parser.Rules.empty in
  let axiom = make_id 0 in
  let rec aux_check res =
    match input_line ci with
    | s ->
        let res = if Parser.run rules axiom s then res + 1 else res in
        aux_check res
    | exception End_of_file ->
        close_in ci;
        res
  in
  aux_check 0

let part_1 file = run false file
let part_2 file = run true file
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
