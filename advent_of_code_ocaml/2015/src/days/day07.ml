open Mdrp_lib

let parse map line =
  let linebuf = Lexing.from_string line in
  let ident, expr = Day07_parser.main Day07_lexer.token linebuf in
  String.Map.add ident expr map

let rec eval e (map : 'a String.Map.t) =
  let open Day07_ast in
  match e with
  | Ident id -> (
      match String.Map.find id map with
      | Value v -> (v, map)
      | e ->
          let v, map = eval e map in
          (v, String.Map.add id (Value v) map))
  | Value v -> (v, map)
  | Binop (e1, bop, e2) ->
      let v1, map = eval e1 map in
      let v2, map = eval e2 map in
      let v =
        match bop with
        | And -> v1 land v2
        | Or -> v1 lor v2
        | Lshift -> v1 lsl v2
        | Rshift -> v1 lsr v2
      in
      (v, map)
  | Not e ->
      let v, map = eval e map in
      (lnot v, map)

let part_1 file =
  let map = Parse.fold_lines parse String.Map.empty file in
  eval (Ident "a") map |> fst

let part_2 file =
  let open Day07_ast in
  let map = Parse.fold_lines parse String.Map.empty file in
  let a, _ = eval (Ident "a") map in
  eval (Ident "a") (String.Map.add "b" (Value a) map) |> fst

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
