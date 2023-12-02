open Mdrp_lib

let parse line =
  let linebuf = Lexing.from_string line in
  Day02_parser.main Day02_lexer.token linebuf

let part_1 file =
  let check i = function
    | Day02_ast.Red -> i <= 12
    | Green -> i <= 13
    | Blue -> i <= 14
  in

  Parse.fold_lines
    (fun acc line ->
      let id, games = parse line in
      if
        List.for_all
          (fun game -> List.for_all (fun (c, i) -> check i c) game)
          games
      then acc + id
      else acc)
    0 file

let update (r, g, b) c i =
  match c with
  | Day02_ast.Red -> (max r i, g, b)
  | Green -> (r, max g i, b)
  | Blue -> (r, g, max b i)

let part_2 file =
  let update (r, g, b) c i =
    match c with
    | Day02_ast.Red -> (max r i, g, b)
    | Green -> (r, max g i, b)
    | Blue -> (r, g, max b i)
  in
  Parse.fold_lines
    (fun acc line ->
      let _, games = parse line in
      let r, g, b =
        List.fold_left
          (fun acc game ->
            List.fold_left (fun acc (c, i) -> update acc c i) acc game)
          (0, 0, 0) games
      in
      (r * g * b) + acc)
    0 file

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
