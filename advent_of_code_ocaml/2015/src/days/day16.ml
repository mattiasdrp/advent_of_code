open Mdrp_lib
module SSet = Set.Make (Day16_ast.Sue)

let gift_sue =
  Day16_ast.Sue.
    {
      id = 0;
      children = Some 3;
      cats = Some 7;
      samoyeds = Some 2;
      pomeranians = Some 3;
      akitas = Some 0;
      vizslas = Some 0;
      goldfish = Some 5;
      trees = Some 3;
      cars = Some 2;
      perfumes = Some 1;
    }

let parse line =
  let linebuf = Lexing.from_string line in
  Day16_parser.main Day16_lexer.token linebuf

let part_1 file =
  let ci = open_in file in

  let rec aux_parse () =
    match input_line ci with
    | s ->
        let sue = parse s in
        if Day16_ast.Sue.equal gift_sue sue then (
          close_in ci;
          sue.id)
        else aux_parse ()
    | exception End_of_file ->
        close_in ci;
        assert false
  in
  aux_parse ()

let part_2 file =
  let ci = open_in file in

  let rec aux_parse () =
    match input_line ci with
    | s ->
        let sue = parse s in
        if Day16_ast.Sue.equal_v2 gift_sue sue then (
          close_in ci;
          sue)
        else aux_parse ()
    | exception End_of_file ->
        close_in ci;
        assert false
  in
  let sue = aux_parse () in
  sue.id

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
