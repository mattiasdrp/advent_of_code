{
  open Day19_sol_parser
  exception Error of string
}

rule token = parse
| [' ' '\t'] { token lexbuf }
| 'a'        { A }
| 'b'        { B }
| '\n'       { EOL }
| eof        { EOL }
| _ as c
    { raise (Error (Printf.sprintf "At offset %d: unexpected character '%c'.\n" (Lexing.lexeme_start lexbuf) c)) }
