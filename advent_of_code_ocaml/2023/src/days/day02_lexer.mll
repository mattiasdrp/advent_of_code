{
  open Day02_parser
  open Day02_ast
  exception Error of string
}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| ":"     { COLON }
| ";"     { SEMICOLON }
| ","     { COMMA }
| "blue"  { COLOUR Blue }
| "green" { COLOUR Green }
| "red"   { COLOUR Red }
| "Game"  { GAME }
| '\n'    { EOL }
| eof     { EOL }
| _ as c
    { raise (Error (Printf.sprintf "At offset %d: unexpected character '%c'.\n" (Lexing.lexeme_start lexbuf) c)) }
