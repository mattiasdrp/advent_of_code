{
  open Day19_parser
  exception Error of string
}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| "\"a\"" { CHAR 'a' }
| "\"b\"" { CHAR 'b' }
| ':'   { COLON }
| '|'   { PIPE }
| '\n'  { EOL }
| eof   { EOL }
| _ as c
    { raise (Error (Printf.sprintf "At offset %d: unexpected character '%c'.\n" (Lexing.lexeme_start lexbuf) c)) }
