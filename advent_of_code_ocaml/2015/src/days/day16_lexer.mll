{
  open Day16_parser
  exception Error of string
}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| ":"     { COLON }
| ","     { COMMA }
| "Sue"         { SUE }
| ['a'-'z'':']+ as s { IDENT (String.sub s 0 (String.length s - 1)) }
| '\n'  { EOL }
| eof   { EOL }
| _ as c
    { raise (Error (Printf.sprintf "At offset %d: unexpected character '%c'.\n" (Lexing.lexeme_start lexbuf) c)) }
