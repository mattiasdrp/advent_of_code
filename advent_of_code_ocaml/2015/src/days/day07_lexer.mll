{
  open Day07_parser
  exception Error of string
}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| "->"     { ARROW }
| "AND"    { BINOP And }
| "OR"     { BINOP Or }
| "LSHIFT" { BINOP Lshift }
| "RSHIFT" { BINOP Rshift }
| "NOT"    { NOT }
| ['a'-'z']+ as s { IDENT s }
| '\n'  { EOL }
| eof   { EOL }
| _ as c
    { raise (Error (Printf.sprintf "At offset %d: unexpected character '%c'.\n" (Lexing.lexeme_start lexbuf) c)) }
