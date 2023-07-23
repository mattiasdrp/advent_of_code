{
  open Day21_parser
  exception Error of string
}

rule token = parse
| [' ' '\t']
{ token lexbuf }
| "contains" { CONTAINS }
| ['a'-'z']+ as word { WORD word }
| ','   { COMMA }
| '('   { LEFTP }
| ')'   { RIGHTP }
| '\n'  { EOL }
| eof   { EOL }
| _ as c
    { raise (Error (Printf.sprintf "At offset %d: unexpected character '%c'.\n" (Lexing.lexeme_start lexbuf) c)) }
