%{
    open Day16_ast
%}

%token <int> INT
%token <string> IDENT
%token SUE "Sue"
%token COMMA ","
%token COLON ":"
%token EOL

%start <Sue.t> main

%%

main:
  | "Sue" id=INT ":" fields=separated_list(",", field) EOL { Sue.of_list id fields }

field:
  | id = IDENT v = INT { id, v }
