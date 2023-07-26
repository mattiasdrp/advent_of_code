%{
    open Day07_ast
%}

%token <int> INT
%token <string> IDENT
%token ARROW "->"
%token <Day07_ast.binop> BINOP
%token NOT
%token EOL

%start <string * Day07_ast.expr> main

%%

main:
  | e = expr "->" ident=IDENT EOL { ident, e }

base_expr:
  | i = INT { Value i }
  | id = IDENT { Ident id }

expr:
  | e = base_expr { e }
  | e1 = base_expr bop = BINOP e2 = base_expr { Binop (e1, bop, e2) }
  | NOT e = expr { Not e }
