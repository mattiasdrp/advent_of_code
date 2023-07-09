%{ open Day19_ast %}


%token <int> INT
%token <char> CHAR
%token COLON ":"
%token PIPE  "|"
%token EOL

%start <int * Day19_ast.res> main

%%

main:
  | e = expr EOL { e }

expr:
  | i = INT ":" pl = separated_nonempty_list("|", pattern)
    { i, Pattern pl }
  | i = INT ":" c = CHAR
    { i, Char c }

pattern:
  | il = nonempty_list(INT) { il }
