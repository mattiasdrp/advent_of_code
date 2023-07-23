%{ open Day19_ast %}


%token <string> WORD
%token COMMA ","
%token LEFTP  "("
%token RIGHTP  ")"
%token CONTAINS  "contains"
%token EOL

%start <string list * string list> main

%%

main:
  | e = expr EOL { e }

expr:
  | ingredients = nonempty_list(WORD)  "(" "contains" allergens = separated_nonempty_list(",", WORD) ")"
    { ingredients, allergens }
