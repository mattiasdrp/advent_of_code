%token <int> INT
%token SEMICOLON ";"
%token COMMA ","
%token COLON ":"
%token GAME
%token <Day02_ast.colour> COLOUR
%token EOL

%start <int * (Day02_ast.colour * int) list list> main

%%

main:
  | GAME id = INT ":" g = games EOL { id, g }

games:
  | subgames = separated_nonempty_list(";", subgame) { subgames }

subgame:
  | cubes = separated_nonempty_list(",", cube) { cubes }

cube:
  | nb = INT c = COLOUR { (c, nb) }
