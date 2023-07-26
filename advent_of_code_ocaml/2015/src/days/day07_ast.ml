type binop = And | Or | Lshift | Rshift

type expr =
  | Ident of string
  | Value of int
  | Binop of expr * binop * expr
  | Not of expr

let rec pp ppf = function
  | Ident s -> Format.fprintf ppf "%s" s
  | Value v -> Format.fprintf ppf "%d" v
  | Binop (e1, _, e2) -> Format.fprintf ppf "%a bop %a" pp e1 pp e2
  | Not e -> Format.fprintf ppf "not %a" pp e

type assign = { ident : string; expr : expr }
