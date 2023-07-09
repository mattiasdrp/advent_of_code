open Mdrp_lib

type res = Char of char | Pattern of int list list

let pp ppf = function
  | Char c -> Format.fprintf ppf "%c" c
  | Pattern ill -> Format.fprintf ppf "%a" List.(pp List.(pp Int.pp)) ill
