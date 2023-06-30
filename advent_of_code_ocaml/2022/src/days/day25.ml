open Mdrp_lib

module Snafu = struct
  type t = string

  let digit_to_int = function
    | '2' -> 2
    | '1' -> 1
    | '0' -> 0
    | '-' -> -1
    | '=' -> -2
    | c -> failwith (Printf.sprintf "%c is not a valid digit" c)

  let int_to_digit = function -2 -> "=" | -1 -> "-" | i -> string_of_int i

  let to_int t =
    String.fold_right
      (fun digit (pow5, res) ->
        let res = (digit_to_int digit * pow5) + res in
        (pow5 * 5, res))
      t (1, 0)
    |> snd

  let of_int i =
    let rec aux res i =
      if i = 0 then res
      else
        let ri = (i + 2) mod 5 in
        let qi = (i + 2) / 5 in
        aux (int_to_digit (ri - 2) ^ res) qi
    in
    aux "" i
end

let part_1 file =
  let res =
    Parse.fold_lines
      (fun res s ->
        let i = Snafu.to_int s in
        i + res)
      0 file
  in
  Format.eprintf "%s@." (Snafu.of_int res);
  res

let part_2 _file = failwith "TODO"
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
