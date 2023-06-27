open Mdrp_lib

module Snafu = struct
  type t = string

  let digit_to_int = function
    | '2' -> 2
    | '1' -> 1
    | '0' -> 0
    | '-' -> -1
    | '=' -> -2

  let to_int t =
    String.fold_right
      (fun digit (pow5, res) ->
        let res = (digit_to_int digit * pow5) + res in
        (pow5 * 5, res))
      t (1, 0)
    |> snd
end

let part_1 file =
  Parse.fold_lines
    (fun res s ->
      let i = Snafu.to_int s in
      Format.eprintf "%d@." i;
      i + res)
    0 file

let part_2 _file = failwith "TODO"
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
