open Mdrp_lib

module Snafu
    (*  : sig *)
    (*   type t = string *)

    (*   val digit_to_int : char -> int *)
    (*   val int_to_digit : int -> string *)
    (*   val to_int : t -> int *)
    (*   val of_int : int -> t *)
    (*   val of_intb : int -> t *)
    (* end *) =
struct
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

  (* let of_int i = if i = 0 then "0" else Base5.minus_int (2 * i) i *)
end

(* and Base5 : sig *)
(*   type t = string *)

(*   val of_int : int -> t *)
(*   val minus : t -> t -> Snafu.t *)
(*   val minus_int : int -> int -> Snafu.t *)
(* end = struct *)
(*   type t = string *)

(*   let of_int i = *)
(*     let rec aux res i = *)
(*       if i = 0 then String.rev res *)
(*       else *)
(*         let ri = i mod 5 in *)
(*         let qi = i / 5 in *)
(*         aux (string_of_int ri ^ res) qi *)
(*     in *)
(*     aux "" i *)

(*   let minus t1 t2 = *)
(*     let rec aux res t1 t2 = *)
(*       if t2 = "" then if t1 = "" then res else String.rev t1 ^ res *)
(*       else *)
(*         let r1 = t1.[0] in *)
(*         let r2 = t2.[0] in *)
(*         aux *)
(*           (Snafu.int_to_digit (Char.to_digit r1 - Char.to_digit r2) ^ res) *)
(*           (String.sub t1 1 (String.length t1 - 1)) *)
(*           (String.sub t2 1 (String.length t2 - 1)) *)
(*     in *)
(*     aux "" t1 t2 *)

(*   let minus_int i1 i2 = minus (of_int i1) (of_int i2) *)
(* end *)

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
