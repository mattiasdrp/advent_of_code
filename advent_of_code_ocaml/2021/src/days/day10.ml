open Mdrp_lib

exception Wrong_Closing of char
exception Too_Much_Closing of char

let complement = function
  | '(' -> ')'
  | '[' -> ']'
  | '{' -> '}'
  | '<' -> '>'
  | _ -> assert false

let score = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _ -> assert false

let part_1 file =
  Parse.fold_lines
    (fun corrupted s ->
      try
        String.fold
          (* This is just Dyck word checking, fill a list with opening symbols and *)
          (* delete them when encountering the proper closing symbol or raise an error *)
            (fun dyck c ->
            match c with
            | ')' | ']' | '}' | '>' -> (
                match dyck with
                | [] -> raise (Too_Much_Closing c)
                | hd :: tl ->
                    (* We could check with the int value of c since the proper closing *)
                    (* is +1 or +2 but let's not be dirty *)
                    if c = complement hd then tl else raise (Wrong_Closing c))
            | c -> c :: dyck)
          [] s
        |> fun _ -> corrupted
      with Wrong_Closing c -> corrupted + score c)
    0 file
  |> Format.eprintf "%d@."

let score = function
  | '(' -> 1
  | '[' -> 2
  | '{' -> 3
  | '<' -> 4
  | _ -> assert false

let complete l = List.fold_left (fun acc c -> (acc * 5) + score c) 0 l

(* There's no optimal way to get the middle element of a list *)
let get_middle l =
  let a = Array.of_list l in
  a.(Array.length a / 2)

let part_2 file =
  Parse.fold_lines
    (fun acc s ->
      try
        String.fold
          (fun dyck c ->
            match c with
            | ')' | ']' | '}' | '>' -> (
                match dyck with
                | [] -> raise (Too_Much_Closing c)
                | hd :: tl ->
                    if c = complement hd then tl else raise (Wrong_Closing c))
            | c -> c :: dyck)
          [] s
        |> complete
        |> fun c -> c :: acc
      with Wrong_Closing _ -> acc)
    [] file
  |> List.fast_sort Int.compare |> get_middle |> Format.printf "%d@."

let run part file =
  match part with 1 -> part_1 file | 2 -> part_2 file | _ -> ()
