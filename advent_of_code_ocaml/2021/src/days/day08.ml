open Mdrp_lib

let part_1 file =
  let ci = open_in file in

  let rec aux_parse acc =
    match input_line ci with
    | s -> (
        match String.split_on_char '|' s with
        | [ _; out ] ->
            List.fold_left
              (fun acc s ->
                let sl = String.length s in
                if sl = 2 || sl = 3 || sl = 4 || sl = 7 then acc + 1 else acc)
              acc
              (String.split_on_char_non_empty ' ' out)
            |> aux_parse
        | _ -> assert false)
    | exception End_of_file -> acc
  in

  aux_parse 0

module Digits = struct
  include Set.Make (Char)

  let of_string s = String.fold_left (fun t c -> add c t) empty s
  let pp ppf d = Format.fprintf ppf "%a" List.(pp Char.pp) (elements d)
end

module IMap = Int.Map
module CMap = Map.Make (Char)

let part_2 file =
  let ci = open_in file in

  let rec aux_parse acc =
    match input_line ci with
    | s -> (
        match String.split_on_char '|' s with
        | [ all; out ] ->
            let open IMap in
            let map =
              List.fold_left
                (fun map s ->
                  let sl = String.length s in
                  if sl = 2 then add 1 (Digits.of_string s) map
                  else if sl = 4 then add 4 (Digits.of_string s) map
                  else map)
                empty
                (String.split_on_char_non_empty ' ' all)
            in
            let one = find 1 map in
            let four = find 4 map in
            (* 0, 6 and 9 *)
            let discriminate_6digits d =
              (* 6 is the only digit that has only one segment in common with 1 *)
              if Digits.(cardinal @@ inter one d) = 1 then "6"
                (* 9 contains 4 whereas 0 lacks one 4 segment *)
              else if Digits.(cardinal @@ inter four d) = 4 then "9"
              else "0"
            in
            (* 2, 3 and 5 *)
            let discriminate_5digits d =
              (* 3 is the only digit that contains 1 *)
              if Digits.(cardinal @@ diff one d) = 0 then "3"
                (* 5 contains 3 segments of 4 whereas 2 contains 2 *)
              else if Digits.(cardinal @@ diff four d) = 1 then "5"
              else "2"
            in
            String.split_on_char_non_empty ' ' out
            |> List.fold_left
                 (fun acc s ->
                   let sl = String.length s in
                   acc
                   ^
                   if sl = 2 then "1"
                   else if sl = 3 then "7"
                   else if sl = 4 then "4"
                   else if sl = 7 then "8"
                   else if sl = 5 then discriminate_5digits (Digits.of_string s)
                   else discriminate_6digits (Digits.of_string s))
                 ""
            |> fun str -> aux_parse (acc + int_of_string str)
        | _ -> assert false)
    | exception End_of_file -> acc
  in
  aux_parse 0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file

(* module Digits = struct *)
(*   module M = Map.Make (Char) *)

(*   let of_string s = String.fold_left (fun t c -> M.add c 1 t) M.empty s *)

(*   let pp ppf d = *)
(*     Format.fprintf ppf "%a" List.(pp (Pair.pp Char.pp Int.pp)) (M.bindings d) *)

(*   let union b1 b2 = *)
(*     M.merge *)
(*       (fun _ o1 o2 -> *)
(*         match (o1, o2) with *)
(*           | None, None -> None *)
(*           | None, Some m | Some m, None -> Some m *)
(*           | Some m1, Some m2 -> Some (m1 + m2)) *)
(*       b1 b2 *)

(*   let filter = M.filter *)

(*   let diff b1 b2 = *)
(*     M.merge *)
(*       (fun _ o1 o2 -> *)
(*         match (o1, o2) with *)
(*           | None, _ -> None *)
(*           | Some m, None -> Some m *)
(*           | Some _, Some _ -> None) *)
(*       b1 b2 *)

(*   let multi_diff b lb = List.fold_left (fun acc b -> diff acc b) b lb *)

(*   let inter b1 b2 = *)
(*     M.merge *)
(*       (fun _ o1 o2 -> *)
(*         match (o1, o2) with *)
(*           | None, None | None, Some _ | Some _, None -> None *)
(*           | Some m1, Some m2 -> Some (max m1 m2)) *)
(*       b1 b2 *)

(*   let min_elt = M.min_binding *)

(*   let cardinal b = M.fold (fun _ _ c -> c + 1) b 0 *)

(*   let singleton x = M.add x 1 M.empty *)

(*   let pp_sing ppf t = *)
(*     assert (cardinal t = 1); *)
(*     min_elt t |> fst |> Format.fprintf ppf "%c" *)
(* end *)

(* let one = find 1 map in *)
(* let two = find 2 map in *)
(* let four = find 4 map in *)
(* let seven = find 7 map in *)
(* (\* A is the segment that is in 7 and not 1 *\) *)
(* let a = Digits.diff seven one in *)
(* (\* B or E are the segments that appear only once in 2, 3 and 5 *\) *)
(* let bore = two |> Digits.filter (fun _ cpt -> cpt = 1) in *)
(* (\* B is the BorE segment that appears in 4 *\) *)
(* let b = Digits.inter bore four in *)
(* (\* E is BorE - B *\) *)
(* let e = Digits.diff bore b in *)
(* (\* D is 4 - B - 1 *\) *)
(* let d = Digits.multi_diff four [ b; one ] in *)
(* (\* G is the segment that appears thrice in {2; 3; 5} - A - D  *\) *)
(* let g = *)
(*   Digits.multi_diff two [ a; d ] *)
(*   |> Digits.filter (fun _ cpt -> cpt = 3) *)
(* in *)
(* (\* CorF are the two segments that are in {0; 6; 9} /\ 1 *\) *)
(* let corf = Digits.inter (find 0 map) one in *)
(* (\* C appears only twice in CorF *\) *)
(* let c = Digits.filter (fun _ cpt -> cpt = 2) corf in *)
(* (\* F appears thrice in CorF *\) *)
(* let f = Digits.filter (fun _ cpt -> cpt = 3) corf in *)
(* let map = *)
(*   CMap.of_list *)
(*     [ *)
(*       ('a', a); *)
(*       ('b', b); *)
(*       ('c', c); *)
(*       ('d', d); *)
(*       ('e', e); *)
(*       ('f', f); *)
(*       ('g', g); *)
(*     ] *)
(* in *)
