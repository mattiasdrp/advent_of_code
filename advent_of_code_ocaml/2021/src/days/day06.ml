open Mdrp_lib

(* Trivial solution that won't scale *)
(* let rec next_day acc = function *)
(*   | [] -> acc *)
(*   | 0 :: tl -> next_day (6 :: 8 :: acc) tl *)
(*   | hd :: tl -> next_day ((hd - 1) :: acc) tl *)

(* let span_days days l = *)
(*   let rec aux i l = if i = days then l else aux (i + 1) (next_day [] l) in *)
(*   aux 0 l *)

let update v map cpt =
  Int.Map.update v
    (function Some cpt' -> Some (cpt' + cpt) | None -> Some cpt)
    map

let next_day map =
  Int.Map.fold
    (fun age cpt map ->
      match age with
      | 7 ->
          update 6 map cpt
          (* There may be some fishes aged 6, don't delete them *)
      | 0 ->
          Int.Map.add 8 cpt (update 6 map cpt)
          (* Add as much 8 aged fish as there were 0 aged fishes *)
          (* Update the number of 6 aged fishes *)
      | age -> Int.Map.add (age - 1) cpt map)
    (* Just add as many fishes aged age-1 *)
    map Int.Map.empty

let span_days days map =
  let rec aux i map = if i = days then map else aux (i + 1) (next_day map) in
  aux 0 map

let nb_fishes map = Int.Map.fold (fun _ cpt sum -> cpt + sum) map 0
let part_1 l = nb_fishes (span_days 80 l) |> Format.printf "%d@."
let part_2 l = nb_fishes (span_days 256 l) |> Format.printf "%d@."

let run () =
  let part = try Sys.argv.(1) with _ -> "2" in
  let file = try Sys.argv.(2) with _ -> "input" in
  let fishes =
    Parse.fold_parse ~sep:','
      (fun map age ->
        let age = int_of_string age in
        Int.Map.update age
          (function Some cpt -> Some (cpt + 1) | None -> Some 1)
          map)
      Int.Map.empty file
  in
  match part with "1" -> part_1 fishes | "2" -> part_2 fishes | _ -> ()

(* From @remyzorg *)
(* Array is the following [| day0; day1; day2; ...; day7; day8 |] *)
(* Each day we need to propagate day7 to day6 (previously day0) and day0 to day8 *)
(* We don't change the values in the array (from day to day-1) but instead virtually *)
(* change the index of the array modulo the size of the array. *)
(* day = 0 -> first index = 0 *)
(* day = 1 -> first index = 1 and so on *)
let next_day array day =
  let day0 = array.(day mod 7) in
  array.(day mod 7) <- day0 + array.((day mod 2) + 7);
  (* day8 receives day0 *)
  array.((day mod 2) + 7) <- day0

let compute array max_days =
  let rec aux day =
    if day = max_days then Array.fold_left ( + ) 0 array |> Format.printf "%d@."
    else (
      next_day array day;
      aux (day + 1))
  in
  aux 0

let run part file =
  let fishes = Array.make 9 0 in
  Parse.fold_parse ~sep:','
    (fun () age ->
      let age = int_of_string age in
      fishes.(age) <- fishes.(age) + 1)
    () file;
  match part with 1 -> compute fishes 80 | 2 -> compute fishes 256 | _ -> ()
