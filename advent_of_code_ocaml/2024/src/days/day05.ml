open Mdrp_lib

(* let transitive_closure map = *)
(*   let rec aux map = *)
(*     let nmap, edited = *)
(*       Int.Map.fold *)
(*         (fun key set (acc, edited) -> *)
(*           Format.eprintf "Closing %d: %a@." key Int.Set.(pp ()) set; *)
(*           let nset, edited = *)
(*             Int.Set.fold *)
(*               (fun key' (set, edited) -> *)
(*                 match Int.Map.find key' map with *)
(*                 | set' -> *)
(*                     Format.eprintf "Found %d: %a@." key' Int.Set.(pp ()) set'; *)
(*                     let nset = Int.Set.union set set' in *)
(*                     ( nset, *)
(*                       edited || Int.Set.cardinal nset > Int.Set.cardinal set ) *)
(*                 | exception _ -> (set, edited)) *)
(*               set (set, edited) *)
(*           in *)
(*           Format.eprintf "Closed %d: %a@." key Int.Set.(pp ()) nset; *)
(*           (Int.Map.add key nset acc, edited)) *)
(*         map (Int.Map.empty, false) *)
(*     in *)
(*     if edited then aux nmap else nmap *)
(*   in *)
(*   aux map *)

let common_part file =
  let input = open_in file in
  let rec parse_constraints map =
    match String.split_on_char '|' (input_line input) with
    | [ left; right ] ->
        let right = int_of_string right in
        Int.Map.update (int_of_string left)
          (function
            | Some set -> Some (Int.Set.add right set)
            | None -> Some (Int.Set.singleton right))
          map
        |> parse_constraints
    | _ -> map
  in
  let map = parse_constraints Int.Map.empty in
  let rec check_list = function
    | hd1 :: hd2 :: tl ->
        (not
           (Int.Set.mem hd1
              (Int.Map.find_opt hd2 map |> Option.value ~default:Int.Set.empty)))
        && check_list (hd2 :: tl)
    | [ _ ] | [] -> true
  in
  let rec check_upgrade (right, wrong) =
    match
      input_line input |> String.split_on_char ',' |> List.map int_of_string
    with
    | list ->
        let acc =
          if check_list list then (list :: right, wrong)
          else (right, list :: wrong)
        in
        check_upgrade acc
    | exception _ ->
        close_in input;
        (right, wrong, map)
  in
  check_upgrade ([], [])

let part_1 file =
  let right, _wrong, _ = common_part file in
  List.fold_left
    (fun acc list -> acc + List.nth list (List.length list / 2))
    0 right

let order map e1 e2 =
  if
    Int.Set.mem e1
      (Int.Map.find_opt e2 map |> Option.value ~default:Int.Set.empty)
  then 1
  else if
    Int.Set.mem e2
      (Int.Map.find_opt e1 map |> Option.value ~default:Int.Set.empty)
  then -1
  else 0

let part_2 file =
  let _right, wrong, map = common_part file in
  List.fold_left
    (fun acc list ->
      let repaired = List.fast_sort (order map) list in
      acc + List.nth repaired (List.length repaired / 2))
    0 wrong

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
