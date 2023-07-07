open Mdrp_lib

module FiniteSet (Card : sig
  val card : int
end) =
struct
  type t = {
    set : Int.Set.t;
    prev : int Int.Map.t;
    index_prev : int;
    card : int;
  }

  let empty =
    {
      set = Int.Set.empty;
      prev = Int.Map.of_list (List.init Card.card (fun i -> (i, 0)));
      index_prev = 0;
      card = 0;
    }

  let pp ppf { set; _ } =
    Format.fprintf ppf "%a" List.(pp Int.pp) (Int.Set.elements set)

  let add i ({ set; prev; index_prev; card } as t) =
    if card < Card.card then
      let prev = Int.Map.add index_prev i prev in
      let index_prev = (index_prev + 1) mod Card.card in
      { set = Int.Set.add i set; card = card + 1; prev; index_prev }
    else
      let set =
        Int.Set.add i (Int.Set.remove (Int.Map.find index_prev prev) set)
      in
      let prev = Int.Map.add index_prev i prev in
      let index_prev = (index_prev + 1) mod Card.card in
      { t with set; prev; index_prev }

  let exists f { set; _ } = Int.Set.exists f set
  let mem e { set; _ } = Int.Set.mem e set
end

let get_invalid preamble file =
  let module M = FiniteSet (struct
    let card = preamble
  end) in
  let ci = open_in file in
  let rec aux_preamble i acc =
    if i = preamble then acc
    else aux_preamble (i + 1) (M.add (int_of_string @@ input_line ci) acc)
  in
  let rec aux_parse acc =
    match input_line ci with
    | s ->
        let v = int_of_string s in
        if
          M.exists
            (fun v' ->
              let rest = v - v' in
              rest > 0 && rest <> v' && M.mem rest acc)
            acc
        then aux_parse (M.add v acc)
        else v
    | exception End_of_file -> assert false
  in
  let preamble = aux_preamble 0 M.empty in
  aux_parse preamble

let part_1 preamble file = get_invalid preamble file

exception Found of int * int

let sum v target l =
  let rec aux acc = function
    | (hd, low, high) :: tl ->
        let sum = hd + v in
        if sum = target then raise (Found (min low v, max high v))
        else if sum > target then aux acc tl
        else aux ((sum, min low v, max high v) :: acc) tl
    | [] -> List.rev ((v, v, v) :: acc)
  in
  aux [] l

let part_2 preamble file =
  let invalid = get_invalid preamble file in
  let ci = open_in file in
  let rec aux_parse acc =
    match input_line ci with
    | s -> aux_parse (sum (int_of_string s) invalid acc)
    | exception End_of_file -> assert false
  in
  try aux_parse [] with Found (i, j) -> i + j

let run part file preamble =
  match part with 1 -> part_1 preamble file | _ -> part_2 preamble file
