open Mdrp_lib

let parse reverse (molecule, regexps) line =
  let re = Str.regexp {|\([a-zA-Z]+\) => \([a-zA-Z]+\)|} in
  if Str.string_match re line 0 then
    let source =
      if reverse then Str.matched_group 2 line else Str.matched_group 1 line
    in
    let target =
      if reverse then Str.matched_group 1 line else Str.matched_group 2 line
    in
    ( molecule,
      String.Map.update source
        (function
          | Some s -> Some (String.Set.add target s)
          | None -> Some (String.Set.singleton target))
        regexps )
  else if line = "" then (molecule, regexps)
  else (line, regexps)

let replace source target line new_lines =
  let re = Str.regexp source in
  let rec aux new_lines pos =
    match Str.replace_forward re ~string:line ~templ:target pos with
    | line, pos -> aux (String.Set.add line new_lines) (pos + 1)
    | exception Not_found -> new_lines
  in
  aux new_lines 0

let replacements regexps line =
  String.Map.fold
    (fun source targets new_lines ->
      String.Set.fold
        (fun target new_lines -> replace source target line new_lines)
        targets new_lines)
    regexps String.Set.empty

module PrioQueue = struct
  type priority = int
  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

  let empty = Empty

  let rec insert queue prio elt =
    match queue with
    | Empty -> Node (prio, elt, Empty, Empty)
    | Node (p, e, left, right) ->
        if prio <= p then Node (prio, elt, insert right p e, left)
        else Node (p, e, insert right prio elt, left)

  exception Queue_is_empty

  let rec remove_top = function
    | Empty -> raise Queue_is_empty
    | Node (_prio, _elt, left, Empty) -> left
    | Node (_prio, _elt, Empty, right) -> right
    | Node
        ( _prio,
          _elt,
          (Node (lprio, lelt, _, _) as left),
          (Node (rprio, relt, _, _) as right) ) ->
        if lprio <= rprio then Node (lprio, lelt, remove_top left, right)
        else Node (rprio, relt, left, remove_top right)

  let extract = function
    | Empty -> raise Queue_is_empty
    | Node (prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
end

let steps_to_reach regexps molecule =
  let exception Res of int in
  let seen = Hashtbl.create 64 in
  let rec aux prioqueue =
    let _, (line, steps), prioqueue = PrioQueue.extract prioqueue in
    Hashtbl.add seen line ();
    Format.eprintf "%s, %d@." line steps;
    let replacements = replacements regexps line in
    try
      let prioqueue =
        String.Set.fold
          (fun line prioqueue ->
            if String.equal line "e" then raise (Res (steps + 1))
            else if not (Hashtbl.mem seen line) then
              PrioQueue.insert prioqueue (String.length line) (line, steps + 1)
            else prioqueue)
          replacements prioqueue
      in
      aux prioqueue
    with Res i -> i
  in
  aux (PrioQueue.insert PrioQueue.empty (String.length molecule) (molecule, 0))

let part_1 file =
  let molecule, regexps =
    Parse.fold_lines (parse false) ("", String.Map.empty) file
  in
  replacements regexps molecule |> String.Set.cardinal

let part_2 file =
  let molecule, regexps =
    Parse.fold_lines (parse true) ("", String.Map.empty) file
  in
  steps_to_reach regexps molecule

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
