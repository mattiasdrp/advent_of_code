open Mdrp_lib

module Make (X : Stdlib.Set.OrderedType) = struct
  type elt = X.t

  type t = { size : int; tree : tree }
  and tree = Empty | Node of t * (elt * int) * t

  let empty = { size = 0; tree = Empty }

  let rec add x t =
    match t.tree with
    | Empty -> { size = 1; tree = Node (empty, (x, 1), empty) }
    | Node (l, (v, cpt), r) ->
        let c = X.compare x v in
        let tree =
          if c = 0 then Node (l, (v, cpt + 1), r)
          else if c < 0 then Node (add x l, (v, cpt), r)
          else Node (l, (v, cpt), add x r)
        in
        { size = t.size + 1; tree }

  let mediane t =
    match t.tree with
    | Empty -> None
    | Node (l, (_, cpt), r) ->
        let med = (l.size + cpt + r.size) / 2 in
        let rec aux med t =
          match t.tree with
          | Empty -> None
          | Node (l, (v, cpt), r) ->
              let posv = l.size + cpt in
              if posv >= med && med >= l.size then Some v
              else if posv < med then aux (med - l.size - cpt) r
              else aux med l
        in
        aux med t
end

module Tree = Tree.Make (Int)

(* Part 1 *)

let part_1 file =
  let tree, list =
    Parse.fold_parse ~sep:','
      (fun (tree, list) v ->
        let v = int_of_string v in
        (Tree.add v tree, v :: list))
      (Tree.empty, []) file
  in
  match Tree.mediane tree with
  | Some mediane -> List.fold_left (fun acc v -> acc + abs (v - mediane)) 0 list
  | None -> assert false

(* Part 2 *)

let path x y =
  let rec aux x i acc = if x = y then acc else aux (x + 1) (i + 1) (acc + i) in
  aux x 1 0

let total mean list =
  List.fold_left (fun acc x -> path (min x mean) (max x mean) + acc) 0 list

let part_2 file =
  let acc, cpt, list =
    Parse.fold_parse ~sep:','
      (fun (acc, cpt, list) v ->
        let v = int_of_string v in
        (acc + v, cpt + 1, v :: list))
      (0, 0, []) file
  in
  let mean1 = acc / cpt in
  let mean2 = int_of_float (ceil (float acc /. float cpt)) in
  min (total mean1 list) (total mean2 list)

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
