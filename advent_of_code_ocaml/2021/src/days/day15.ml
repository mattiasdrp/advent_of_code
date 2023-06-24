open Mdrp_lib

module Cell = struct
  type t = {
    i : int;
    j : int;
    risk : int;
    mutable distance : int;
    mutable visited : bool;
  }

  let compare c1 c2 =
    let c = compare c1.j c2.j in
    if c = 0 then compare c1.i c2.i else c

  let pp ppf { i; j; risk; distance; visited } =
    Format.fprintf ppf "{i: %d; j: %d; risk: %d; distance: %d; %b}" i j risk
      distance visited

  let priority { distance; _ } = distance
end

module CSet = struct
  include Set.Make (Cell)
  open Cell

  let get_min_distance t =
    let min =
      fold
        (fun ({ distance; _ } as cell) acc ->
          if distance < acc.distance then cell else acc)
        t (min_elt t)
    in
    (min, remove min t)
end

module CList = struct
  type elt = Cell.t
  type t = elt list

  let empty = []

  let add e l =
    let rec aux acc = function
      | [] -> List.rev (e :: acc)
      | hd :: tl when e > hd -> aux (hd :: acc) tl
      | l -> List.rev_append acc (e :: l)
    in
    aux [] l

  let get_min_distance = function [] -> raise Not_found | hd :: tl -> (hd, tl)
end

module CHeap = struct
  module Heap = Heap.Make (struct
    type t = Cell.t

    let compare c1 c2 = Cell.(Stdlib.compare c1.distance c2.distance)
  end)

  type elt = Cell.t
  type t = Heap.t

  let empty = Heap.empty
  let add = Heap.add

  let get_min_distance t =
    let r = Heap.find_min t in
    (r, Heap.del_min t)
end

module type SetType = sig
  type elt
  type t

  val empty : t
  val add : elt -> t -> t
  val get_min_distance : t -> elt * t
end

let dijkstra matrix =
  let cellset =
    match try Sys.argv.(4) with _ -> "1" with
    | "1" ->
        Format.eprintf "Visited stored in a Set@.";
        (module CSet : SetType with type elt = Cell.t)
    | "2" ->
        Format.eprintf "Visited stored in a sorted List@.";
        (module CList : SetType with type elt = Cell.t)
    | _ ->
        Format.eprintf "Visited stored in Batteries Heap@.";
        (module CHeap : SetType with type elt = Cell.t)
  in
  let module CellSet : SetType with type elt = Cell.t = (val cellset) in
  let open Cell in
  matrix.(0).(0).distance <- 0;
  let width, height = Array.Matrix.width_height matrix in
  let rec dijsktra current modified =
    (* Format.eprintf "@[<v 2>%a@." Array.Matrix.(pp Cell.(pp ~verbosity:1)) matrix; *)
    let modified =
      Seq.fold_left
        (fun modified (i, j) ->
          let ({ risk; distance; visited; _ } as cell) = matrix.(i).(j) in
          if not visited then
            let new_dist = risk + current.distance in
            if distance > new_dist then (
              cell.distance <- new_dist;
              CellSet.add cell modified)
            else modified
          else modified)
        modified
        (Array.Matrix.neumann_neighbourhood matrix current.i current.j)
    in
    current.visited <- true;
    if current.i = height - 1 && current.j = width - 1 then current.distance
    else
      let current, modified = CellSet.get_min_distance modified in
      dijsktra current modified
  in
  dijsktra matrix.(0).(0) CellSet.empty

let greedy_shortest_path matrix =
  let open Cell in
  let width, height = Array.Matrix.width_height matrix in

  let queue = Queue.create () in
  Queue.add matrix.(0).(0) queue;
  matrix.(0).(0).distance <- 0;
  (* matrix.(0).(0).visited <- true; *)
  let rec loop () =
    (* Format.eprintf "@[<v 2>%a@." Array.Matrix.(pp Cell.(pp ~verbosity:1)) matrix; *)
    let current = Queue.pop queue in
    Seq.iter
      (fun (i, j) ->
        let ({ risk; distance; _ } as cell) = matrix.(i).(j) in
        let new_dist = risk + current.distance in
        if distance > new_dist then (
          cell.distance <- new_dist;
          Queue.add cell queue (* cell.visited <- true *)))
      (Array.Matrix.neumann_neighbourhood matrix current.i current.j);
    if Queue.is_empty queue then matrix.(width - 1).(height - 1).distance
    else loop ()
  in
  loop ()

let part_1 file algo =
  let matrix =
    let _, matrix =
      Parse.fold_lines
        (fun (i, matrix) s ->
          let line =
            String.to_arrayi
              (fun j c ->
                Cell.
                  {
                    i;
                    j;
                    risk = Char.to_digit c;
                    distance = 50;
                    visited = false;
                  })
              s
          in
          (i + 1, line :: matrix))
        (0, []) file
    in
    matrix |> List.rev |> Array.of_list
  in
  match algo with
  | 1 ->
      Format.eprintf "Greedy Algorithm@.";
      greedy_shortest_path matrix
  | _ ->
      Format.eprintf "Dijkstra@.";
      dijkstra matrix

let part_2 file algo =
  let open Cell in
  let matrix =
    let _, matrix =
      Parse.fold_lines
        (fun (i, matrix) s ->
          let line =
            String.to_arrayi
              (fun j c ->
                Cell.
                  {
                    i;
                    j;
                    risk = Char.to_digit c;
                    distance = max_int / 2;
                    visited = false;
                  })
              s
          in
          (i + 1, line :: matrix))
        (0, []) file
    in
    matrix |> List.rev |> Array.of_list
  in
  let width, height = Array.Matrix.width_height matrix in
  let matrix =
    Array.init (5 * width) (fun i ->
        Array.init (5 * height) (fun j ->
            let cell = matrix.(i mod height).(j mod height) in
            let risk = cell.risk + (i / height) + (j / height) in
            let risk =
              if risk > 9 then if risk mod 9 = 0 then 9 else risk mod 9
              else risk
            in
            { i; j; risk; distance = max_int / 2; visited = false }))
  in
  (* Format.eprintf "@[<v 2>%a@." Array.Matrix.(pp Cell.(pp ~verbosity:0)) matrix; *)
  match algo with
  | 1 ->
      Format.eprintf "Greedy Algorithm@.";
      greedy_shortest_path matrix
  | _ ->
      Format.eprintf "Dijkstra@.";
      dijkstra matrix

let run part file algo =
  match part with 1 -> part_1 file algo | _ -> part_2 file algo
