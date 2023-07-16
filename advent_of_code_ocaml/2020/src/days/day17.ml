open Mdrp_lib

module Point3D = struct
  type t = { x : int; y : int; z : int }

  let moore_neighbourhood { x; y; z } =
    let maxx = x + 1 in
    let maxy = y + 1 in
    let maxz = z + 1 in

    let rec moore_neighbourhood_aux (nx, ny, nz) =
      if nx > maxx then None
      else if nx = x && ny = y && nz = z then
        moore_neighbourhood_aux (nx, ny, nz + 1)
      else if nz > maxz then moore_neighbourhood_aux (nx, ny + 1, z - 1)
      else if ny > maxy then moore_neighbourhood_aux (nx + 1, y - 1, z - 1)
      else Some ({ x = nx; y = ny; z = nz }, (nx, ny, nz + 1))
    in
    Seq.unfold moore_neighbourhood_aux (x - 1, y - 1, z - 1)

  let compare t1 t2 =
    let c = Int.compare t1.z t2.z in
    if c = 0 then
      let c = Int.compare t1.x t2.x in
      if c = 0 then Int.compare t1.y t2.y else c
    else c

  let pp ppf { x; y; z } = Format.fprintf ppf "{x: %d; y: %d; z: %d}" x y z
end

module Graph (Point : sig
  type t

  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val moore_neighbourhood : t -> t Seq.t
end) =
struct
  module M = Map.Make (Point)
  include M

  let graph_of_neighbours t =
    M.fold
      (fun p _ nmap ->
        Seq.fold_left
          (fun nmap p -> M.add p false nmap)
          nmap
          (Point.moore_neighbourhood p))
      t M.empty

  let active_neighbours t p =
    Point.moore_neighbourhood p
    |> Seq.fold_left
         (fun acc p ->
           match M.find p t with
           | true -> acc + 1
           | _ | (exception Not_found) -> acc)
         0

  let update_state prev_t t =
    M.mapi
      (fun p _ ->
        let v =
          match M.find p prev_t with v -> v | exception Not_found -> false
        in
        (v
        &&
        let act = active_neighbours prev_t p in
        act = 2 || act = 3)
        || ((not v) && active_neighbours prev_t p = 3))
      t

  let cycle t = update_state t (graph_of_neighbours t)

  let cycles n t =
    let rec aux i t = if i = n then t else aux (i + 1) (cycle t) in
    aux 0 t

  let total_actives t = M.fold (fun _ v acc -> if v then acc + 1 else acc) t 0
end

let part_1 file =
  let module Graph = Graph (Point3D) in
  Parse.fold_lines
    (fun (x, graph) line ->
      ( x + 1,
        String.foldi
          (fun y acc c ->
            Graph.add
              Point3D.{ x; y; z = 0 }
              (match c with '.' -> false | '#' -> true | _ -> assert false)
              acc)
          graph line ))
    (0, Graph.empty) file
  |> snd |> Graph.cycles 6 |> Graph.total_actives

module Point4D = struct
  type t = { x : int; y : int; z : int; w : int }

  let moore_neighbourhood { x; y; z; w } =
    let maxx = x + 1 in
    let maxy = y + 1 in
    let maxz = z + 1 in
    let maxw = w + 1 in

    let rec moore_neighbourhood_aux (nx, ny, nz, nw) =
      if nx > maxx then None
      else if nx = x && ny = y && nz = z && nw = w then
        moore_neighbourhood_aux (nx, ny, nz, nw + 1)
      else if nw > maxw then moore_neighbourhood_aux (nx, ny, nz + 1, w - 1)
      else if nz > maxz then moore_neighbourhood_aux (nx, ny + 1, z - 1, w - 1)
      else if ny > maxy then
        moore_neighbourhood_aux (nx + 1, y - 1, z - 1, w - 1)
      else Some ({ x = nx; y = ny; z = nz; w = nw }, (nx, ny, nz, nw + 1))
    in
    Seq.unfold moore_neighbourhood_aux (x - 1, y - 1, z - 1, w - 1)

  let compare = Stdlib.compare

  let pp ppf { x; y; z; w } =
    Format.fprintf ppf "{x: %d; y: %d; z: %d; w: %d}" x y z w
end

let part_2 file =
  let module Graph = Graph (Point4D) in
  Parse.fold_lines
    (fun (x, graph) line ->
      ( x + 1,
        String.foldi
          (fun y acc c ->
            Graph.add
              Point4D.{ x; y; z = 0; w = 0 }
              (match c with '.' -> false | '#' -> true | _ -> assert false)
              acc)
          graph line ))
    (0, Graph.empty) file
  |> snd |> Graph.cycles 6 |> Graph.total_actives

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
