open Mdrp_lib

module Dir = struct
  type t = Left | Right | Down | Up

  let compare = compare

  let pp ppf t =
    Format.fprintf ppf "%c"
      (match t with Left -> '<' | Right -> '>' | Up -> '^' | Down -> 'v')

  let move width height (abs, ord) = function
    | Left ->
        let abs = abs - 1 in
        if abs = 0 then (width, ord) else (abs, ord)
    | Right ->
        let abs = abs + 1 in
        if abs = width + 1 then (1, ord) else (abs, ord)
    | Up ->
        let ord = ord - 1 in
        if ord = 0 then (abs, height) else (abs, ord)
    | Down ->
        let ord = ord + 1 in
        if ord = height + 1 then (abs, 1) else (abs, ord)
end

let c_to_dir = function
  | '>' -> Dir.Right
  | '<' -> Left
  | '^' -> Up
  | 'v' -> Down
  | _ -> raise Exit

module S = Set.Make (Dir)

module Map = struct
  module IM = Map.Make (struct
    type t = int * int

    let compare = compare
    let pp = Pair.pp Int.pp Int.pp
  end)

  type t = { map : S.t IM.t; width : int; height : int; lcm : int }

  let add key value t = { t with map = IM.add key value t.map }
  let find key t = IM.find key t.map
  let update key f t = { t with map = IM.update key f t.map }

  let empty t =
    match t with
    | None -> { map = IM.empty; width = 0; height = 0; lcm = 0 }
    | Some t -> { t with map = IM.empty }

  let fold f t acc = IM.fold f t.map acc

  let pp ppf t =
    Format.fprintf ppf "@[<v 0>";
    for ord = 1 to t.height do
      for abs = 1 to t.width do
        match IM.find (abs, ord) t.map with
        | s ->
            if S.cardinal s = 1 then Format.fprintf ppf "%a" Dir.pp (S.choose s)
            else Format.fprintf ppf "%d" (S.cardinal s)
        | exception Not_found -> Format.fprintf ppf "."
      done;
      Format.fprintf ppf "@,"
    done;
    Format.fprintf ppf "@]"

  let step =
    let memo = Hashtbl.create 19 in
    fun (minute : int) t ->
      try Hashtbl.find memo (minute mod t.lcm)
      with Not_found ->
        let map =
          fold
            (fun coords set acc ->
              S.fold
                (fun dir acc ->
                  let ncoords = Dir.move t.width t.height coords dir in
                  update ncoords
                    (function
                      | Some s -> Some (S.add dir s)
                      | None -> Some (S.singleton dir))
                    acc)
                set acc)
            t (empty (Some t))
        in
        Hashtbl.add memo minute map;
        map
end

let init_map file =
  let width, height, map =
    Parse.fold_lines
      (fun (_, ord, map) s ->
        let map =
          String.foldi
            (fun abs map c ->
              match c_to_dir c with
              | dir -> Map.add (abs, ord) (S.singleton dir) map
              | exception Exit -> map)
            map s
        in
        (String.length s, ord + 1, map))
      (0, 0, Map.empty None)
      file
  in
  {
    map with
    width = width - 2;
    height = height - 2;
    lcm = Int.Decimal.lcm (width - 2) (height - 2);
  }

let bfs start goal minute map =
  let module Visited = Set.Make (struct
    type t = (int * int) * int

    let compare = compare
    let pp _ppf _t = failwith "TODO"
  end) in
  let queue = Queue.create () in
  Queue.add (start, minute) queue;
  let visited = Visited.singleton (start, minute) in
  let rec aux map visited =
    if Queue.is_empty queue then minute
    else
      let ((abs, ord) as coord), minute = Queue.pop queue in
      if coord = goal then minute
      else
        let minute = minute + 1 in
        let map = Map.step minute map in
        let neighbours =
          [
            (abs, ord + 1);
            (abs + 1, ord);
            (abs, ord - 1);
            (abs - 1, ord);
            (abs, ord);
          ]
        in
        let visited =
          List.fold_left
            (fun visited (abs, ord) ->
              let nb = ((abs, ord), minute) in
              if
                (* Coordinates are valid *)
                (abs > 0 && abs <= map.Map.width && ord > 0
                 && ord <= map.Map.height
                (* Coords is the start point *)
                || (abs, ord) = start
                || (* Coords is the goal point *)
                (abs, ord) = goal)
                (* Coordinates at this minute were not already visited *)
                && (not (Visited.mem nb visited))
                (* Spot is empty at this minute *)
                &&
                match Map.find (abs, ord) map with
                | s -> S.is_empty s
                | exception Not_found -> true
              then (
                Queue.add nb queue;
                Visited.add nb visited)
              else visited)
            visited neighbours
        in
        aux map visited
  in
  aux map visited

let part_1 file =
  let map = init_map file in
  Format.eprintf "%a@." Map.pp map;
  let res = bfs (0, 1) (map.Map.width, map.Map.height + 1) 0 map in
  res

let part_2 file =
  let map = init_map file in
  let res1 = bfs (0, 1) (map.Map.width, map.Map.height + 1) 0 map in
  let res2 = bfs (map.Map.width, map.Map.height + 1) (0, 1) res1 map in
  let res3 = bfs (0, 1) (map.Map.width, map.Map.height + 1) res2 map in
  res3

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
