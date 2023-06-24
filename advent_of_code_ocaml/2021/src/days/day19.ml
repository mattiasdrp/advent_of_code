open Mdrp_lib

(* Disclaimer *)
(* *)
(* This solution is heavily brute forced. The easiest optimisation to make *)
(* is some kind of "heuristic" for each beacon. Being able to create a hash *)
(* or signature to each beacon would allow us to reject immediately the ones *)
(* that seem too different *)
module IM = Int.Map

module Coords = struct
  type t = int * int * int

  let ( = ) : int -> int -> bool = ( = )

  let compare (x1, y1, z1) (x2, y2, z2) =
    let c = Int.compare x1 x2 in
    if c = 0 then
      let c = Int.compare y1 y2 in
      if c = 0 then Int.compare z1 z2 else c
    else c

  let equal (x1, y1, z1) (x2, y2, z2) = x1 = x2 && y1 = y2 && z1 = z2
  let distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

  let manhattan (x1, y1, z1) (x2, y2, z2) =
    (abs (x1 - x2), abs (y1 - y2), abs (z1 - z2))

  let translation t1 t2 =
    let xd, yd, zd = distance t1 t2 in

    ((fun (x, y, z) -> (x + xd, y + yd, z + zd)), (xd, yd, zd))

  let rotations =
    [
      (1, fun (x, y, z) -> (x, y, z));
      (2, fun (x, y, z) -> (x, -z, y));
      (3, fun (x, y, z) -> (x, -y, -z));
      (4, fun (x, y, z) -> (x, z, -y));
      (5, fun (x, y, z) -> (-x, -y, z));
      (6, fun (x, y, z) -> (-x, -z, -y));
      (7, fun (x, y, z) -> (-x, y, -z));
      (8, fun (x, y, z) -> (-x, z, y));
      (9, fun (x, y, z) -> (y, z, x));
      (10, fun (x, y, z) -> (y, -x, z));
      (11, fun (x, y, z) -> (y, -z, -x));
      (12, fun (x, y, z) -> (y, x, -z));
      (13, fun (x, y, z) -> (-y, -z, x));
      (14, fun (x, y, z) -> (-y, -x, -z));
      (15, fun (x, y, z) -> (-y, z, -x));
      (16, fun (x, y, z) -> (-y, x, z));
      (17, fun (x, y, z) -> (z, x, y));
      (18, fun (x, y, z) -> (z, -y, x));
      (19, fun (x, y, z) -> (z, -x, -y));
      (20, fun (x, y, z) -> (z, y, -x));
      (21, fun (x, y, z) -> (-z, -x, y));
      (22, fun (x, y, z) -> (-z, -y, -x));
      (23, fun (x, y, z) -> (-z, x, -y));
      (24, fun (x, y, z) -> (-z, y, x));
    ]

  (* let rot x y z = [ (x, y, z); (x, -z, y); (x, -y, -z); (x, z, -y) ] in *)
  (* let negative x y z = rot (-x) (-y) z in *)
  (* let all_rotations x y z = rot x y z @ negative x y z in *)
  (* all_rotations x y z @ all_rotations y z x @ all_rotations z x y *)

  let pp ppf (x, y, z) = Format.fprintf ppf "(%d, %d, %d)" x y z

  let name, pp_names =
    let names = Hashtbl.create 19 in
    let cpt = ref 0 in
    ( (fun (c : t) ->
        try Hashtbl.find names c
        with Not_found ->
          let id = !cpt in
          let name = Format.sprintf "pair%d" id in
          incr cpt;
          Hashtbl.add names c name;
          name),
      fun ppf () ->
        let cpp = pp in
        Format.fprintf ppf "@[<v 1>%a@."
          List.(pp ~pp_sep:Format.pp_print_cut Pair.(pp cpp String.pp))
          (Hashtbl.to_seq names |> List.of_seq) )
end

module CS = Set.Make (Coords)
module CM = Map.Make (Coords)

module PCS = Set.Make (struct
  type t = Coords.t * Coords.t

  let compare (c1, c2) (c1', c2') =
    let c = Coords.compare c1 c1' in
    if c = 0 then Coords.compare c2 c2' else c

  let pp ppf (c1, c2) = Format.fprintf ppf "(%a, %a)" Coords.pp c1 Coords.pp c2
end)

let all_distances l =
  let rec aux acc rest = function
    | [] -> acc
    | c1 :: tl ->
        aux
          (List.fold_left
             (fun acc c2 ->
               let dist = Coords.(distance c1 c2) in
               CM.update c1
                 (function
                   | Some dist_set -> Some (PCS.add (dist, c2) dist_set)
                   | None -> Some (PCS.singleton (dist, c2)))
                 acc)
             acc (List.rev_append rest tl))
          (c1 :: rest) tl
  in
  aux CM.empty [] l

let find_matching c1 dist rotation acc l =
  let rec aux acc = function
    | [] -> acc
    | (c2, dist2_set) :: tl -> (
        match
          List.find_opt
            (fun (dist2, _) -> Coords.equal dist (rotation dist2))
            (dist2_set |> PCS.elements)
        with
        | Some (dist2, _) -> ((c1, c2), (dist, rotation dist2)) :: acc
        | None -> aux acc tl)
  in
  aux acc l

let find_common dist_set scan2 =
  let rec aux acc = function
    | [] -> (None, acc)
    | (c2, dist2_set) :: tl ->
        let cardinal =
          PCS.cardinal
            (PCS.filter
               (fun (dist1, _) ->
                 PCS.exists
                   (fun (dist2, _) -> Coords.equal dist1 dist2)
                   dist2_set)
               dist_set)
        in
        if cardinal >= 11 then (Some c2, List.rev_append acc tl)
        else aux ((c2, dist2_set) :: acc) tl
  in
  aux [] scan2

let find_first (scan1, scanners) scan2 =
  (* Let's find 3 distances in scan2 that are common with distances in scan1. *)
  (* Considering n beacons in scan1, scan1 contains, for each beacon, *)
  (* its distances to all the other beacons (allowing us to get rid of *)
  (* relative distances). *)
  (* For each beacon in scan1, if there exists a beacon in scan2 that *)
  (* has 3 common distances (modulo rotation and axis) with it, then these *)
  (* 2 beacons are the same but from a different perspective. *)
  (* Once we find 2 common beacons, we know how scan2 is positioned relatively *)
  (* to scan1 and we don't need to check modulo again. *)
  (* All other beacons from scan1 are left untouched and all beacons *)
  (* from scan2 are positioned relative to scan1 *)
  let scan2 = CM.bindings scan2 in
  let rec find_first = function
    | (c1, dist_set) :: tl -> (
        let rec aux = function
          | (idr, rotation) :: tl ->
              (* C1 has 23 distances in dist_map *)
              (* Does this rotation allows a c2 in scan2 to have 12 *)
              (* common distances with it? *)
              let res =
                PCS.fold
                  (fun (dist, _) acc ->
                    find_matching c1 dist rotation acc scan2)
                  dist_set []
              in
              if List.length res >= 11 then Some (idr, rotation) else aux tl
          | [] -> None
        in
        match aux Coords.rotations with
        | Some (idr, rotation) ->
            (* We found a rotation and a coordinate in c1 that allow *)
            (* c1 to have 11 common distances with a coordinate c2 in scan2 *)
            (* Now let's find all the coordinates in scan1 that have 11 *)
            (* common distances with a coordinate in scan2 with this *)
            (* rotation *)
            (* Format.eprintf "%d@." idr; *)
            Some (idr, rotation)
        | None -> find_first tl)
    | [] -> None
  in
  match find_first (CM.bindings scan1) with
  | Some (_, rotation) ->
      let scan2 =
        List.map
          (fun (c2, dist_set) ->
            ( rotation c2,
              PCS.map (fun (dist, c) -> (rotation dist, rotation c)) dist_set ))
          scan2
      in
      let reference, scan2 =
        CM.fold
          (fun c1 dist_set (reference, scan2) ->
            (* Find a c2 in scan2 that has a dist_set that has common distances *)
            (* with c1 dist_set *)
            match find_common dist_set scan2 with
            | None, scan2 -> (reference, scan2)
            | Some c2, scan2 -> (Some (c1, c2), scan2))
          scan1 (None, scan2)
      in
      let translation, dist =
        match reference with
        | Some (c1, c2) -> Coords.translation c1 c2
        | _ -> assert false
      in

      (* Format.eprintf "Dist is %a@." Coords.pp dist; *)
      let scan1 =
        List.fold_left
          (fun acc (c, dist_set) -> CM.add (translation c) dist_set acc)
          scan1 scan2
      in
      Some (scan1, dist :: scanners)
  | None -> None

let parse file =
  Parse.fold_lines
    (fun (scanner, set, map) s ->
      if String.contains s 's' then
        let map = IM.add scanner set map in
        let set = CS.empty in
        let scanner =
          match String.split_on_char ' ' s with
          | [ _; _; s; _ ] -> int_of_string s
          | _ -> assert false
        in
        (scanner, set, map)
      else
        match String.split_on_char ',' s with
        | [ x; y; z ] ->
            ( scanner,
              CS.add (int_of_string x, int_of_string y, int_of_string z) set,
              map )
        | _ -> (scanner, set, map))
    (0, CS.empty, IM.empty) file

let run part file =
  let scanner, set, map = parse file in
  let map = IM.add scanner set map in
  let map' = IM.map (fun s -> CS.elements s |> all_distances) map in
  let _, scanners =
    let rec aux acc rest = function
      | (id, scan) :: tl -> (
          match find_first acc scan with
          | Some scan -> aux scan rest tl
          | None -> aux acc ((id, scan) :: rest) tl)
      | [] -> ( match rest with [] -> acc | l -> aux acc [] (List.rev l))
    in
    aux
      (IM.find 0 map', [])
      []
      (IM.bindings map' (* |> List.map (fun (_, a) -> a) *))
  in
  match part with
  | 1 -> List.length scanners
  | _ ->
      let rec all_manhattan acc = function
        | d1 :: tl ->
            all_manhattan
              (List.fold_left
                 (fun acc d2 ->
                   let x, y, z = Coords.manhattan d1 d2 in
                   Int.Map.add (x + y + z) (x, y, z) acc)
                 acc tl)
              tl
        | [] -> acc
      in
      let map = all_manhattan Int.Map.empty scanners in
      let max, _coord = Int.Map.max_binding map in
      max
