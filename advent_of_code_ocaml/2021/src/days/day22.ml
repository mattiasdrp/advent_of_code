open Mdrp_lib

module Lit = struct
  type t = On | Off

  let of_string = function
    | "on" -> On
    | "off" -> Off
    | _ -> failwith "Invalid lit string"

  let pp ppf = function
    | On -> Format.fprintf ppf "on"
    | Off -> Format.fprintf ppf "off"
end

module Cube = struct
  type t = {
    lit : Lit.t;
    xmin : int;
    xmax : int;
    ymin : int;
    ymax : int;
    zmin : int;
    zmax : int;
  }

  let size { xmin; xmax; ymin; ymax; zmin; zmax; _ } =
    (xmax - xmin + 1) * (ymax - ymin + 1) * (zmax - zmin + 1)

  let make lit (xmin, xmax) (ymin, ymax) (zmin, zmax) =
    { lit; xmin; xmax; ymin; ymax; zmin; zmax }

  let pp ppf { lit; xmin; xmax; ymin; ymax; zmin; zmax } =
    Format.fprintf ppf
      "{lit: %a; xmin: %d; xmax:%d ; ymin:%d ; ymax:%d ; zmin:%d ; zmax: %d }"
      Lit.pp lit xmin xmax ymin ymax zmin zmax

  let pp_list ppf { lit; xmin; xmax; ymin; ymax; zmin; zmax } =
    if lit = On then
      for x = xmin to xmax do
        for y = ymin to ymax do
          for z = zmin to zmax do
            Format.fprintf ppf "%d, %d, %d@," x y z
          done
        done
      done

  type col = Left | Right | Contained | Contains | CNone
  type collision = { x : col; y : col; z : col }

  let int_collision min1 max1 min2 max2 =
    (* Format.eprintf "%d, %d -> %d, %d: " min1 max1 min2 max2; *)
    if
      max2 < min1 || min2 > max1
      (* --------               *)
      (*               -------- *)
    then CNone
    else if
      min2 <= min1 && max2 >= min1 && max2 < max1
      (*            --------    *)
      (*        --------        *)
    then Left
      (*      --------          *)
      (*          --------      *)
    else if
      min2 > min1 && min2 <= max1 && max2 >= max1
      (*            --------    *)
      (*        --------        *)
    then Right
    else if
      min2 > min1 && max2 < max1
      (*     ---------------    *)
      (*        --------        *)
    then Contained
    else
      (*        --------        *)
      (*     ---------------    *)
      Contains

  let collision c1 c2 =
    let x = int_collision c1.xmin c1.xmax c2.xmin c2.xmax in
    let y = int_collision c1.ymin c1.ymax c2.ymin c2.ymax in
    let z = int_collision c1.zmin c1.zmax c2.zmin c2.zmax in
    { x; y; z }

  let change_xmin c x = { c with xmin = x }
  let change_xmax c x = { c with xmax = x }
  let change_ymin c y = { c with ymin = y }
  let change_ymax c y = { c with ymax = y }
  let change_zmin c z = { c with zmin = z }
  let change_zmax c z = { c with zmax = z }

  let split col c1 min2 max2 change_min change_max =
    (* split c1 in two parts: *)
    (* the part that can't be touched anymore by c2 *)
    (* the part that can still be touched by c2 in *)
    (* (in following dimensions) *)
    match col with
    (* c2 contains c1, all c1 can be touched by the next dimensions *)
    | Contains -> ([], [ c1 ])
    (* c2 is contained in c1, the left and right part of c1 can't *)
    (* be touched anymore but the center part can *)
    | Contained ->
        ( [ change_max c1 (min2 - 1); change_min c1 (max2 + 1) ],
          [ change_min (change_max c1 max2) min2 ] )
    (* c2 is on c1 left side so the right side of c1 is untouched *)
    (* and the left side can be touched *)
    | Left -> ([ change_min c1 (max2 + 1) ], [ change_max c1 max2 ])
    (* c2 is on c1 right side so the left side of c1 is untouched *)
    (* and the right side can be touched *)
    | Right -> ([ change_max c1 (min2 - 1) ], [ change_min c1 min2 ])
    | CNone -> assert false

  (* Intersection will always take the second cube lit *)
  let intersect c1 c2 =
    match collision c1 c2 with
    | { x = CNone; _ } | { y = CNone; _ } | { z = CNone; _ } ->
        (* Format.eprintf "None@."; *)
        [ c1 ]
    | { x; y; z } ->
        (* 1D *)
        let untouched_c1, touched_c1 =
          split x c1 c2.xmin c2.xmax change_xmin change_xmax
        in
        (* 2D *)
        let untouched_c1, touched_c1 =
          List.fold_left
            (fun (untouched, touched) c1 ->
              let uc1, tc1 =
                split y c1 c2.ymin c2.ymax change_ymin change_ymax
              in
              (List.rev_append uc1 untouched, List.rev_append tc1 touched))
            (untouched_c1, []) touched_c1
        in
        (* 3D *)
        let untouched_c1, _ =
          List.fold_left
            (fun (untouched, touched) c1 ->
              let uc1, tc1 =
                split z c1 c2.zmin c2.zmax change_zmin change_zmax
              in
              (List.rev_append uc1 untouched, List.rev_append tc1 touched))
            (untouched_c1, []) touched_c1
        in
        untouched_c1
end

let split_coord s =
  match String.split_on_char '=' s with
  | [ _; vals ] -> (
      match String.split_on_char_non_empty '.' vals with
      | [ min; max ] -> (int_of_string min, int_of_string max)
      | _ -> failwith (Format.sprintf "%s is not a valid interval" vals))
  | _ -> failwith (Format.sprintf "%s is not a valid coord" s)

let split_line line =
  match String.split_on_char ' ' line with
  | [ lit; coords ] -> (
      match String.split_on_char ',' coords with
      | [ x; y; z ] ->
          let x = split_coord x in
          let y = split_coord y in
          let z = split_coord z in
          Cube.make (Lit.of_string lit) x y z
      | _ -> failwith (Format.sprintf "%s is not a valid coords" coords))
  | _ -> failwith " not a valid line"

let part_1 file =
  let l =
    Parse.fold_lines
      (fun acc line ->
        let c = split_line line in
        if
          c.xmax < -50 || c.xmin > 50 || c.ymax < -50 || c.ymin > 50
          || c.zmax < -50 || c.zmin > 50
        then acc
        else c :: acc)
      [] file
    |> List.rev
  in

  let l =
    List.fold_left
      (fun acc c ->
        c :: List.flatten (List.map (fun c1 -> Cube.intersect c1 c) acc))
      [ List.hd l ]
      (List.tl l)
  in
  List.fold_left
    (fun acc c -> if c.Cube.lit = On then acc + Cube.size c else acc)
    0 l
  |> Format.printf "%d@."

let part_2 file =
  let l =
    Parse.fold_lines (fun acc line -> split_line line :: acc) [] file
    |> List.rev
  in
  let _id, _prev, l =
    List.fold_left
      (fun (id, _, acc) c ->
        let acc =
          c :: List.flatten (List.map (fun c1 -> Cube.intersect c1 c) acc)
        in
        let length = List.length acc in
        (id + 1, length, acc))
      (0, 0, [ List.hd l ])
      (List.tl l)
  in
  let total =
    List.fold_left
      (fun acc c -> if c.Cube.lit = On then acc + Cube.size c else acc)
      0 l
  in
  Format.printf "%d@." total

let run part file =
  match part with 1 -> part_1 file | 2 -> part_2 file | _ -> ()
