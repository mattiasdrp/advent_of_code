open Mdrp_lib

let step (x, y) = ((if x < 0 then x + 1 else if x = 0 then x else x - 1), y - 1)

let compute_max vy =
  (* y(n) = y(n-1) + v_y(n-1) gives
     y(n) = (n-2*(y(0)+1))(n-1)/2
     that's
     y(n) = -n^2/2 + n(v0 + 3/2) - (v0 + 1) *)
  let f abs =
    let abs = float abs in
    let vy = float vy in
    truncate ((-.(abs *. abs) /. 2.) +. (abs *. (vy +. 1.5)) -. vy -. 1.)
  in
  max (f (vy + 1)) (f (vy + 2))

(* Given an initial speed (c0) and a point we want to reach (y), we get *)
(* the minimum and maximum x that allow to reach y *)
(* In our case, y is xmin and xmax from the target, c0 the x initial speed *)
(* As an example, if we want to reach [20..30], 1, 2, 3, 4 and 5 won't *)
(* allow us to reach xmin (5+4+3+2+1=15), *)
(* 6 reaches 20 in 6 steps (0+6+5+4+3+2=20) and all steps after it stay in the target *)
(* 8 reaches 20 in 4 steps (0+8+7+6=21) and is less than 30 in 6 steps (0+8+7+6+5+4=30) *)
(* 12 reaches 20 in 3 steps (0+12+11=23) is less than 30 in the same 3 steps *)
let reciprocal ~first_half c0 y =
  let y = float y in
  let c0 = float c0 in
  let a = -0.5 in
  let b = c0 +. 1.5 in
  let c = -.(c0 +. 1.) in
  let x =
    (if first_half then ( ~-. ) else ( ~+. ))
      (sqrt ((y /. a) +. (((b *. b) -. (4. *. a *. c)) /. (4. *. a *. a))))
    -. (b /. (2. *. a))
  in
  if Float.is_nan x then None else Some (truncate (ceil x))

let f v0 x =
  let v0 = float v0 in
  let x = float x in
  truncate (-.(0.5 *. x *. x) +. ((v0 +. 1.5) *. x) -. (v0 +. 1.))

type interval = Closed of int * int | Infinite of int | INone

let get_interval ~first_half v tmin tmax =
  let fv = f v in
  match (reciprocal ~first_half v tmin, reciprocal ~first_half v tmax) with
  | Some vmin, Some vmax ->
      (* Since for y we compare negative numbers, we need to *)
      (* invert the comparators *)
      let lt, gt = if first_half then (( < ), ( > )) else (( > ), ( < )) in
      let vmin = if lt (fv vmin) tmin then vmin + 1 else vmin in
      let vmax = if gt (fv vmax) tmax then vmax - 1 else vmax in
      if vmin > vmax then INone else Closed (vmin, vmax)
  | Some x, None -> Infinite x
  | None, None -> INone
  | _ -> assert false

let aux ((_, _, high) as acc) vx ytmin ytmax xmin xmax =
  let rec aux acc vy =
    if vy + ytmax >= 0 then acc
    else
      match get_interval ~first_half:false vy ytmin ytmax with
      | Closed (ymin, ymax) ->
          if ymax >= xmin && ymin <= xmax then
            let nhigh = compute_max vy in
            let acc = if nhigh > high then (vx, vy, nhigh) else acc in
            aux acc (vy + 1)
          else aux acc (vy + 1)
      | INone -> aux acc (vy + 1)
      | _ -> assert false
  in
  aux acc 1

let part_1 (xtmin, xtmax) (ytmax, ytmin) =
  let rec get_x acc vx =
    if vx > xtmax then acc
    else
      let xtmax = min (compute_max xtmax) xtmax in
      match get_interval ~first_half:true vx xtmin xtmax with
      | Closed (xmin, xmax) -> get_x (aux acc vx ytmin ytmax xmin xmax) (vx + 1)
      | Infinite x -> aux acc vx ytmin ytmax x max_int
      | INone -> get_x acc (vx + 1)
  in
  get_x (0, 0, 0) 1 |> fun (_, _, high) -> high

let aux acc vx ytmin ytmax xmin xmax =
  let rec aux acc vy =
    if vy + ytmax >= 0 then acc
    else
      (* Format.eprintf "  vy: %d@." vy; *)
      match get_interval ~first_half:false vy ytmin ytmax with
      | Closed (ymin, ymax) ->
          if ymax >= xmin && ymin <= xmax then aux ((vx, vy) :: acc) (vy + 1)
          else aux acc (vy + 1)
      | INone -> aux acc (vy + 1)
      | _ -> assert false
  in
  aux acc ytmax

let part_2 (xtmin, xtmax) (ytmax, ytmin) =
  let rec get_x acc vx =
    if vx > xtmax then acc
    else
      let xtmax = min (compute_max xtmax) xtmax in
      let acc =
        match get_interval ~first_half:true vx xtmin xtmax with
        | Closed (xmin, xmax) -> aux acc vx ytmin ytmax xmin xmax
        | Infinite x -> aux acc vx ytmin ytmax x max_int
        | INone -> acc
      in
      get_x acc (vx + 1)
  in
  get_x [] 0 |> List.length

let run part file =
  let ci = open_in file in
  let xpair, ypair (* (ymin, ymax) *) =
    match String.split_on_char ',' (input_line ci) with
    | [ x; y ] -> (
        ( (match String.split_on_char_non_empty '.' x with
          | [ xmin; xmax ] ->
              ( int_of_string (String.sub xmin 2 (String.length xmin - 2)),
                int_of_string xmax )
          | _ -> assert false),
          match String.split_on_char_non_empty '.' y with
          | [ ymin; ymax ] ->
              ( int_of_string (String.sub ymin 3 (String.length ymin - 3)),
                int_of_string ymax )
          | _ -> assert false ))
    | _ -> assert false
  in
  close_in ci;
  match part with 1 -> part_1 xpair ypair | _ -> part_2 xpair ypair

(* let max_high vy ytmin = *)
(*   let rec aux (value, acc) y = *)
(*     if value < ytmin then acc *)
(*     else *)
(*       let value = value + y in *)
(*       aux (value, max value acc) (y - 1) *)
(*   in *)
(*   aux (0, 0) vy *)

(* let part_1 (xtmin, xtmax) (ytmin, ytmax) = *)
(*   Format.eprintf "(%d, %d) (%d, %d)@." xtmin xtmax ytmin ytmax; *)
(*   let rec aux_x (id, value, list, ids) x = *)
(*     if value >= xtmin && value <= xtmax && x = 0 then *)
(*       Some (List.hd (List.rev (id :: ids)), max_int) *)
(*     else if value > xtmax then *)
(*       if ids = [] then None else Some (List.hd (List.rev ids), List.hd ids) *)
(*     else if value >= xtmin && value <= xtmax then *)
(*       aux_x (id + 1, value + x, value :: list, id :: ids) (x - 1) *)
(*     else if x = 0 then None *)
(*     else aux_x (id + 1, value + x, value :: list, ids) (x - 1) *)
(*   in *)

(*   let rec aux_y (id, value, list, ids) y = *)
(*     if value >= ytmin && value <= ytmax then *)
(*       aux_y (id + 1, value + y, value :: list, id :: ids) (y - 1) *)
(*     else if value < ytmin then *)
(*       ( List.fold_left max 0 list, *)
(*         if ids = [] then None else Some (List.hd (List.rev ids), List.hd ids) ) *)
(*     else aux_y (id + 1, value + y, value :: list, ids) (y - 1) *)
(*   in *)

(*   let all_y = *)
(*     let rec aux acc y = *)
(*       if y + ytmin = 0 then List.rev acc *)
(*       else aux (aux_y (0, 0, [], []) y :: acc) (y + 1) *)
(*     in *)
(*     aux [] 1 *)
(*   in *)
(*   Format.eprintf "@[<v 1>%a@." *)
(*     List.( *)
(*       pp ~pp_sep:Format.pp_print_cut *)
(*         Pair.(pp Int.pp Option.(pp Pair.(pp Int.pp Int.pp)))) *)
(*     all_y; *)
(*   let rec aux x = *)
(*     if x > xtmax then () *)
(*     else ( *)
(*       (match aux_x (0, 0, [], []) x with *)
(*       | Some (min, max) -> *)
(*           Format.eprintf "X: %d@." x; *)
(*           Format.eprintf "  [%d..%s]@." min *)
(*             (if max = max_int then "inf" else string_of_int max) *)
(*           (\* List.fold_left *\) *)
(*       | None -> ()); *)
(*       aux (x + 1)) *)
(*   in *)

(*   aux 1 *)
