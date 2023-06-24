open Mdrp_lib

let split_on_arrow s =
  let open String in
  let buf = Buffer.create 8 in
  let rec aux i =
    let c = unsafe_get s i in
    if c = ' ' then
      let c1 =
        match String.split_on_char ',' (Buffer.contents buf) with
        | [ x; y ] -> (int_of_string x, int_of_string y)
        | _ -> assert false
      in
      let c2 =
        match
          String.(split_on_char ',' (sub s (i + 4) (String.length s - i - 4)))
        with
        | [ x; y ] -> (int_of_string x, int_of_string y)
        | _ -> assert false
      in
      (c1, c2)
    else (
      Buffer.add_char buf c;
      aux (i + 1))
  in
  aux 0

let add_all_interval_nodiag horizontal const incr limit acc =
  let rec aux incr acc =
    if incr > limit then acc
    else
      aux (incr + 1)
        (Int.PairMap.update
           (if horizontal then (const, incr) else (incr, const))
           (function Some n -> Some (n + 1) | None -> Some 1)
           acc)
  in
  aux incr acc

let part_1 file =
  Parse.fold_lines
    (fun acc string ->
      let (x1, y1), (x2, y2) = split_on_arrow string in
      if x1 = x2 then
        add_all_interval_nodiag false x1 (min y1 y2) (max y1 y2) acc
      else if y1 = y2 then
        add_all_interval_nodiag true y1 (min x1 x2) (max x1 x2) acc
      else acc)
    Int.PairMap.empty file
  |> fun m ->
  Int.PairMap.fold (fun _ cpt acc -> if cpt > 1 then acc + 1 else acc) m 0

let add_all_interval x1 x2 incrx cmpx y1 y2 incry cmpy acc =
  let rec aux x y acc =
    if cmpx x x2 || cmpy y y2 then acc
    else
      aux (x + incrx) (y + incry)
        (Int.PairMap.update (x, y)
           (function Some n -> Some (n + 1) | None -> Some 1)
           acc)
  in
  aux x1 y1 acc

let part_2 file =
  Parse.fold_lines
    (fun acc string ->
      let (x1, y1), (x2, y2) = split_on_arrow string in
      let incrx, cmpx =
        if x1 = x2 then (0, ( > ))
        else if x1 < x2 then (1, ( > ))
        else (-1, ( < ))
      in
      let incry, cmpy =
        if y1 = y2 then (0, ( > ))
        else if y1 < y2 then (1, ( > ))
        else (-1, ( < ))
      in
      add_all_interval x1 x2 incrx cmpx y1 y2 incry cmpy acc)
    Int.PairMap.empty file
  |> fun m ->
  Int.PairMap.fold (fun _ cpt acc -> if cpt > 1 then acc + 1 else acc) m 0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
