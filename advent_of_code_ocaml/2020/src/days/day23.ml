open Mdrp_lib

module Ring = struct
  type t = { content : int array; size : int }

  let pp init ppf { content; _ } =
    let rec aux v =
      Format.fprintf ppf "%d" v;
      let v' = content.(v - 1) in
      if v' = init then () else aux v'
    in
    aux init

  let fill s size =
    let length = String.length s in
    let content = Array.init size (fun i -> if i >= length then i + 2 else 0) in
    String.iteri
      (fun i c ->
        let v = Char.to_digit c in
        if size <> length && i = length - 1 then content.(v - 1) <- length + 1
        else
          let v_next = Char.to_digit s.[(i + 1) mod size] in
          content.(v - 1) <- v_next)
      s;
    if size <> length then content.(size - 1) <- Char.to_digit s.[0];
    { content; size }

  let pick_next_three { content; size } v =
    let v1 = content.(v - 1) in
    let v2 = content.(v1 - 1) in
    let v3 = content.(v2 - 1) in
    let v4 = content.(v3 - 1) in
    let rec aux v =
      let v = if v < 1 then size else v in
      if v = v1 || v = v2 || v = v3 then aux (v - 1) else v
    in
    let nv = aux (v - 1) in
    content.(v - 1) <- v4;
    let nv_next = content.(nv - 1) in
    content.(nv - 1) <- v1;
    content.(v1 - 1) <- v2;
    content.(v2 - 1) <- v3;
    content.(v3 - 1) <- nv_next

  let loop init t =
    let rec aux step current =
      if step = 100 then
        int_of_string (String.sub (Format.asprintf "%a" (pp 1) t) 1 8)
      else (
        pick_next_three t current;
        let current = t.content.(current - 1) in
        aux (step + 1) current)
    in
    aux 0 init

  let loop_10_million init t =
    let rec aux step current =
      if step = 10_000_000 then
        let v1 = t.content.(0) in
        let v2 = t.content.(v1 - 1) in
        v1 * v2
      else (
        pick_next_three t current;
        let current = t.content.(current - 1) in
        aux (step + 1) current)
    in
    aux 0 init
end

let part_1 file =
  let ci = open_in file in
  let ring = input_line ci in
  close_in ci;
  let init = Char.to_digit ring.[0] in
  let ring = Ring.fill ring 9 in
  Ring.loop init ring

let part_2 file =
  let ci = open_in file in
  let ring = input_line ci in
  close_in ci;
  let init = Char.to_digit ring.[0] in
  let ring = Ring.fill ring 1_000_000 in
  (* Format.eprintf "%a@." Ring.(pp 3) ring; *)
  Ring.loop_10_million init ring

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
