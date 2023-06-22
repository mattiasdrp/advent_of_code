open Mdrp_lib

module Number = struct
  type t = Int of int | Pair of { depth : int; left : t; right : t }

  let rec pp ?(min = true) () ppf = function
    | Pair { depth; left; right } ->
        if min then
          Format.fprintf ppf "[%a, %a]" (pp ~min ()) left (pp ~min ()) right
        else
          Format.fprintf ppf "[(%d), %a, %a]" depth (pp ~min ()) left
            (pp ~min ()) right
    | Int i -> Format.fprintf ppf "%d" i

  let rec pair s ~offset ~depth =
    let aux offset =
      match s.[offset] with
      | '[' -> pair s ~offset:(offset + 1) ~depth:(depth + 1)
      | '0' .. '9' as c -> (Int (Char.to_digit c), offset + 2)
      | c -> failwith (Format.sprintf "saw %c, not expected" c)
    in
    let left, offset = aux offset in
    let right, offset = aux offset in
    (Pair { depth; left; right }, offset + 1)

  let rec propagate_leftmost v = function
    | Int v' -> Int (v + v')
    | Pair { depth; left; right } ->
        Pair { depth; left = propagate_leftmost v left; right }

  let rec propagate_rightmost v = function
    | Int v' -> Int (v + v')
    | Pair { depth; left; right } ->
        Pair { depth; left; right = propagate_rightmost v right }

  let rec incr_depth = function
    | Int _ as t -> t
    | Pair { depth; left; right } ->
        Pair
          {
            depth = depth + 1;
            left = incr_depth left;
            right = incr_depth right;
          }

  let explode t =
    let rec explode t =
      match t with
      | Int _ -> (t, (None, None), false)
      (* We change a depth 4 Number by destroying it and telling its parent *)
      (* to propagate its previous left and right ints *)
      | Pair { depth = 4; left = Int l; right = Int r } ->
          (Int 0, (Some l, Some r), true)
      | Pair { depth; left; right } -> (
          match explode left with
          | left, (l, r), true ->
              (* left changed. If we need to propagate a value right, we have *)
              (* a right number so just propagate it in the leftmost number in it *)
              (* Don't touch left, though, since it's the number we just exploded, *)
              (* and propagate any left remain to the parents *)
              let right =
                match r with
                | Some r -> propagate_leftmost r right
                | None -> right
              in
              (Pair { depth; left; right }, (l, None), true)
          | left, _, false -> (
              (* left unchanged. Try to explode right *)
              match explode right with
              | right, (l, r), changed ->
                  (* Same comment as before but change left for right and vice versa *)
                  let left =
                    match l with
                    | Some l -> propagate_rightmost l left
                    | None -> left
                  in
                  (Pair { depth; left; right }, (None, r), changed)))
    in
    (* We may have some left or right int to propagate but there's *)
    (* no number to propagate them into so they're just thrown *)
    let t, _, changed = explode t in
    (t, changed)

  let split p =
    let rec split depth = function
      | Int i when i > 9 ->
          ( (if i mod 2 = 0 then
               Pair { depth; left = Int (i / 2); right = Int (i / 2) }
             else
               Pair
                 { depth; left = Int ((i - 1) / 2); right = Int ((i + 1) / 2) }),
            true )
      | Int _ as p -> (p, false)
      | Pair { depth; left; right } ->
          let left, right, changed =
            let left, changed = split (depth + 1) left in
            if changed then (left, right, changed)
            else
              let right, changed = split (depth + 1) right in
              (left, right, changed)
          in
          (Pair { depth; left; right }, changed)
    in
    split 0 p

  (* reduce rule is very simple but forces us to start from the beginning again and again *)
  (* explode has the highest priority *)
  (* split comes after *)
  (* As long as we explode, keep trying to explode from the start *)
  (* Split if we didn't explode anything *)
  let reduce p =
    let rec aux p =
      let p, changed = explode p in
      if changed then aux p
      else
        let p, changed = split p in
        if changed then aux p else p
    in
    aux p

  (*  *)
  let add t1 t2 =
    reduce (Pair { depth = 0; left = incr_depth t1; right = incr_depth t2 })

  let of_string s = fst (pair s ~offset:1 ~depth:0)

  let rec magnitude = function
    | Int i -> i
    | Pair { left; right; _ } -> (3 * magnitude left) + (2 * magnitude right)
end

let part_1 file =
  Parse.fold_lines
    (fun (first, acc) s ->
      let n = Number.of_string s in
      if first then (false, n) else (first, Number.add acc n))
    (true, Int 0) file
  |> fun (_, n) ->
  Format.printf "@[<v 1>%a@.%d@." Number.(pp ()) n Number.(magnitude n)

let part_2 file =
  let l = Parse.fold_lines (fun acc s -> Number.of_string s :: acc) [] file in
  let rec aux acc = function
    | n1 :: tl ->
        aux
          (List.fold_left
             (fun acc n2 ->
               let n1n2 = Number.(add n1 n2 |> magnitude) in
               let n2n1 = Number.(add n2 n1 |> magnitude) in
               max (max n1n2 n2n1) acc)
             acc tl)
          tl
    | [] -> acc
  in
  aux 0 l |> Format.printf "%d@."

let run part file =
  match part with 1 -> part_1 file | 2 -> part_2 file | _ -> ()
