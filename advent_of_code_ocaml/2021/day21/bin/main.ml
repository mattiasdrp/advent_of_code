open Mdrp_lib

module Player = struct
  type t = { score : int; position : int }

  let pp ppf { score; position } =
    Format.fprintf ppf "{score: %d; position: %d}" score position

  let move player dice =
    let position = ((player.position - 1 + dice) mod 10) + 1 in
    { score = player.score + position; position }
end

let dice_100 =
  let v = ref 0 in
  let round v = if v > 100 then v - 100 else v in
  fun () ->
    let v1 = round (!v + 1) in
    let v2 = round (!v + 2) in
    let v3 = round (!v + 3) in
    let res = v1 + v2 + v3 in
    v := v3;
    res

let part_1 p1 p2 =
  let rec aux rounds (p1, p2) =
    let p1 = Player.move p1 (dice_100 ()) in
    if p1.score >= 1000 then (p1.score, p2.Player.score, rounds)
    else aux (rounds + 1) (p2, p1)
  in
  let s1, s2, rounds = aux 1 (p1, p2) in
  Format.printf "%d, %d, %d, %d@." s1 s2 rounds (s2 * rounds * 3)

let moves =
  let move dice ((p1, p2), from) card =
    ((p2, Player.move p1 dice), from * card)
  in
  fun l pp ->
    move 3 pp 1 :: move 4 pp 3 :: move 5 pp 6 :: move 6 pp 7 :: move 7 pp 6
    :: move 8 pp 3 :: move 9 pp 1 :: l

let part_2 p1 p2 =
  let rec aux (acc, p1win, p2win) swap =
    if acc = [] then Format.printf "%d@." (max p1win p2win)
    else
      (* This is the part where I should memoise but DotA is calling me *)
      let acc = List.fold_left moves [] acc in
      let p1win, acc =
        List.fold_left
          (fun (p1w, acc) (((_, { Player.score; _ }), from) as pp) ->
            if score >= 21 then (p1w + from, acc) else (p1w, pp :: acc))
          (p1win, []) acc
      in
      aux (acc, p2win, p1win) (not swap)
  in
  aux ([ ((p1, p2), 1) ], 0, 0) false

let position ci =
  match String.split_on_char ':' (input_line ci) with
  | [ _; p ] -> int_of_string (String.sub p 1 (String.length p - 1))
  | _ -> assert false

let () =
  let part = try Sys.argv.(1) with _ -> "2" in
  let file = try Sys.argv.(2) with _ -> "input" in
  let ci = open_in file in
  let player1 = Player.{ score = 0; position = position ci } in
  let player2 = Player.{ score = 0; position = position ci } in
  match part with
  | "1" -> part_1 player1 player2
  | "2" -> part_2 player1 player2
  | _ -> ()
