open Mdrp_lib

let parse file =
  let player2, player1 =
    Parse.fold_lines
      (fun ((curr_player, other_player) as acc) line ->
        if line = "" then acc
        else
          match int_of_string line with
          | i -> (i :: curr_player, other_player)
          | exception Failure _ -> (other_player, curr_player))
      ([], []) file
  in
  (List.rev player1, List.rev player2)

let score l =
  List.fold_right (fun i (res, mul) -> ((i * mul) + res, mul + 1)) l (0, 1)
  |> fst

let play player1 player2 =
  let rec aux player1 player2 =
    match (player1, player2) with
    | [], l | l, [] -> score l
    | c1 :: tl1, c2 :: tl2 when c1 > c2 -> aux (tl1 @ [ c1; c2 ]) tl2
    | c1 :: tl1, c2 :: tl2 -> aux tl1 (tl2 @ [ c2; c1 ])
  in
  aux player1 player2

let part_1 file =
  let player1, player2 = parse file in
  play player1 player2

module Configurations = struct
  module LSet = Set.Make (Pair.Make (Int.List) (Int.List))

  (* let hashtbl = Hashtbl.create 19 *)
  let empty = LSet.empty
  let add player1 player2 t = LSet.add (player1, player2) t
  let already_seen player1 player2 t = LSet.mem (player1, player2) t
end

let recursive_play player1 player2 =
  let rec game player1 player2 configurations sub =
    if
      (* If we're in a subgame and player 1 has the highest card
         and the value of this card is higher than the length of both decks
         then player 1 will win this subgame. *)
      sub
      &&
      let max1 = List.fold_left max 0 player1 in
      let max2 = List.fold_left max 0 player2 in
      max1 > max2 && max1 >= List.length player1 + List.length player2 - 2
    then (true, 0)
    else
      match (player1, player2) with
      | l, [] -> (true, score l)
      | [], l -> (false, score l)
      | c1 :: tl1, c2 :: tl2 ->
          if Configurations.already_seen player1 player2 configurations then
            (true, 0)
          else if c1 <= List.length tl1 && c2 <= List.length tl2 then
            match
              (* The rule was written but sometimes you forget to read:
                 "To play a sub-game of Recursive Combat, each player creates a new deck
                 by making a copy of the next cards in their deck (the quantity of cards
                 copied is equal to the number on the card they drew to trigger the sub-game)" *)
              game (List.take c1 tl1) (List.take c2 tl2) Configurations.empty
                true
            with
            | true, _ ->
                game
                  (tl1 @ [ c1; c2 ])
                  tl2
                  (Configurations.add player1 player2 configurations)
                  sub
            | false, _ ->
                game tl1
                  (tl2 @ [ c2; c1 ])
                  (Configurations.add player1 player2 configurations)
                  sub
          else if c1 > c2 then
            game
              (tl1 @ [ c1; c2 ])
              tl2
              (Configurations.add player1 player2 configurations)
              sub
          else
            game tl1
              (tl2 @ [ c2; c1 ])
              (Configurations.add player1 player2 configurations)
              sub
  in
  game player1 player2 Configurations.empty false |> snd

let part_2 file =
  let player1, player2 = parse file in
  recursive_play player1 player2

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
