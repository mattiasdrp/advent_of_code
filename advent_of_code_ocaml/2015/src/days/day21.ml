open Mdrp_lib

module Item = struct
  (* Weapons:    Cost  Damage  Armor
     Dagger        8     4       0
     Shortsword   10     5       0
     Warhammer    25     6       0
     Longsword    40     7       0
     Greataxe     74     8       0

     Armor:      Cost  Damage  Armor
     Leather      13     0       1
     Chainmail    31     0       2
     Splintmail   53     0       3
     Bandedmail   75     0       4
     Platemail   102     0       5

     Rings:      Cost  Damage  Armor
     Damage +1    25     1       0
     Damage +2    50     2       0
     Damage +3   100     3       0
     Defense +1   20     0       1
     Defense +2   40     0       2
          Defense +3   80     0       3 *)

  type t = { name : string; cost : int; damage : int; armor : int }

  let of_tuple (name, cost, damage, armor) = { name; cost; damage; armor }
  let compare t1 t2 = String.compare t1 t2

  let pp ppf t =
    Format.fprintf ppf "{name: %s; cost: %d; damage: %d; armor: %d}" t.name
      t.cost t.damage t.armor
end

module Player = struct
  type t = { hp : int; damage : int; armor : int; items_cost : int }

  let pp ppf t =
    Format.fprintf ppf "{hp:%d; damage: %d; armor: %d; items: %d}" t.hp t.damage
      t.armor t.items_cost

  let of_values hp damage armor = { hp; damage; armor; items_cost = 0 }

  let apply_item t i =
    {
      t with
      damage = t.damage + i.Item.damage;
      armor = t.armor + i.Item.armor;
      items_cost = t.items_cost + i.Item.cost;
    }

  let apply_items t l = List.fold_left apply_item t l

  let time_to_beat boss player =
    let dmg_boss_player = max 1 (boss.damage - player.armor) in
    let time_boss_player =
      int_of_float @@ ceil @@ (float player.hp /. float dmg_boss_player)
    in
    let dmg_player_boss = max 1 (player.damage - boss.armor) in
    let time_player_boss =
      int_of_float @@ ceil @@ (float boss.hp /. float dmg_player_boss)
    in
    (time_boss_player, time_player_boss)

  let score boss player =
    let tboss, tplayer = time_to_beat boss player in
    if tplayer <= tboss then player.items_cost else max_int

  let score_to_lose boss player =
    let tboss, tplayer = time_to_beat boss player in
    if tplayer > tboss then player.items_cost else 0
end

let weapons =
  List.map Item.of_tuple
    [
      ("Dagger", 8, 4, 0);
      ("Shortsword", 10, 5, 0);
      ("Warhammer", 25, 6, 0);
      ("Longsword", 40, 7, 0);
      ("Greataxe", 74, 8, 0);
    ]

let armors =
  List.map Item.of_tuple
    [
      ("Leather", 13, 0, 1);
      ("Chainmail", 31, 0, 2);
      ("Splintmail", 53, 0, 3);
      ("Bandedmail", 75, 0, 4);
      ("Platemail", 102, 0, 5);
    ]

let rings =
  List.map Item.of_tuple
    [
      ("Damage +1", 25, 1, 0);
      ("Damage +2", 50, 2, 0);
      ("Damage +3", 100, 3, 0);
      ("Defense +1", 20, 0, 1);
      ("Defense +2", 40, 0, 2);
      ("Defense +3", 80, 0, 3);
    ]

let two_rings = Seq.subsets 2 rings

let itemize_player score compare init_score ~boss ~player ~weapons ~armors
    ~rings =
  let score = score boss in
  let rec aux compare_score weapons sarmors srings =
    match (weapons, sarmors, srings) with
    | [], _, _ -> compare_score
    | w :: wl, [], _ ->
        let compare_score =
          Seq.fold_left
            (fun compare_score l ->
              compare compare_score (Player.apply_items player (w :: l) |> score))
            compare_score two_rings
        in
        aux compare_score wl armors rings
    | w :: _, a :: al, [] ->
        let compare_score =
          Seq.fold_left
            (fun compare_score l ->
              compare compare_score
                (Player.apply_items player (w :: a :: l) |> score))
            compare_score two_rings
        in
        aux compare_score weapons al rings
    | w :: _, a :: _, r :: rl ->
        let compare_score =
          compare compare_score (Player.apply_items player [ w ] |> score)
          |> compare (Player.apply_items player [ w; r ] |> score)
          |> compare (Player.apply_items player [ w; a ] |> score)
          |> compare (Player.apply_items player [ w; a; r ] |> score)
        in
        aux compare_score weapons sarmors rl
  in

  aux init_score weapons armors rings

let parse ci =
  match String.split_on_char ':' (input_line ci) with
  | [ _; v ] -> int_of_string (String.sub v 1 (String.length v - 1))
  | _ -> assert false

let boss_player file =
  let ci = open_in file in
  let hp = parse ci in
  let damage = parse ci in
  let armor = parse ci in
  close_in ci;
  let boss = Player.of_values hp damage armor in
  let player = Player.of_values 100 0 0 in
  (boss, player)

let part_1 file =
  let boss, player = boss_player file in
  itemize_player Player.score min max_int ~boss ~player ~weapons ~armors ~rings

let part_2 file =
  let boss, player = boss_player file in
  itemize_player Player.score_to_lose max 0 ~boss ~player ~weapons ~armors
    ~rings

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
