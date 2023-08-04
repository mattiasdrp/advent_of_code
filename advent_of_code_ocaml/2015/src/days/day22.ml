module Spell = struct
  (*
    Magic Missile costs 53 mana. It instantly does 4 damage.
    Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
    Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7.
    Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn while it is active, it deals the boss 3 damage.
    Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn while it is active, it gives you 101 new mana.
 *)
  type effect = { value : int; timer : int }

  let pp_effect ppf t = Format.fprintf ppf "%d during %d" t.value t.timer

  type spell =
    | MagicMissile of int
    | Drain of int
    | Shield of effect
    | Poison of effect
    | Recharge of effect

  type t = { cost : int; spell : spell }

  let pp ppf t =
    (match t.spell with
    | MagicMissile d -> Format.fprintf ppf "MagicMissile %d" d
    | Drain d -> Format.fprintf ppf "Drain %d" d
    | Shield e -> Format.fprintf ppf "Shield %a" pp_effect e
    | Poison e -> Format.fprintf ppf "Poison %a" pp_effect e
    | Recharge e -> Format.fprintf ppf "Recharge %a" pp_effect e);
    Format.fprintf ppf " cost: %d" t.cost

  let magic_missile = { cost = 53; spell = MagicMissile 4 }
  let drain = { cost = 73; spell = Drain 2 }
  let shield = { cost = 113; spell = Shield { value = 7; timer = 6 } }
  let poison = { cost = 173; spell = Poison { value = 3; timer = 6 } }
  let recharge = { cost = 229; spell = Recharge { value = 101; timer = 5 } }
  let spells = [ magic_missile; drain; shield; poison; recharge ]
  let min_mana = 53
end

module Boss = struct
  type t = { hp : int; damage : int; poison : Spell.effect option }

  let pp ppf t =
    Format.fprintf ppf "{hp: %d; damage %d; poison: %a}" t.hp t.damage
      Mdrp_lib.Option.(pp Spell.pp_effect)
      t.poison

  let of_values hp damage = { hp; damage; poison = None }
end

module Player = struct
  type t = {
    hp : int;
    armor : int;
    mana : int;
    shield : Spell.effect option;
    recharge : Spell.effect option;
  }

  let pp ppf t =
    Format.fprintf ppf "{hp: %d; armor: %d; mana: %d; shield: %a; recharge: %a}"
      t.hp t.armor t.mana
      Mdrp_lib.Option.(pp Spell.pp_effect)
      t.shield
      Mdrp_lib.Option.(pp Spell.pp_effect)
      t.recharge

  let of_values hp mana =
    { hp; mana; armor = 0; shield = None; recharge = None }
end

let update_boss boss =
  match boss.Boss.poison with
  | Some effect ->
      let timer = effect.Spell.timer - 1 in
      {
        boss with
        hp = boss.hp - effect.value;
        poison = (if timer = 0 then None else Some { effect with timer });
      }
  | None -> boss

let update_player part_2 player =
  let player =
    if part_2 then Player.{ player with hp = player.hp - 1 } else player
  in
  let player =
    match player.Player.shield with
    | Some shield ->
        let timer = shield.Spell.timer - 1 in
        let player = { player with armor = shield.value } in
        if timer = 0 then { player with armor = 0; shield = None }
        else { player with shield = Some { shield with timer } }
    | None -> { player with shield = None }
  in
  match player.Player.recharge with
  | Some recharge ->
      let timer = recharge.Spell.timer - 1 in
      let player = { player with mana = player.mana + recharge.value } in
      if timer = 0 then { player with recharge = None }
      else { player with recharge = Some { recharge with timer } }
  | None -> { player with recharge = None }

let damage_player damage player =
  Player.{ player with hp = player.hp - max 1 (damage - player.armor) }

let apply spell boss player =
  if player.Player.mana < spell.Spell.cost then None
  else
    match spell.spell with
    | Spell.MagicMissile d ->
        Some
          ( Boss.{ boss with hp = boss.hp - d },
            { player with mana = player.mana - spell.cost } )
    | Drain d ->
        Some
          ( { boss with hp = boss.hp - d },
            { player with hp = player.hp + d; mana = player.mana - spell.cost }
          )
    | Shield e -> (
        match player.shield with
        | None ->
            Some
              ( boss,
                { player with shield = Some e; mana = player.mana - spell.cost }
              )
        | Some _ -> None)
    | Poison e -> (
        match boss.poison with
        | None ->
            Some
              ( { boss with poison = Some e },
                { player with mana = player.mana - spell.cost } )
        | Some _ -> None)
    | Recharge e -> (
        match player.recharge with
        | None ->
            Some
              ( boss,
                {
                  player with
                  recharge = Some e;
                  mana = player.mana - spell.cost;
                } )
        | Some _ -> None)

let replay part_2 boss player spells =
  let rec aux total boss player = function
    | [] ->
        Format.eprintf "End:@.  Player: %a@.  Boss: %a@.@." Player.pp player
          Boss.pp boss;
        total
    | spell :: spells -> (
        Format.eprintf
          "Player turn:@.  Player: %a@.  Boss: %a@.  Spell: %a@.  Total: %d@.@."
          Player.pp player Boss.pp boss Spell.pp spell total;

        let player = update_player part_2 player in
        let boss = update_boss boss in
        match apply spell boss player with
        | None -> assert false
        | Some (boss, player) ->
            Format.eprintf "Boss turn:@.  Player: %a@.  Boss: %a@.@." Player.pp
              player Boss.pp boss;
            let player = update_player false player in
            let player = damage_player boss.damage player in
            let boss = update_boss boss in

            aux (total + spell.cost) boss player spells)
  in
  aux boss player spells

let dfs part_2 boss player =
  let rec aux min_mana min_spells total_mana spells boss player =
    let player = update_player part_2 player in
    if player.Player.hp <= 0 then (min_mana, min_spells)
    else
      let boss = update_boss boss in
      if boss.Boss.hp <= 0 then
        if min_mana <= total_mana then (min_mana, min_spells)
        else (total_mana, spells)
      else
        List.fold_left
          (fun (min_mana, min_spells) spell ->
            match apply spell boss player with
            | None -> (min_mana, min_spells)
            | Some (boss, player) ->
                let total_mana = total_mana + spell.Spell.cost in
                let spells = spell :: spells in
                if total_mana >= min_mana then (min_mana, min_spells)
                else
                  let boss = update_boss boss in
                  if boss.hp <= 0 then
                    if min_mana <= total_mana then (min_mana, min_spells)
                    else (total_mana, spells)
                  else
                    (* Play boss turn *)
                    let player = update_player false player in
                    let player = damage_player boss.damage player in
                    if player.hp <= 0 then (min_mana, min_spells)
                    else aux min_mana min_spells total_mana spells boss player)
          (min_mana, min_spells) Spell.spells
  in
  aux max_int [] 0 [] boss player

let parse ci =
  match String.split_on_char ':' (input_line ci) with
  | [ _; v ] -> int_of_string (String.sub v 1 (String.length v - 1))
  | _ -> assert false

let boss_player file =
  let ci = open_in file in
  let hp = parse ci in
  let damage = parse ci in
  close_in ci;
  let boss = Boss.of_values hp damage in
  let player = Player.of_values 50 500 in
  (boss, player)

let list = Spell.[ recharge; shield; drain; poison; magic_missile ]

let part_1 file =
  let boss, player = boss_player file in
  let min_mana, _min_spells = dfs false boss player in
  (* Format.eprintf "total: %d@." *)
  (*   (replay false 0 boss player (List.rev min_spells)); *)
  min_mana

let part_2 file =
  let boss, player = boss_player file in
  let min_mana, _min_spells = dfs true boss player in
  (* Format.eprintf "total: %d@." (replay true 0 boss player (List.rev min_spells)); *)
  min_mana

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
