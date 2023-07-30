open Mdrp_lib

module Ingredient = struct
  type t = {
    name : string;
    capacity : int;
    durability : int;
    flavor : int;
    texture : int;
    calories : int;
  }

  let pp ppf t = Format.fprintf ppf "%s" t.name
  let compare t1 t2 = String.compare t1.name t2.name
end

module ISet = Set.Make (Ingredient)

let parse (set, nb_ingredients) line =
  let re =
    Str.regexp
      {|\([A-Za-z]+\): capacity \(-?[0-9]+\), durability \(-?[0-9]+\), flavor \(-?[0-9]+\), texture \(-?[0-9]+\), calories \(-?[0-9]+\)|}
  in
  if Str.string_match re line 0 then
    let name = Str.matched_group 1 line in
    let capacity = Str.matched_group 2 line |> int_of_string in
    let durability = Str.matched_group 3 line |> int_of_string in
    let flavor = Str.matched_group 4 line |> int_of_string in
    let texture = Str.matched_group 5 line |> int_of_string in
    let calories = Str.matched_group 6 line |> int_of_string in
    ( ISet.add
        Ingredient.{ name; capacity; durability; flavor; texture; calories }
        set,
      nb_ingredients + 1 )
  else assert false

let min_0 v = min 0 v

let best_cookie p compositions ingredients =
  let rec aux acc = function
    | [] -> acc
    | c :: tl ->
        let composition = List.combine c ingredients in
        let cookie =
          Ingredient.
            {
              name = "cookie";
              capacity = 0;
              durability = 0;
              flavor = 0;
              texture = 0;
              calories = 0;
            }
        in

        let cookie =
          List.fold_left
            (fun cookie (nb, i) ->
              Ingredient.
                {
                  cookie with
                  capacity = cookie.capacity + (nb * i.capacity);
                  durability = cookie.durability + (nb * i.durability);
                  flavor = cookie.flavor + (nb * i.flavor);
                  texture = cookie.texture + (nb * i.texture);
                  calories = cookie.calories + (nb * i.calories);
                })
            cookie composition
        in
        let score =
          if p cookie then
            max 0 cookie.capacity * max 0 cookie.durability
            * max 0 cookie.flavor * max 0 cookie.texture
          else 0
        in
        aux (if score > acc then score else acc) tl
  in
  aux 0 compositions

let part_1 file =
  let set, nb_ingredients = Parse.fold_lines parse (ISet.empty, 0) file in
  let ingredients = ISet.to_list set in
  let compositions =
    Int.Decimal.Seq.restricted_weak_composition ~n:100 ~k:nb_ingredients
      ~min_value:0 ~max_value:100
  in
  best_cookie (fun _ -> true) compositions ingredients

let part_2 file =
  let set, nb_ingredients = Parse.fold_lines parse (ISet.empty, 0) file in
  let ingredients = ISet.to_list set in
  let compositions =
    Int.Decimal.Seq.restricted_weak_composition ~n:100 ~k:nb_ingredients
      ~min_value:0 ~max_value:100
  in
  best_cookie (fun t -> t.calories = 500) compositions ingredients

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
