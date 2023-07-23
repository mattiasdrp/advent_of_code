open Mdrp_lib

let parse_line line =
  let linebuf = Lexing.from_string line in
  Day21_parser.main Day21_lexer.token linebuf

(** Each allergen is found in exactly one ingredient
      This means that if we have [a b contains dairy] and [a c contains dairy]
      [b] or [c] can't contain [dairy] because this will make the other sentence
      false since [a] can't contain [dairy]
      Each time we meet a list of ingredients and allergens we update the set
      of already existing ingredients for each allergen by intersecting it with
      the new ingredient set. *)
let parse file =
  Parse.fold_lines
    (fun (ingredients_list, map) line ->
      let ingredients, allergens = parse_line line in
      ( List.rev_append ingredients ingredients_list,
        List.fold_left
          (fun map allergen ->
            String.Map.update allergen
              (function
                | Some s ->
                    Some (String.Set.inter (String.Set.of_list ingredients) s)
                | None -> Some (String.Set.of_list ingredients))
              map)
          map allergens ))
    ([], String.Map.empty) file

let part_1 file =
  let ingredients_list, map = parse file in
  let allergen_ingredients =
    String.Map.fold
      (fun _ ingredients allergen_ingredients ->
        String.Set.union ingredients allergen_ingredients)
      map String.Set.empty
  in
  List.fold_left
    (fun acc e ->
      if String.Set.mem e allergen_ingredients then acc else acc + 1)
    0 ingredients_list

module UniqueSet = Set.Make (struct
  type t = string * string

  let compare (a1, _) (a2, _) = String.compare a1 a2
  let pp ppf (_, i) = String.pp ppf i
end)

(** It may be interesting to look at https://en.wikipedia.org/wiki/Hopcroft%E2%80%93Karp_algorithm
    But I started with the hypothesis that at each step there would be at least one allergen
    with only one corresponding food which leads to a straight-forward algorithm. *)
let part_2 file =
  let _, map = parse file in
  let rec aux map unique =
    let unique, marked_ingredients, map =
      String.Map.fold
        (fun all ingr_set (unique, marked_ingredients, map) ->
          if String.Set.cardinal ingr_set = 1 then
            let ingr = String.Set.choose ingr_set in
            ( UniqueSet.add (all, ingr) unique,
              String.Set.add ingr marked_ingredients,
              map )
          else (unique, marked_ingredients, String.Map.add all ingr_set map))
        map
        (unique, String.Set.empty, String.Map.empty)
    in
    if String.Set.is_empty marked_ingredients then unique
    else
      let map =
        String.Map.filter_map
          (fun _all ingr_set ->
            Some (String.Set.diff ingr_set marked_ingredients))
          map
      in
      aux map unique
  in
  let unique = aux map UniqueSet.empty in
  Format.printf "%a@."
    UniqueSet.(
      pp ~pp_sep:(fun ppf () -> Format.fprintf ppf ",") ~left:"" ~right:"" ())
    unique;
  0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
