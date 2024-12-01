open Mdrp_lib

let rec traverse = function
  | v1 :: (v2 :: _ as tl) ->
      let delta = v2 - v1 in
      delta >= 1 && delta <= 3 && traverse tl
  | [ _ ] | [] -> true

let traverse_dampened ints =
  let rec aux_traverse prev_value = function
    | v1 :: v2 :: tl -> (
        let delta = v2 - v1 in
        (* No error, keep traversing *)
        (delta >= 1 && delta <= 3 && aux_traverse (Some v1) (v2 :: tl))
        (* Error encountered, remove v1 or v2
           Reuse traverse since we already removed a value,
           this is like solving part 1 *)
        || traverse (v1 :: tl)
        ||
        match prev_value with
        | None -> traverse (v2 :: tl)
        | Some v -> traverse (v :: v2 :: tl))
    | [ _ ] | [] -> true
  in
  aux_traverse None ints

let common_part file part =
  let traverse = if part = 1 then traverse else traverse_dampened in
  Mdrp_lib.Parse.fold_lines
    (fun safe line ->
      let ints = String.split_on_char ' ' line |> List.map int_of_string in
      if traverse ints || traverse (List.rev ints) then safe + 1 else safe)
    0 file

let run part file = common_part file part
