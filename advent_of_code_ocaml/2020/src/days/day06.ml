open Mdrp_lib

let part_1 file =
  let set, acc =
    Parse.fold_lines
      (fun (set, acc) line ->
        match line with
        | "" -> (Char.Set.empty, acc + Char.Set.cardinal set)
        | s -> (String.fold (fun set c -> Char.Set.add c set) set s, acc))
      (Char.Set.empty, 0) file
  in
  acc + Char.Set.cardinal set

let part_2 file =
  let _, set, acc =
    Parse.fold_lines
      (fun (first, set, acc) line ->
        match line with
        | "" -> (true, Char.Set.empty, acc + Char.Set.cardinal set)
        | s ->
            let set =
              if first then Char.Set.of_list (String.to_list s)
              else Char.Set.inter set (Char.Set.of_list (String.to_list s))
            in
            (false, set, acc))
      (true, Char.Set.empty, 0) file
  in
  acc + Char.Set.cardinal set

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
