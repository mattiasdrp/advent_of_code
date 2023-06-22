open Mdrp_lib

module Mask = struct
  let empty = []

  let of_string s =
    String.foldi
      (fun i acc -> function
        | 'X' -> acc
        | c when c = '0' || c = '1' -> (i, c) :: acc
        | _ -> assert false)
      [] s

  let apply t s =
    let b = Int.Binary.to_bytes s in
    List.iter (fun (i, c) -> Bytes.set b i c) t;
    Int.Binary.of_bytes b
end

let part_1 file =
  let ci = open_in file in
  let re_mask = Str.regexp {|mask = \([X01]+\)|} in
  let re_mem = Str.regexp {|mem\[\([0-9]+\)\] = \([0-9]+\)|} in
  let rec aux_parse mask acc =
    match input_line ci with
    | s ->
        if Str.string_match re_mask s 0 then
          aux_parse (Str.matched_group 1 s |> Mask.of_string) acc
        else (
          assert (Str.string_match re_mem s 0);

          aux_parse mask
            (Int.Map.add
               (Str.matched_group 1 s |> int_of_string)
               (Str.matched_group 2 s |> int_of_string
               |> Int.Binary.of_dec ~extend:36
               |> Mask.apply mask)
               acc))
    | exception End_of_file ->
        Int.Map.fold (fun _ v acc -> acc + Int.Binary.to_dec v) acc 0
  in
  aux_parse Mask.empty Int.Map.empty |> Format.printf "%d@."

module Mask2 = struct
  type action = Overwrite | Split

  let empty = []

  type t = (int * action) list

  let of_string s =
    String.foldi
      (fun i acc -> function
        | 'X' -> (i, Split) :: acc
        | '1' -> (i, Overwrite) :: acc
        | _ -> acc)
      [] s

  let apply (action_list : t) s =
    let b = Int.Binary.to_bytes s in
    let rec aux acc = function
      | [] -> acc
      | (i, Overwrite) :: tl ->
          List.iter (fun b -> Bytes.set b i '1') acc;
          aux acc tl
      | (i, Split) :: tl ->
          aux
            (List.fold_left
               (fun acc b ->
                 let b' = Bytes.copy b in
                 Bytes.set b i '0';
                 Bytes.set b' i '1';
                 b :: b' :: acc)
               [] acc)
            tl
    in
    aux [ b ] action_list |> List.map Int.Binary.of_bytes
end

let part_2 file =
  let ci = open_in file in
  let re_mask = Str.regexp {|mask = \([X01]+\)|} in
  let re_mem = Str.regexp {|mem\[\([0-9]+\)\] = \([0-9]+\)|} in
  let rec aux_parse mask acc =
    match input_line ci with
    | s ->
        if Str.string_match re_mask s 0 then
          aux_parse (Str.matched_group 1 s |> Mask2.of_string) acc
        else (
          assert (Str.string_match re_mem s 0);
          let v = Str.matched_group 2 s |> int_of_string in
          Str.matched_group 1 s |> int_of_string
          |> Int.Binary.of_dec ~extend:36
          |> Mask2.apply mask
          |> List.fold_left
               (fun acc k -> Int.Map.add (Int.Decimal.of_bin k) v acc)
               acc
          |> aux_parse mask)
    | exception End_of_file ->
        Int.Map.fold
          (fun _k v acc ->
            (* Format.eprintf "%d %d@." k v; *)
            acc + v)
          acc 0
  in
  aux_parse Mask2.empty Int.Map.empty |> Format.printf "%d@."

let run part file =
  match part with 1 -> part_1 file | 2 -> part_2 file | _ -> ()
