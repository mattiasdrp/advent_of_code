open Mdrp_lib
open Array

let check_neighbours a i j v =
  (match a.?(i - 1) with None -> true | Some a' -> a'.(j) > v)
  && (match a.?(i + 1) with None -> true | Some a' -> a'.(j) > v)
  && (match a.(i).?(j - 1) with None -> true | Some v' -> v' > v)
  && match a.(i).?(j + 1) with None -> true | Some v' -> v' > v

let part_1 file =
  let a =
    Parse.fold_lines
      (fun acc s -> String.to_array Char.to_digit s :: acc)
      [] file
    |> List.rev |> Array.of_list
  in
  Array.fold_lefti
    (fun i acc a' ->
      Array.fold_lefti
        (fun j acc v -> if check_neighbours a i j v then acc + v + 1 else acc)
        acc a')
    0 a
  |> Format.printf "%d@."

let cpt =
  let cpt = ref 0 in
  fun () ->
    incr cpt;
    !cpt

type cell = { value : int; basin : int }

let pp_cell ppf { value; basin } = Format.fprintf ppf "{%d; %d}" value basin

module IM = Int.Map

let rec mark a i j b acc =
  match a.?(i) with
  | None -> acc
  | Some a' -> (
      match a'.?(j) with
      | None -> acc
      | Some { value; basin } ->
          if basin > 0 || value = 9 then acc
          else (
            (* Mark this cell as belonging to the current basin and explore *)
            (* all its neighbours. We stop when we meet a 9 or an already marked *)
            (* neighbour (that should belong to the same basin) *)
            a.(i).(j) <- { value; basin = b };
            IM.update b (function None -> Some 1 | Some c -> Some (c + 1)) acc
            |> mark a (i - 1) j b
            |> mark a i (j - 1) b
            |> mark a (i + 1) j b
            |> mark a i (j + 1) b))

let part_2 file =
  let a =
    Parse.fold_lines
      (fun acc s ->
        String.to_array (fun c -> { value = Char.to_digit c; basin = 0 }) s
        :: acc)
      [] file
    |> List.rev |> Array.of_list
  in
  let im =
    Array.fold_lefti
      (fun i acc a' ->
        Array.fold_lefti
          (fun j acc { value; basin } ->
            (* As soon as we meet an unmarked non 9 cell, we're in a basin and *)
            (* mark all other cells belonging to it *)
            if value < 9 && basin = 0 then mark a i j (cpt ()) acc else acc)
          acc a')
      IM.empty a
  in
  IM.fold (fun _ size acc -> size :: acc) im []
  |> List.fast_sort Int.compare_dec
  |> function
  | hd1 :: hd2 :: hd3 :: _ -> Format.printf "%d@." (hd1 * hd2 * hd3)
  | _ -> assert false

let run part file =
  match part with 1 -> part_1 file | 2 -> part_2 file | _ -> ()
