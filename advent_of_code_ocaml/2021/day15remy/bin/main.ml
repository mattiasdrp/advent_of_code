let foldf f init file =
  let ic = open_in file in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> acc
    | line -> loop (f acc line)
  in
  let r = loop init in
  close_in ic;
  r

let parse_matrix file =
  foldf
    (fun acc line ->
      let line =
        String.to_seq line
        |> Seq.fold_left (fun acc c -> (Char.code c - 48) :: acc) []
        |> List.rev |> Array.of_list
      in
      line :: acc)
    [] file
  |> List.rev |> Array.of_list

let visit_4neighbours acc board (x, y) f =
  let visit f x y acc =
    match board.(y).(x) with
    | exception Invalid_argument _ -> acc
    | v -> f acc (x, y) v
  in
  visit f (x + 1) y acc
  |> visit f (x - 1) y
  |> visit f x (y - 1)
  |> visit f x (y + 1)

module SortedPoint = struct
  type t = (int * int) * ((int * int) * int)

  let compare (_, (_, w1)) (_, (_, w2)) = compare w1 w2
end

module Heap = Batheap.Make (SortedPoint)

let rec sorted_insert e f l =
  match l with
  | [] -> [ e ]
  | h :: t when f e h < 0 -> e :: h :: t
  | h :: t when f e h > 0 -> h :: sorted_insert e f t
  | h :: t -> e :: h :: t

let shortest_path grid start dest =
  let h = Hashtbl.create 17 in
  let rec aux heap =
    match Heap.find_min heap with
    | exception Invalid_argument _ -> ()
    | spot, (_prev, cum_weight) ->
        let heap = Heap.del_min heap in
        let heap =
          visit_4neighbours heap grid spot (fun acc nbr weight ->
              let cum_weight = weight + cum_weight in
              match Hashtbl.find h nbr with
              | exception Not_found ->
                  Hashtbl.replace h nbr (spot, cum_weight);
                  Heap.insert acc (nbr, (spot, cum_weight))
              | _, nbr_cum_weight when cum_weight < nbr_cum_weight ->
                  Hashtbl.replace h nbr (spot, cum_weight);
                  Heap.insert acc (nbr, (spot, cum_weight))
              | _ -> acc)
        in

        aux heap
  in
  aux (Heap.insert Heap.empty (start, (start, 0)));
  Hashtbl.find h dest |> snd

let unfold_grid grid =
  Array.init
    (Array.length grid * 5)
    (fun y ->
      Array.init
        (Array.length grid.(0) * 5)
        (fun x ->
          let mody = y mod Array.length grid in
          let multy = (y - mody) / Array.length grid in
          let modx = x mod Array.length grid.(0) in
          let multx = (x - modx) / Array.length grid.(0) in
          let r = grid.(mody).(modx) + multy + multx in
          if r mod 9 = 0 then 9 else r mod 9))

let run () =
  let grid = parse_matrix "day15.txt" in
  let grid = unfold_grid grid in
  Format.printf "%d@\n"
    (shortest_path grid (0, 0)
       (Array.length grid - 1, Array.length grid.(0) - 1))

let () = run ()
