open Mdrp_lib

module Mode = struct
  type t = File | FreeSpace
end

module Content = struct
  type t = File of { quantity : int; index : int } | FreeSpace of int
end

let parse line =
  let max_index = String.length line / 2 in
  let list, map, _, _ =
    String.fold_right
      (fun c (acc, map, mode, index) ->
        let quantity = Char.to_digit c in
        match mode with
        | Mode.File ->
            let acc, map =
              if quantity = 0 then (acc, map)
              else
                ( Content.File { quantity; index } :: acc,
                  Int.Map.add index quantity map )
            in

            (acc, map, FreeSpace, index - 1)
        | FreeSpace ->
            let acc =
              if quantity = 0 then acc else Content.FreeSpace quantity :: acc
            in
            (acc, map, File, index))
      line
      ( [],
        Int.Map.empty,
        (if String.length line mod 2 = 0 then FreeSpace else File),
        max_index )
  in
  (list, map, max_index)

let sum n = n * (n - 1) / 2

let fill list map index =
  let rec aux list map next_index new_list =
    if Int.Map.is_empty map then
      (* We have no more elements to add, that's our first fix point *)
      List.rev new_list
    else
      match list with
      | Content.FreeSpace qty_free :: tlist ->
          (* If we have free space: *)
          let quantity = Int.Map.find next_index map in
          (* Get the quantity that needs to be placed *)
          let list, map, index, new_list =
            if quantity <= qty_free then
              (* If we have enough free space quantity *)
              let new_list =
                Content.File { quantity; index = next_index } :: new_list
                (* Add the whole content to the free space *)
              in
              let list =
                if quantity = qty_free then tlist
                else
                  (* If the free space quantity was more than needed,
                     add the rest back to not lose it *)
                  Content.FreeSpace (qty_free - quantity) :: tlist
              in
              let map =
                (* In any case, this index is now completely placed,
                   remove it from the map *)
                Int.Map.remove next_index map
              in
              (list, map, next_index - 1, new_list)
            else
              (* quantity > qty_free *)
              let new_list =
                (* Add the amount that fits *)
                Content.File { quantity = qty_free; index = next_index }
                :: new_list
              in
              let map =
                (* Don't remove the index, just add back the quantity
                   that still needs to be added to some free space *)
                Int.Map.add next_index (quantity - qty_free) map
              in
              (tlist, map, next_index, new_list)
          in
          aux list map index new_list
      | (Content.File { index; quantity } as content) :: tlist -> (
          (* If we have a file content, just keep it there and remove
             it from the map *)
          match Int.Map.find_opt index map with
          | Some q when q < quantity ->
              (* Special end case where the map is not empty but will contain
                 the current file content but with less quantity, this means that
                 we reached our second fix point and just add what remains in
                 the map *)
              List.rev (Content.File { index; quantity = q } :: new_list)
          | _ ->
              aux tlist (Int.Map.remove index map) next_index
                (content :: new_list))
      | [] ->
          (* Third fix point, should always never happen (only happens when
             there's no free space to start with) *)
          List.rev new_list
  in
  aux list map index []

let place (index, quantity) list =
  let rec aux prev = function
    | Content.File { index = index'; _ } :: _ as rest when index' = index ->
        (* We could return [list] but let's not use compressed free spaces
           along the way *)
        List.rev_append prev rest
    | Content.FreeSpace q1 :: Content.FreeSpace q2 :: tl ->
        (* Compress two free spaces into one if they are contiguous *)
        aux prev (Content.FreeSpace (q1 + q2) :: tl)
    | Content.FreeSpace quantity' :: tl when quantity' >= quantity ->
        (* We have enough free space to fit the current quantity *)
        let tl =
          if quantity = quantity' then tl
          else
            (* If the free space quantity is more than needed, don't lose
               the rest *)
            Content.FreeSpace (quantity' - quantity) :: tl
        in
        (* Replace the previous place occupied by free space of the
           same quantity *)
        let tl =
          List.map
            (function
              | Content.File { index = index'; _ } when index' = index ->
                  Content.FreeSpace quantity
              | e -> e)
            tl
        in
        List.rev_append prev (Content.File { index; quantity } :: tl)
    | e :: tl -> aux (e :: prev) tl
    | [] -> assert false
  in
  aux [] list

let fill_whole list map max_index =
  let rec aux list map next_index =
    if Int.Map.is_empty map then list
    else
      let quantity = Int.Map.find next_index map in
      aux
        (place (next_index, quantity) list)
        (Int.Map.remove next_index map)
        (next_index - 1)
  in
  aux list map max_index

let score list =
  let rec aux acc i = function
    | [] -> acc
    | Content.File { index; quantity } :: tl ->
        aux (acc + (index * ((quantity * i) + sum quantity))) (i + quantity) tl
    | Content.FreeSpace quantity :: tl -> aux acc (i + quantity) tl
  in
  aux 0 0 list

let common_part part file =
  let line =
    let input = open_in file in
    let line = input_line input in
    close_in input;
    line
  in
  let disk_map, map, max_index = parse line in
  let compressor = if part = 1 then fill else fill_whole in
  compressor disk_map map max_index |> score

let run part file = common_part part file
