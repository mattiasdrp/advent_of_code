open Mdrp_lib

let part_1 file =
  let llist, rlist =
    Parse.fold_lines
      (fun (llist, rlist) line ->
        match String.split_on_char_non_empty ' ' line with
        | [ left; right ] ->
            (int_of_string left :: llist, int_of_string right :: rlist)
        | _ -> assert false)
      ([], []) file
  in
  let llist = List.fast_sort Int.compare llist in
  let rlist = List.fast_sort Int.compare rlist in
  List.fold_left2
    (fun acc left right -> acc + max left right - min left right)
    0 llist rlist

let part_2 file =
  let incr = function Some i -> Some (i + 1) | None -> Some 1 in
  let llist, rmap =
    Parse.fold_lines
      (fun (llist, rmap) line ->
        match String.split_on_char_non_empty ' ' line with
        | [ left; right ] ->
            ( int_of_string left :: llist,
              Int.Map.update (int_of_string right) incr rmap )
        | _ -> assert false)
      ([], Int.Map.empty) file
  in
  List.fold_left
    (fun acc key ->
      let count = Option.value (Int.Map.find_opt key rmap) ~default:0 in
      acc + (key * count))
    0 llist

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
