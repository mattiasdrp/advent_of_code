open Mdrp_lib

type part1 = { hor : int; depth : int }

let part_1 file =
  let { hor; depth } =
    Parse.fold_lines
      (fun ({ hor; depth } as acc) line ->
        match String.split_on_char ' ' line with
        | [ dir; value ] -> (
            let value = int_of_string value in
            match dir with
            | "forward" -> { acc with hor = hor + value }
            | "up" -> { acc with depth = depth - value }
            | "down" -> { acc with depth = depth + value }
            | _ -> assert false)
        | _ -> assert false)
      { hor = 0; depth = 0 } file
  in
  hor * depth

type part2 = { hor : int; depth : int; aim : int }

let part_2 file =
  let { hor; depth; _ } =
    Parse.fold_lines
      (fun ({ hor; depth; aim } as acc) line ->
        match String.split_on_char ' ' line with
        | [ dir; value ] -> (
            let value = int_of_string value in
            match dir with
            | "forward" ->
                { acc with hor = hor + value; depth = depth + (aim * value) }
            | "up" -> { acc with aim = aim - value }
            | "down" -> { acc with aim = aim + value }
            | _ -> assert false)
        | _ -> assert false)
      { hor = 0; depth = 0; aim = 0 }
      file
  in
  hor * depth

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
