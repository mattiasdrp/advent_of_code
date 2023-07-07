open Mdrp_lib

module Square = struct
  type t = Tree | Open

  let of_char = function '.' -> Open | '#' -> Tree | _ -> assert false

  let pp ppf t =
    Format.fprintf ppf "%c" (match t with Tree -> '#' | Open -> '.')
end

let area file =
  Parse.fold_lines
    (fun acc s -> String.to_array Square.of_char s :: acc)
    [] file
  |> List.rev |> Array.of_list

let down area (ri, dj) =
  let height = Array.length area in
  let width = Array.length area.(0) in
  let rec down (i, j) trees =
    if i >= height then trees
    else
      let trees = if area.(i).(j) = Square.Tree then trees + 1 else trees in
      down (i + ri, (j + dj) mod width) trees
  in
  down (0, 0) 0

let part_1 file =
  let area = area file in
  down area (1, 3)

let part_2 file =
  let area = area file in
  let down = down area in
  down (1, 1) * down (1, 3) * down (1, 5) * down (1, 7) * down (2, 1)

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
