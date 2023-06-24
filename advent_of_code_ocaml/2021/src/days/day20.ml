open Mdrp_lib

module Image = struct
  type t = int array array

  let pp ppf t =
    Format.fprintf ppf "@[<v 0>";
    Array.iter
      (fun a ->
        Array.iter
          (fun v ->
            Format.fprintf ppf "%c"
              (match v with 0 -> '.' | 1 -> '#' | _ -> assert false))
          a;
        Format.fprintf ppf "@,")
      t;
    Format.fprintf ppf "@]"

  let default blink step = if blink then if step mod 2 = 0 then 0 else 1 else 0

  let vicinity ?(blink = true) t (x, y) step =
    let width, height = Array.Matrix.width_height t in
    (* We need to take into account that the vicinity are the cells in the previous *)
    (* step, not the one we're working on. So (step-1) *)
    let default = default blink (step - 1) in
    let value (x, y) =
      string_of_int
        (if x < 0 || y < 0 || x >= height || y >= width then default
         else t.(x).(y))
    in
    value (x - 1, y - 1)
    ^ value (x - 1, y)
    ^ value (x - 1, y + 1)
    ^ value (x, y - 1)
    ^ value (x, y)
    ^ value (x, y + 1)
    ^ value (x + 1, y - 1)
    ^ value (x + 1, y)
    ^ value (x + 1, y + 1)
    |> Int.Decimal.of_bin

  let augment ?(blink = true) step t =
    let incr = 2 in
    let shift = incr / 2 in
    let width = Array.length t.(0) + incr in
    let height = Array.length t + incr in
    let default = default blink step in
    Array.init height (fun i ->
        if i < shift || i >= height - shift then Array.make width default
        else
          let a = t.(i - shift) in
          Array.init width (fun i ->
              if i < shift || i >= width - shift then default else a.(i - shift)))

  let enhance ?(blink = true) step enhancement t =
    Array.mapi
      (fun x a ->
        Array.mapi (fun y _ -> enhancement.(vicinity ~blink t (x, y) step)) a)
      t
    |> augment ~blink step

  let lit t =
    Array.Matrix.fold (fun acc _ _ v -> if v = 1 then acc + 1 else acc) 0 t
end

let part_1 blink enhancement matrix =
  let matrix = Image.enhance ~blink 1 enhancement matrix in
  let matrix = Image.enhance ~blink 2 enhancement matrix in
  Image.lit matrix

let part_2 blink enhancement matrix =
  let rec aux i matrix =
    if i > 50 then Image.lit matrix
    else aux (i + 1) (Image.enhance ~blink i enhancement matrix)
  in
  aux 1 matrix

let run part file =
  let ci = open_in file in
  let enhancement =
    let s = input_line ci in
    Array.init (String.length s) (fun i ->
        match s.[i] with '.' -> 0 | '#' -> 1 | _ -> assert false)
  in
  let blink = enhancement.(0) = 1 in
  ignore (input_line ci);
  let rec aux_parse acc =
    match input_line ci with
    | s ->
        aux_parse
          (Array.init (String.length s) (fun i ->
               match s.[i] with '.' -> 0 | '#' -> 1 | _ -> assert false)
          :: acc)
    | exception End_of_file ->
        close_in ci;
        (* let sl = Array.length (List.hd acc) in *)
        (* let l = *)
        List.rev acc |> Array.of_list |> Image.augment 0
  in
  let matrix = aux_parse [] in
  match part with
  | 1 -> part_1 blink enhancement matrix
  | _ -> part_2 blink enhancement matrix
