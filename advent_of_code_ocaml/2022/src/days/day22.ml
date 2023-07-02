open Mdrp_lib

module Square = struct
  type t = {
    id : int;
    content : int Array.t Array.t;
    west : int;
    north : int;
    east : int;
    south : int;
  }

  let pp ppf { content; west; north; east; south; _ } =
    Format.fprintf ppf "@[<v 0>%a@,%d %d %d %d@]"
      Array.Matrix.(pp Int.pp)
      content west north east south
end

let square_size file =
  let exception End of (int * int) in
  let width, height =
    try
      Parse.fold_lines
        (fun (width, height) s ->
          if s = "" then raise (End (width, height))
          else (max width (String.length s), height + 1))
        (0, 0) file
    with End res -> res
  in
  Int.Decimal.gcd width height

let parse_squares file =
  let square_size = square_size file in
  let squares =
    let l =
      List.init 6 (fun i ->
          ( i,
            Square.
              {
                id = i;
                content = Array.make_matrix square_size square_size 0;
                west = -1;
                north = -1;
                east = -1;
                south = -1;
              } ))
    in
    Int.Map.of_list l
  in
  Parse.fold_lines
    (fun (squares, last, path) s ->
      if s = "" then (squares, true, path)
      else if last then (squares, last, s)
      else (squares, last, path))
    (squares, false, "") file

let part_1 _file = failwith "TODO"

let part_2 file =
  let squares, _, path = parse_squares file in
  Format.eprintf "%a@.%s@." Int.Map.(pp Square.pp) squares path;
  0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
