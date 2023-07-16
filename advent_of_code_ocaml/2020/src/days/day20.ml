open Mdrp_lib

module Square = struct
  type t = {
    id : int;
    rotation : int;
    north : string;
    west : string;
    east : string;
    south : string;
  }

  let empty =
    { id = -1; rotation = 0; north = ""; west = ""; east = ""; south = "" }

  let compare t1 t2 =
    let c = Int.compare t1.id t2.id in
    if c <> 0 then c else Int.compare t1.rotation t2.rotation

  let equal t1 t2 = Int.equal t1.id t2.id

  let pp ?(minimal = false) () ppf t =
    if minimal then
      Format.fprintf ppf "{id: %d, rot: %s}" t.id
        (match t.rotation with
        | 0 -> "∅"
        | 1 -> "↴"
        | 2 -> "↵"
        | 3 -> "↻"
        | _ -> assert false)
    else
      Format.fprintf ppf
        "{id = %d; rotation: %d; north = %s; west = %s; east = %s; south = %s }"
        t.id t.rotation t.north t.west t.east t.south

  (** Clockwise rotation by 90° *)
  let rotate t =
    {
      t with
      rotation = t.rotation + (1 mod 4);
      north = String.reverse t.west;
      east = t.north;
      south = String.reverse t.east;
      west = t.south;
    }

  type touching = NS | SN | WE | EW

  let pp_touching ppf t =
    Format.fprintf ppf "%s"
      (match t with NS -> "NS" | SN -> "SN" | WE -> "WE" | EW -> "EW")

  let same_sides t1 t2 =
    let rec aux s1 s2 touching acc = if s1 = s2 then touching :: acc else acc in
    aux t1.north t2.south NS []
    |> aux t1.south t2.north SN |> aux t1.west t2.east WE
    |> aux t1.east t2.west EW
end

let parse file =
  let re_field = Str.regexp {|Tile \([0-9]+\):|} in

  let _, _, acc =
    Parse.fold_lines ~end_line:true
      (fun (index, current, acc) line ->
        match line with
        | "" -> (0, Square.empty, current :: acc)
        | s ->
            (* First line giving us the id *)
            if Str.string_match re_field s 0 then
              ( 0,
                { current with id = int_of_string (Str.matched_group 1 s) },
                acc )
            else
              let north = if index = 0 then line else current.north in
              let south = line in
              let west = String.append_char current.west line.[0] in
              let east =
                String.append_char current.east line.[String.length line - 1]
              in
              (index + 1, { current with north; west; east; south }, acc))
      (0, Square.empty, []) file
  in
  Format.eprintf "@[<v 0>%a@."
    List.(pp ~pp_sep:Format.pp_print_cut Square.(pp ()))
    acc;
  acc

module SquareMap = Map.Make (struct
  include Square

  let pp = pp ~minimal:true ()
end)

let neighbours l =
  let rec aux map = function
    | s1 :: tl ->
        let map =
          List.fold_left
            (fun map s2 ->
              if Square.equal s1 s2 then map
              else
                let s2r = Square.rotate s2 in
                let s2rr = Square.rotate s2r in
                let s2rrr = Square.rotate s2rr in
                let l2 = [ s2; s2r; s2rr; s2rrr ] in
                List.fold_left
                  (fun map s2 ->
                    let sides = Square.same_sides s1 s2 in
                    match sides with
                    | [] -> map
                    | _ ->
                        SquareMap.update s1
                          (function
                            | None -> Some (SquareMap.singleton s2 sides)
                            | Some m -> Some (SquareMap.add s2 sides m))
                          map)
                  map l2)
            map l
        in
        aux map tl
    | [] -> map
  in
  aux SquareMap.empty l

let part_1 file =
  let squares = parse file in
  let map = neighbours squares in
  Format.eprintf "@[<v 0>%a@."
    SquareMap.(
      pp (fun ppf s ->
          Format.fprintf ppf "@[<v 0>%a@]"
            SquareMap.(pp List.(pp Square.(pp_touching)))
            s))
    map;
  0

let part_2 _file = failwith "TODO"
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
