open Mdrp_lib

let size_min = 1
let size_max = 9
let size_square = size_max - size_min

module rec Square : sig
  type t = {
    id : int;
    flip : bool;
    rotation : int;
    north : int * int;
    west : int * int;
    east : int * int;
    south : int * int;
    content : string array;
  }
  (** A square has 4 sides. Each side is a pair of int because they can be read in both directions
      and we need to remember both values.

      The flip and rotation fields correspond to the transformations applied to the square *)

  val create :
    int ->
    north:string ->
    west:string ->
    east:string ->
    south:string ->
    string list ->
    t
  (** Classic creation.

      The only thing that happens here is the trimming of the content to remove the borders *)

  val pp : ?minimal:bool -> unit -> Format.formatter -> t -> unit

  val compare : t -> t -> int
  (** We don't need a complex comparison function
      since it's impossible that we add the same square that has been rotated or flipped *)

  val equal : t -> t -> bool
  (** Same as [!compare] *)

  val get_row : int -> t -> string
  (** [get_row row t] will return the row at index [row] from the content according to the transformations applied to [t]. *)

  val transform : bool -> int -> t -> t
  (** [transform flip rotate t] flips [t] according to [flip] then applies a clockwise rotation by steps of [rotate * 90]° *)

  type touching = NS | EW | SN | WE

  val transform_touching : t -> touching -> touching
  val same_sides : t -> t -> touching list
  val pp_touching : Format.formatter -> touching -> unit

  val neighbours : t list -> touching list SquareMap.t SquareMap.t
  (** For each square [t], for each possible flipping and rotation of a square [t'], checks if [t'] it can [t] *)
end = struct
  type t = {
    id : int;
    flip : bool;
    rotation : int;
    north : int * int;
    west : int * int;
    east : int * int;
    south : int * int;
    content : string array;
  }

  (** The value of a side *)
  let value string =
    let v, r, _ =
      String.fold_left
        (fun (value, reverse, rev_pow) -> function
          | '.' -> (value lsl 1, reverse, rev_pow * 2)
          | _ -> ((value lsl 1) + 1, reverse + rev_pow, rev_pow * 2))
        (0, 0, 1) string
    in
    (v, r)

  let create id ~north ~west ~east ~south content =
    let north = value north in
    let west = value west in
    let east = value east in
    let south = value south in
    let content =
      List.map (fun s -> String.sub s size_min size_square) content
      |> List.rev |> Array.of_list
    in
    { id; flip = false; rotation = 0; north; west; east; south; content }

  let compare t1 t2 = Int.compare t1.id t2.id
  let equal t1 t2 = Int.equal t1.id t2.id

  (** Flip the square then rotate it *)
  let apply_transformation t =
    match t.flip with
    | true -> (
        match t.rotation with
        | 0 ->
            {
              t with
              north = Pair.rev t.north;
              south = Pair.rev t.south;
              west = t.east;
              east = t.west;
            }
        | 1 ->
            {
              t with
              north = Pair.rev t.east;
              east = Pair.rev t.north;
              south = Pair.rev t.west;
              west = Pair.rev t.south;
            }
        | 2 ->
            {
              t with
              north = t.south;
              south = t.north;
              east = Pair.rev t.east;
              west = Pair.rev t.west;
            }
        | 3 ->
            {
              t with
              north = t.west;
              east = t.south;
              south = t.east;
              west = t.north;
            }
        | _ -> assert false)
    | false -> (
        match t.rotation with
        | 0 -> t
        | 1 ->
            {
              t with
              north = Pair.rev t.west;
              east = t.north;
              south = Pair.rev t.east;
              west = t.south;
            }
        | 2 ->
            {
              t with
              north = Pair.rev t.south;
              east = Pair.rev t.west;
              south = Pair.rev t.north;
              west = Pair.rev t.east;
            }
        | 3 ->
            {
              t with
              north = t.east;
              east = Pair.rev t.south;
              south = t.west;
              west = Pair.rev t.north;
            }
        | _ -> assert false)

  let pp ?(minimal = false) () ppf t =
    if minimal then
      Format.fprintf ppf "{id: %d, rot: %s; flip: %b}" t.id
        (match t.rotation with
        | 0 -> "0"
        | 1 -> "1"
        | 2 -> "2"
        | 3 -> "3"
        | _ -> assert false)
        t.flip
    else
      Format.fprintf ppf
        "{id = %d; rotation = %d; flip: %5b; north = %3d; south = %3d; west = \
         %3d; east = %3d }"
        t.id t.rotation t.flip (fst t.north) (fst t.south) (fst t.west)
        (fst t.east)

  let max_index = size_square - 1

  let get_row row t =
    match t.flip with
    | false -> (
        match t.rotation with
        | 0 -> t.content.(row)
        | 2 -> String.reverse t.content.(max_index - row)
        | 1 ->
            let bytes = Bytes.create size_square in
            for i = max_index downto 0 do
              let s = Array.unsafe_get t.content i in
              Bytes.unsafe_set bytes (max_index - i) s.[row]
            done;
            Bytes.to_string bytes
        | 3 ->
            let bytes = Bytes.create size_square in
            for i = 0 to max_index do
              let s = Array.unsafe_get t.content i in
              Bytes.unsafe_set bytes i s.[max_index - row]
            done;
            Bytes.to_string bytes
        | _ -> assert false)
    | true -> (
        match t.rotation with
        | 0 -> String.reverse t.content.(row)
        | 2 -> t.content.(max_index - row)
        | 1 ->
            let bytes = Bytes.create size_square in
            for i = max_index downto 0 do
              let s = Array.unsafe_get t.content i in
              Bytes.unsafe_set bytes (max_index - i) s.[max_index - row]
            done;
            Bytes.to_string bytes
        | 3 ->
            let bytes = Bytes.create size_square in
            for i = 0 to max_index do
              let s = Array.unsafe_get t.content i in
              Bytes.unsafe_set bytes i s.[row]
            done;
            Bytes.to_string bytes
        | _ -> assert false)

  let transform flip rotate t =
    let rotation =
      (* If the square is flipped, the rotation applied to it needs to be counterclockwise. *)
      ((if flip then 4 - t.rotation else t.rotation) + rotate) mod 4
    in
    let flip = flip ||! t.flip in
    apply_transformation { t with rotation; flip }

  (** Type representing on which side two squares are touching *)
  type touching = NS | EW | SN | WE

  let to_int flip = function
    | NS -> 0
    | EW -> if flip then 3 else 1
    | SN -> 2
    | WE -> if flip then 1 else 3

  let of_int = function
    | 0 -> NS
    | 1 -> EW
    | 2 -> SN
    | 3 -> WE
    | _ -> assert false

  let move flip i touching = of_int ((to_int flip touching + i) mod 4)

  (** Transform a side-by-side touching according to the flipping and rotation of the parent square *)
  let transform_touching t touching = move t.flip t.rotation touching

  let pp_touching ppf t =
    Format.fprintf ppf "%s"
      (match t with NS -> "NS" | SN -> "SN" | WE -> "WE" | EW -> "EW")

  let same_sides t1 t2 =
    let aux s1 s2 touching acc = if s1 = s2 then touching :: acc else acc in
    aux t1.north t2.south NS []
    |> aux t1.south t2.north SN |> aux t1.west t2.east WE
    |> aux t1.east t2.west EW

  let neighbours l =
    let rec aux map = function
      | s1 :: tl ->
          let set =
            List.fold_left
              (fun set s2 ->
                if equal s1 s2 then set
                else
                  let rotations = [ 0; 1; 2; 3 ] in
                  let transformations =
                    List.cartesian_product [ false; true ] rotations
                  in
                  List.fold_left
                    (fun set (flip, rotate) ->
                      let s2 = transform flip rotate s2 in
                      let sides = same_sides s1 s2 in
                      match sides with
                      | [] -> set
                      | _ -> SquareMap.add s2 sides set)
                    set transformations)
              SquareMap.empty l
          in
          aux (SquareMap.add s1 set map) tl
      | [] -> map
    in
    aux SquareMap.empty l
end

and SquareMap : (Map.S with type key = Square.t) = Map.Make (struct
  include Square

  let pp ppf t = pp ~minimal:true () ppf t
end)

module SquareSet = Set.Make (struct
  include Square

  let pp = Square.pp ~minimal:true ()
end)

let parse file =
  let re_field = Str.regexp {|Tile \([0-9]+\):|} in

  let _, _, _, _, _, _, _, acc =
    Parse.fold_lines ~end_line:true
      (fun (index, id, north, west, east, south, lines, acc) line ->
        match line with
        | "" ->
            ( 0,
              -1,
              "",
              "",
              "",
              "",
              [],
              Square.create id ~north ~west ~east ~south lines :: acc )
        | _ ->
            (* First line giving us the id *)
            if Str.string_match re_field line 0 then
              ( 0,
                int_of_string (Str.matched_group 1 line),
                north,
                west,
                east,
                south,
                lines,
                acc )
            else
              let north = if index = 0 then line else north in
              let south = line in
              let west = String.append_char west line.[0] in
              let east =
                String.append_char east line.[String.length line - 1]
              in
              let lines =
                if index < size_min || index >= size_max then lines
                else line :: lines
              in
              (index + 1, id, north, west, east, south, lines, acc))
      (0, -1, "", "", "", "", [], [])
      file
  in
  Square.neighbours acc

let part_1 file =
  let neighbours = parse file in
  SquareMap.fold
    (fun s map angles ->
      if SquareMap.cardinal map = 2 then angles * s.Square.id else angles)
    neighbours 1

module Image = struct
  type t = { rows : int; cols : int; content : string array }

  let pp ppf t =
    Format.fprintf ppf "{rows: %d; cols: %d; content: %a}" t.rows t.cols
      Array.(pp String.pp)
      t.content

  let sharps t =
    Array.fold_left
      (fun acc s ->
        String.fold_left (fun acc -> function '#' -> acc + 1 | _ -> acc) acc s)
      0 t.content
end

let recompose_image neighbours =
  let boarders =
    SquareMap.fold
      (fun _s map boarders ->
        if SquareMap.cardinal map <= 3 then boarders + 1 else boarders)
      neighbours 0
  in
  let angle, _ =
    SquareMap.find_predicate
      (fun _s map ->
        SquareMap.cardinal map = 2
        &&
        match SquareMap.bindings map with
        | [ (_, [ Square.SN ]); (_, [ EW ]) ] | [ (_, [ EW ]); (_, [ SN ]) ] ->
            true
        | _ -> false)
      neighbours
  in
  let image = Array.init (size_square * ((boarders / 4) + 1)) (fun _ -> "") in
  let queue = Queue.create () in
  let next_rows = Queue.create () in
  Queue.push (angle, (0, 0)) queue;
  while not (Queue.is_empty queue && Queue.is_empty next_rows) do
    let square, (row, col) =
      try Queue.pop queue with Queue.Empty -> Queue.pop next_rows
    in
    for i = 0 to size_square - 1 do
      let s = Square.get_row i square in
      let i_image = (size_square * row) + i in
      let is = Array.unsafe_get image i_image in
      Array.unsafe_set image i_image (is ^ s)
    done;
    let nbrs = SquareMap.find square neighbours in
    SquareMap.iter
      (fun neighbour sides ->
        (* Don't forget to flip and rotate the touching sides according to the parent square *)
        match List.map (Square.transform_touching square) sides with
        | [ Square.NS ] | [ WE ] -> ()
        | [ SN ] when col = 0 ->
            (* transform the next square according to its parent flipping and rotation *)
            let neighbour =
              Square.transform square.flip square.rotation neighbour
            in
            Queue.add (neighbour, (row + 1, 0)) next_rows
        | [ EW ] ->
            (* transform the next square according to its parent flipping and rotation *)
            let neighbour =
              Square.transform square.flip square.rotation neighbour
            in
            Queue.add (neighbour, (row, col + 1)) queue
        | [ SN ] -> ()
        | _ -> assert false)
      nbrs
  done;
  Image.
    {
      rows = Array.length image;
      cols = String.length image.(0);
      content = image;
    }

let flip (Image.{ rows; content; _ } as t) =
  let ncontent = Array.copy content in
  for i = 0 to (rows - 1) / 2 do
    ncontent.(i) <- content.(rows - 1 - i);
    ncontent.(rows - 1 - i) <- content.(i)
  done;
  { t with content = ncontent }

let rotate Image.{ rows; cols; content } =
  let content =
    Array.init cols (fun col ->
        String.init rows (fun row -> content.(rows - 1 - row).[col]))
  in
  Image.{ rows = cols; cols = rows; content }

let check_monster image row_i col_i monster =
  let open Image in
  let rec aux row col =
    row < image.rows && col < image.cols
    && (row = monster.rows
       ||
       if col = monster.cols then aux (row + 1) 0
       else
         match monster.content.(row).[col] with
         | ' ' -> aux row (col + 1)
         | '#' -> (
             match image.content.(row_i + row).[col_i + col] with
             | '#' -> aux row (col + 1)
             | _ | (exception Invalid_argument _) -> false)
         | _ | (exception Invalid_argument _) -> assert false)
  in
  aux 0 0

let number_of_monsters image monster =
  let open Image in
  let rec aux row col res =
    if row >= image.rows - monster.rows then res
    else if col >= image.cols - monster.cols then aux (row + 1) 0 res
    else if check_monster image row col monster then aux row (col + 1) (res + 1)
    else aux row (col + 1) res
  in
  aux 0 0 0

let apply n rotate image =
  let rec aux i image = if i = 0 then image else aux (i - 1) (rotate image) in
  aux n image

let find_monsters image monster =
  let rotations = [ 0; 1; 2; 3 ] in
  let transformations = List.cartesian_product [ false; true ] rotations in
  let rec aux = function
    | [] -> assert false
    | (f, r) :: tl ->
        let monster = (if f then flip monster else monster) |> apply r rotate in
        let res = number_of_monsters image monster in
        if res > 0 then res else aux tl
  in
  aux transformations

let part_2 file =
  let neighbours = parse file in
  let image = recompose_image neighbours in
  let sharps_image = Image.sharps image in

  let monster =
    Image.
      {
        rows = 3;
        cols = 20;
        content =
          [|
            "                  # ";
            "#    ##    ##    ###";
            " #  #  #  #  #  #   ";
          |];
      }
  in
  let sharps_monster = Image.sharps monster in
  let monsters = find_monsters image monster in
  sharps_image - (monsters * sharps_monster)

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
