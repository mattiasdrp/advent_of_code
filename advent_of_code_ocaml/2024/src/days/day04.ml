module TextMatrix = struct
  type t = string Array.t
  type direction = Normal | Reverse

  let word = "XMAS"

  let of_list l =
    let padded_l = List.map (fun row -> "...." ^ row ^ "....") l in
    let frow = List.hd padded_l in
    let length = String.length frow in
    let padded_row = String.init length (fun _ -> '.') in
    let four_padded = [ padded_row; padded_row; padded_row; padded_row ] in
    four_padded @ padded_l @ four_padded |> Array.of_list

  let pp ppf t =
    Format.(pp_print_array ~pp_sep:pp_print_cut pp_print_string ppf) t

  let check t row col char = t.(row).[col] = char

  let count_xmas (t : t) row col =
    (* Horizontal *)
    (* Left *)
    let cpt =
      match
        (t.(row).[col], t.(row).[col + 1], t.(row).[col + 2], t.(row).[col + 3])
      with
      | 'X', 'M', 'A', 'S' -> 1
      | _ -> 0
    in
    (* Right *)
    let cpt =
      match
        (t.(row).[col], t.(row).[col - 1], t.(row).[col - 2], t.(row).[col - 3])
      with
      | 'X', 'M', 'A', 'S' -> cpt + 1
      | _ -> cpt
    in
    (* Vertical *)
    (* Up *)
    let cpt =
      match
        (t.(row).[col], t.(row + 1).[col], t.(row + 2).[col], t.(row + 3).[col])
      with
      | 'X', 'M', 'A', 'S' -> cpt + 1
      | _ -> cpt
    in
    (* Down *)
    let cpt =
      match
        (t.(row).[col], t.(row - 1).[col], t.(row - 2).[col], t.(row - 3).[col])
      with
      | 'X', 'M', 'A', 'S' -> cpt + 1
      | _ -> cpt
    in
    (* Diagonal *)
    let cpt =
      match
        ( t.(row).[col],
          t.(row + 1).[col + 1],
          t.(row + 2).[col + 2],
          t.(row + 3).[col + 3] )
      with
      | 'X', 'M', 'A', 'S' -> cpt + 1
      | _ -> cpt
    in
    let cpt =
      match
        ( t.(row).[col],
          t.(row + 1).[col - 1],
          t.(row + 2).[col - 2],
          t.(row + 3).[col - 3] )
      with
      | 'X', 'M', 'A', 'S' -> cpt + 1
      | _ -> cpt
    in
    let cpt =
      match
        ( t.(row).[col],
          t.(row - 1).[col + 1],
          t.(row - 2).[col + 2],
          t.(row - 3).[col + 3] )
      with
      | 'X', 'M', 'A', 'S' -> cpt + 1
      | _ -> cpt
    in
    let cpt =
      match
        ( t.(row).[col],
          t.(row - 1).[col - 1],
          t.(row - 2).[col - 2],
          t.(row - 3).[col - 3] )
      with
      | 'X', 'M', 'A', 'S' -> cpt + 1
      | _ -> cpt
    in
    cpt

  let check_xmas t =
    let cpt = ref 0 in
    Array.iteri
      (fun row s ->
        String.iteri
          (fun col c -> if c = 'X' then cpt := !cpt + count_xmas t row col)
          s)
      t;
    !cpt

  let count_x_mas t row col =
    let first_branch =
      match (t.(row + 1).[col + 1], t.(row - 1).[col - 1]) with
      | 'M', 'S' | 'S', 'M' -> true
      | _ -> false
    in
    let second_branch =
      match (t.(row - 1).[col + 1], t.(row + 1).[col - 1]) with
      | 'M', 'S' | 'S', 'M' -> true
      | _ -> false
    in
    if first_branch && second_branch then 1 else 0

  let check_x_mas t =
    let cpt = ref 0 in
    Array.iteri
      (fun row s ->
        String.iteri
          (fun col c -> if c = 'A' then cpt := !cpt + count_x_mas t row col)
          s)
      t;
    !cpt
end

let part_1 file =
  let matrix = Mdrp_lib.Parse.lines file |> TextMatrix.of_list in
  Format.eprintf "@[<v 0>%a@." TextMatrix.pp matrix;
  TextMatrix.check_xmas matrix

let part_2 file =
  let matrix = Mdrp_lib.Parse.lines file |> TextMatrix.of_list in
  Format.eprintf "@[<v 0>%a@." TextMatrix.pp matrix;
  TextMatrix.check_x_mas matrix

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
