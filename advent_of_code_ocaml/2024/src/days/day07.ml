open Mdrp_lib

let operate part goal numbers =
  let rec aux res = function
    | hd :: tl -> (
        match aux (res + hd) tl with
        | res -> res
        | exception Not_found -> (
            match aux (res * hd) tl with
            | res -> res
            | exception Not_found ->
                if part = 1 then raise Not_found
                else
                  let conc =
                    string_of_int res ^ string_of_int hd |> int_of_string
                  in
                  aux conc tl))
    | [] -> if res = goal then res else raise Not_found
  in
  aux (List.hd numbers) (List.tl numbers)

let common_part part file =
  Parse.fold_lines
    (fun acc line ->
      match String.split_on_char ':' line with
      | [ goal; numbers ] -> (
          let numbers =
            String.split_on_char_non_empty ' ' numbers |> List.map int_of_string
          in

          let goal = int_of_string goal in
          match operate part goal numbers with
          | _res -> acc + goal
          | exception Not_found -> acc)
      | _ -> assert false)
    0 file

let run part file = common_part part file
