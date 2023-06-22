open Mdrp_lib

let part_1 file =
  let _, cpt =
    Parse.fold_lines
      (fun (prev, cpt) i ->
        let i = int_of_string i in
        let cpt = if i > prev then cpt + 1 else cpt in
        (i, cpt))
      (0, -1) file
    (* Starting at -1 because we need to discount the first measurement *)
  in
  Format.printf "%d@." cpt

let part_2 file =
  let _, cpt =
    Parse.fold_lines
      (fun ((idx, v1, v2, v3), cpt) i ->
        let i = int_of_string i in
        if idx < 3 then ((idx + 1, v2, v3, i), cpt)
        else
          let prev_sum = v1 + v2 + v3 in
          let new_sum = v2 + v3 + i in
          let cpt = if new_sum > prev_sum then cpt + 1 else cpt in
          ((idx + 1, v2, v3, i), cpt))
      ((0, 0, 0, 0), 0)
      file
    (* Starting at 0 because we start at index 3 so fourth number *)
  in
  Format.printf "%d@." cpt

let generate file i =
  let co = open_out file in
  let ppf = Format.formatter_of_out_channel co in
  for _ = 0 to i do
    Format.fprintf ppf "%d@." (Random.int 1000)
  done;
  close_out co

let () =
  let part = try Sys.argv.(1) with _ -> "2" in
  let file = try Sys.argv.(2) with _ -> "input" in
  match part with
  | "1" -> part_1 file
  | "2" -> part_2 file
  | i -> generate file (int_of_string i)
