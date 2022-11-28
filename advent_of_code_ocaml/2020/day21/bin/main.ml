open Mdrp_lib

let part_1 file =
  let ci = open_in file in
  let rec aux_parse acc =
    match input_line ci with s -> acc | exception End_of_file -> acc
  in
  aux_parse ()

let part_2 _file = ()

let () =
  let part = try Sys.argv.(1) with _ -> "2" in
  let file = try Sys.argv.(2) with _ -> "input" in
  match part with "1" -> part_1 file | "2" -> part_2 file | _ -> ()
