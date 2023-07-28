open Mdrp_lib

let parse line =
  let code_length = String.length line in
  let real_length, _, _ =
    String.fold_left
      (fun (acc, escaped, skip) -> function
        | ('\\' | '"') when escaped -> (acc + 1, false, 0)
        | '\\' -> (acc, true, 0)
        | '"' -> (acc, false, 0)
        | 'x' when escaped -> (acc, false, 1)
        | _ when skip = 1 -> (acc, false, 0)
        | _ -> (acc + 1, false, 0))
      (0, false, 0) line
  in
  (code_length, real_length)

let part_1 file =
  let code_lengths, real_lengths =
    Parse.fold_lines
      (fun (code_lengths, real_lengths) line ->
        let code_length, real_length = parse line in
        (code_length + code_lengths, real_length + real_lengths))
      (0, 0) file
  in
  code_lengths - real_lengths

let parse line = (String.length line, 2 + (String.length @@ String.escaped line))

let part_2 file =
  let orig_lengths, escaped_lengths =
    Parse.fold_lines
      (fun (orig_lengths, escaped_lengths) line ->
        let orig_length, escaped_length = parse line in
        (orig_length + orig_lengths, escaped_length + escaped_lengths))
      (0, 0) file
  in
  escaped_lengths - orig_lengths

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
