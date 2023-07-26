let step subject_number value = value * subject_number mod 20201227

let loop_size public_key =
  let rec aux loop_size value =
    let value = step 7 value in
    if value = public_key then loop_size else aux (loop_size + 1) value
  in
  aux 1 1

let transform subject_number loop_size =
  let rec aux i value =
    if i = loop_size then value else aux (i + 1) (step subject_number value)
  in
  aux 0 1

let part_1 file =
  let ci = open_in file in
  let pk1 = int_of_string (input_line ci) in
  let pk2 = int_of_string (input_line ci) in
  close_in ci;
  let loop_size = loop_size pk1 in
  transform pk2 loop_size

let part_2 _file = failwith "TODO"
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
