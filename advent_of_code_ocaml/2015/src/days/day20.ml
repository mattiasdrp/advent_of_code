open Mdrp_lib

let house n =
  Seq.fold_left
    (fun acc (i1, i2) ->
      if i1 = i2 then acc + (10 * i1) else acc + (10 * i1) + (10 * i2))
    0
    (Int.Decimal.Seq.pair_divisors n)

let part_1 file =
  let input =
    let ci = open_in file in
    let res = int_of_string (input_line ci) in
    close_in ci;
    res
  in
  let rec aux i =
    let h = house i in
    if h >= input then i else aux (i + 1)
  in
  aux 1

let house n =
  Seq.fold_left
    (fun acc (i1, i2) ->
      let res1 = if i1 <> i2 && i2 <= 50 then 11 * i1 else 0 in
      let res2 = if i1 <= 50 then 11 * i2 else 0 in
      acc + res1 + res2)
    0
    (Int.Decimal.Seq.pair_divisors n)

let part_2 file =
  let input =
    let ci = open_in file in
    let res = int_of_string (input_line ci) in
    close_in ci;
    res
  in
  let rec aux i =
    let h = house i in
    if h >= input then i else aux (i + 1)
  in
  aux 1

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
