let rec code =
  let m = 252533 in
  let q = 33554393 in
  fun col row ->
    if row = 0 && col = 0 then 20151125
    else
      let res =
        if col = 0 then code (row - 1) col else code (col - 1) (row + 1)
      in
      res mod q * m mod q

(* Faster with dynamic programming *)
let code_dp col row =
  let m = 252533 in
  let q = 33554393 in
  let rec aux prev c r =
    if c = col && r = row then prev mod q * m mod q
    else
      let res = prev mod q * m mod q in
      let c, r = if r = 0 then (0, c + 1) else (c + 1, r - 1) in
      aux res c r
  in
  aux 20151125 0 1

let part_1 _file =
  let row = 2980 in
  let col = 3074 in
  code_dp col row

let part_2 _file = failwith "TODO"
let run part file = match part with 1 -> part_1 file | _ -> part_2 file
