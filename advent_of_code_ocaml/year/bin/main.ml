open Days

let file = match Sys.argv.(2) with file -> Some file | exception _ -> None

let () =
  let get_file day =
    match file with None -> "resources/day" ^ day | Some file -> file
  in
  (match Sys.argv.(1) with
  | "d01.p1" -> Day01.run 1 (get_file "01")
  | "d01.p2" -> Day01.run 2 (get_file "01")
  | "d02.p1" -> Day02.run 1 (get_file "02")
  | "d02.p2" -> Day02.run 2 (get_file "02")
  | "d03.p1" -> Day03.run 1 (get_file "03")
  | "d03.p2" -> Day03.run 2 (get_file "03")
  | "d04.p1" -> Day04.run 1 (get_file "04")
  | "d04.p2" -> Day04.run 2 (get_file "04")
  | "d05.p1" -> Day05.run 1 (get_file "05")
  | "d05.p2" -> Day05.run 2 (get_file "05")
  | "d06.p1" -> Day06.run 1 (get_file "06")
  | "d06.p2" -> Day06.run 2 (get_file "06")
  | "d07.p1" -> Day07.run 1 (get_file "07")
  | "d07.p2" -> Day07.run 2 (get_file "07")
  | "d08.p1" -> Day08.run 1 (get_file "08")
  | "d08.p2" -> Day08.run 2 (get_file "08")
  | "d09.p1" -> Day09.run 1 (get_file "09")
  | "d09.p2" -> Day09.run 2 (get_file "09")
  | "d10.p1" -> Day10.run 1 (get_file "10")
  | "d10.p2" -> Day10.run 2 (get_file "10")
  | "d11.p1" -> Day11.run 1 (get_file "11")
  | "d11.p2" -> Day11.run 2 (get_file "11")
  | "d12.p1" -> Day12.run 1 (get_file "12")
  | "d12.p2" -> Day12.run 2 (get_file "12")
  | "d13.p1" -> Day13.run 1 (get_file "13")
  | "d13.p2" -> Day13.run 2 (get_file "13")
  | "d14.p1" -> Day14.run 1 (get_file "14")
  | "d14.p2" -> Day14.run 2 (get_file "14")
  | "d15.p1" -> Day15.run 1 (get_file "15")
  | "d15.p2" -> Day15.run 2 (get_file "15")
  | "d16.p1" -> Day16.run 1 (get_file "16")
  | "d16.p2" -> Day16.run 2 (get_file "16")
  | "d17.p1" -> Day17.run 1 (get_file "17")
  | "d17.p2" -> Day17.run 2 (get_file "17")
  | "d18.p1" -> Day18.run 1 (get_file "18")
  | "d18.p2" -> Day18.run 2 (get_file "18")
  | "d19.p1" -> Day19.run 1 (get_file "19")
  | "d19.p2" -> Day19.run 2 (get_file "19")
  | "d20.p1" -> Day20.run 1 (get_file "20")
  | "d20.p2" -> Day20.run 2 (get_file "20")
  | "d21.p1" -> Day21.run 1 (get_file "21")
  | "d21.p2" -> Day21.run 2 (get_file "21")
  | "d22.p1" -> Day22.run 1 (get_file "22")
  | "d22.p2" -> Day22.run 2 (get_file "22")
  | "d23.p1" -> Day23.run 1 (get_file "23")
  | "d23.p2" -> Day23.run 2 (get_file "23")
  | "d24.p1" -> Day24.run 1 (get_file "24")
  | "d24.p2" -> Day24.run 2 (get_file "24")
  | "d25.p1" -> Day25.run 1 (get_file "25")
  | "d25.p2" -> Day25.run 2 (get_file "25")
  | str ->
      Format.eprintf "%s part not found" str;
      exit 0)
  |> Format.printf "%d@."
