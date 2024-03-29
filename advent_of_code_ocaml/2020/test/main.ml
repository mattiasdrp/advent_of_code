open Tezt
open Tezt.Base
open Days

let () =
  Test.register ~__FILE__ ~title:"d01.p1" ~tags:[ "d01"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day01.run 1 "resources/day01" = 970816) int)
    ~error_msg:"expected Day01.run 1 input = %R, got %L";
  Log.info "Day 01, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d01.p2" ~tags:[ "d01"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day01.run 2 "resources/day01" = 96047280) int)
    ~error_msg:"expected Day01.run 2 input = %R, got %L";
  Log.info "Day 01, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d02.p1" ~tags:[ "d02"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day02.run 1 "resources/day02" = 465) int)
    ~error_msg:"expected Day02.run 1 input = %R, got %L";
  Log.info "Day 02, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d02.p2" ~tags:[ "d02"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day02.run 2 "resources/day02" = 294) int)
    ~error_msg:"expected Day02.run 2 input = %R, got %L";
  Log.info "Day 02, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d03.p1" ~tags:[ "d03"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day03.run 1 "resources/day03" = 184) int)
    ~error_msg:"expected Day03.run 1 input = %R, got %L";
  Log.info "Day 03, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d03.p2" ~tags:[ "d03"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day03.run 2 "resources/day03" = 2431272960) int)
    ~error_msg:"expected Day03.run 2 input = %R, got %L";
  Log.info "Day 03, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d04.p1" ~tags:[ "d04"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day04.run 1 "resources/day04" = 210) int)
    ~error_msg:"expected Day04.run 1 input = %R, got %L";
  Log.info "Day 04, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d04.p2" ~tags:[ "d04"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day04.run 2 "resources/day04" = 131) int)
    ~error_msg:"expected Day04.run 2 input = %R, got %L";
  Log.info "Day 04, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d05.p1" ~tags:[ "d05"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day05.run 1 "resources/day05" = 930) int)
    ~error_msg:"expected Day05.run 1 input = %R, got %L";
  Log.info "Day 05, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d05.p2" ~tags:[ "d05"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day05.run 2 "resources/day05" = 515) int)
    ~error_msg:"expected Day05.run 2 input = %R, got %L";
  Log.info "Day 05, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d06.p1" ~tags:[ "d06"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day06.run 1 "resources/day06" = 6542) int)
    ~error_msg:"expected Day06.run 1 input = %R, got %L";
  Log.info "Day 06, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d06.p2" ~tags:[ "d06"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day06.run 2 "resources/day06" = 3299) int)
    ~error_msg:"expected Day06.run 2 input = %R, got %L";
  Log.info "Day 06, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d07.p1" ~tags:[ "d07"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day07.run 1 "resources/day07" = 131) int)
    ~error_msg:"expected Day07.run 1 input = %R, got %L";
  Log.info "Day 07, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d07.p2" ~tags:[ "d07"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day07.run 2 "resources/day07" = 11261) int)
    ~error_msg:"expected Day07.run 2 input = %R, got %L";
  Log.info "Day 07, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d08.p1" ~tags:[ "d08"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day08.run 1 "resources/day08" = 1810) int)
    ~error_msg:"expected Day08.run 1 input = %R, got %L";
  Log.info "Day 08, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d08.p2" ~tags:[ "d08"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day08.run 2 "resources/day08" = 969) int)
    ~error_msg:"expected Day08.run 2 input = %R, got %L";
  Log.info "Day 08, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d09.p1" ~tags:[ "d09"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day09.run 1 "resources/day09" 25 = 507622668) int)
    ~error_msg:"expected Day09.run 1 input = %R, got %L";
  Log.info "Day 09, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d09.p2" ~tags:[ "d09"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day09.run 2 "resources/day09" 25 = 76688505) int)
    ~error_msg:"expected Day09.run 2 input = %R, got %L";
  Log.info "Day 09, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d10.p1" ~tags:[ "d10"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day10.run 1 "resources/day10" = 2112) int)
    ~error_msg:"expected Day10.run 1 input = %R, got %L";
  Log.info "Day 10, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d10.p2" ~tags:[ "d10"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day10.run 2 "resources/day10" = 3022415986688) int)
    ~error_msg:"expected Day10.run 2 input = %R, got %L";
  Log.info "Day 10, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d11.p1" ~tags:[ "d11"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day11.run 1 "resources/day11" = 2152) int)
    ~error_msg:"expected Day11.run 1 input = %R, got %L";
  Log.info "Day 11, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d11.p2" ~tags:[ "d11"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day11.run 2 "resources/day11" = 1937) int)
    ~error_msg:"expected Day11.run 2 input = %R, got %L";
  Log.info "Day 11, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d12.p1" ~tags:[ "d12"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day12.run 1 "resources/day12" = 1441) int)
    ~error_msg:"expected Day12.run 1 input = %R, got %L";
  Log.info "Day 12, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d12.p2" ~tags:[ "d12"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day12.run 2 "resources/day12" = 61616) int)
    ~error_msg:"expected Day12.run 2 input = %R, got %L";
  Log.info "Day 12, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d13.p1" ~tags:[ "d13"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day13.run 1 "resources/day13" = 205) int)
    ~error_msg:"expected Day13.run 1 input = %R, got %L";
  Log.info "Day 13, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d13.p2" ~tags:[ "d13"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day13.run 2 "resources/day13" = 803025030761664) int)
    ~error_msg:"expected Day13.run 2 input = %R, got %L";
  Log.info "Day 13, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d14.p1" ~tags:[ "d14"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day14.run 1 "resources/day14" = 7817357407588) int)
    ~error_msg:"expected Day14.run 1 input = %R, got %L";
  Log.info "Day 14, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d14.p2" ~tags:[ "d14"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day14.run 2 "resources/day14" = 4335927555692) int)
    ~error_msg:"expected Day14.run 2 input = %R, got %L";
  Log.info "Day 14, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d15.p1" ~tags:[ "d15"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day15.run 1 "resources/day15" = 1015) int)
    ~error_msg:"expected Day15.run 1 input = %R, got %L";
  Log.info "Day 15, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d15.p2" ~tags:[ "d15"; "p2"; "really_slow" ]
  @@ fun () ->
  Check.((Day15.run 2 "resources/day15" = 201) int)
    ~error_msg:"expected Day15.run 2 input = %R, got %L";
  Log.info "Day 15, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d16.p1" ~tags:[ "d16"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day16.run 1 "resources/day16" = 29759) int)
    ~error_msg:"expected Day16.run 1 input = %R, got %L";
  Log.info "Day 16, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d16.p2" ~tags:[ "d16"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day16.run 2 "resources/day16" = 1307550234719) int)
    ~error_msg:"expected Day16.run 2 input = %R, got %L";
  Log.info "Day 16, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d17.p1" ~tags:[ "d17"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day17.run 1 "resources/day17" = 333) int)
    ~error_msg:"expected Day17.run 1 input = %R, got %L";
  Log.info "Day 17, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d17.p2" ~tags:[ "d17"; "p2"; "slow" ]
  @@ fun () ->
  Check.((Day17.run 2 "resources/day17" = 2676) int)
    ~error_msg:"expected Day17.run 2 input = %R, got %L";
  Log.info "Day 17, part 2, is correct.";
  unit

(* let () = *)
(*   Test.register ~__FILE__ ~title:"d18.p1" ~tags:[ "d18"; "p1"; "fast" ] *)
(*   @@ fun () -> *)
(*   Check.((Day18.run 1 "resources/day18" = 3159145843816) int) *)
(*     ~error_msg:"expected Day18.run 1 input = %R, got %L"; *)
(*   Log.info "Day 18, part 1, is correct."; *)
(*   unit *)

let () =
  Test.register ~__FILE__ ~title:"d18.p2" ~tags:[ "d18"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day18.run 2 "resources/day18" = 55699621957369) int)
    ~error_msg:"expected Day18.run 2 input = %R, got %L";
  Log.info "Day 18, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d19.p1" ~tags:[ "d19"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day19.run 1 "resources/day19" = 151) int)
    ~error_msg:"expected Day19.run 1 input = %R, got %L";
  Log.info "Day 19, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d19.p2" ~tags:[ "d19"; "p2"; "slow" ]
  @@ fun () ->
  Check.((Day19.run 2 "resources/day19" = 386) int)
    ~error_msg:"expected Day19.run 2 input = %R, got %L";
  Log.info "Day 19, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d20.p1" ~tags:[ "d20"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day20.run 1 "resources/day20" = 23386616781851) int)
    ~error_msg:"expected Day20.run 1 input = %R, got %L";
  Log.info "Day 20, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d20.p2" ~tags:[ "d20"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day20.run 2 "resources/day20" = 2376) int)
    ~error_msg:"expected Day20.run 2 input = %R, got %L";
  Log.info "Day 20, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d21.p1" ~tags:[ "d21"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day21.run 1 "resources/day21" = 2302) int)
    ~error_msg:"expected Day21.run 1 input = %R, got %L";
  Log.info "Day 21, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d21.p2" ~tags:[ "d21"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day21.run 2 "resources/day21" = 0) int)
    ~error_msg:"expected Day21.run 2 input = %R, got %L";
  Log.info "Day 21, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d22.p1" ~tags:[ "d22"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day22.run 1 "resources/day22" = 33559) int)
    ~error_msg:"expected Day22.run 1 input = %R, got %L";
  Log.info "Day 22, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d22.p2" ~tags:[ "d22"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day22.run 2 "resources/day22" = 32789) int)
    ~error_msg:"expected Day22.run 2 input = %R, got %L";
  Log.info "Day 22, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d23.p1" ~tags:[ "d23"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day23.run 1 "resources/day23" = 24987653) int)
    ~error_msg:"expected Day23.run 1 input = %R, got %L";
  Log.info "Day 23, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d23.p2" ~tags:[ "d23"; "p2"; "slow" ]
  @@ fun () ->
  Check.((Day23.run 2 "resources/day23" = 442938711161) int)
    ~error_msg:"expected Day23.run 2 input = %R, got %L";
  Log.info "Day 23, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d24.p1" ~tags:[ "d24"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day24.run 1 "resources/day24" = 254) int)
    ~error_msg:"expected Day24.run 1 input = %R, got %L";
  Log.info "Day 24, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d24.p2" ~tags:[ "d24"; "p2"; "slow" ]
  @@ fun () ->
  Check.((Day24.run 2 "resources/day24" = 3697) int)
    ~error_msg:"expected Day24.run 2 input = %R, got %L";
  Log.info "Day 24, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d25.p1" ~tags:[ "d25"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day25.run 1 "resources/day25" = 10187657) int)
    ~error_msg:"expected Day25.run 1 input = %R, got %L";
  Log.info "Day 25, part 1, is correct.";
  unit

(* CALL the main function of Tezt so that it actually runs your tests. *)
let () = Test.run ()
