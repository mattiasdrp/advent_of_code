open Tezt
open Tezt.Base
open Days

let () =
  Test.register ~__FILE__ ~title:"d06.p1" ~tags:[ "d06"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day06.run 1 "resources/day06" = 543903) int)
    ~error_msg:"expected Day06.run 1 input = %R, got %L";
  Log.info "Day 06, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d06.p2" ~tags:[ "d06"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day06.run 2 "resources/day06" = 14687245) int)
    ~error_msg:"expected Day06.run 2 input = %R, got %L";
  Log.info "Day 06, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d07.p1" ~tags:[ "d07"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day07.run 1 "resources/day07" = 46065) int)
    ~error_msg:"expected Day07.run 1 input = %R, got %L";
  Log.info "Day 07, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d07.p2" ~tags:[ "d07"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day07.run 2 "resources/day07" = 14134) int)
    ~error_msg:"expected Day07.run 2 input = %R, got %L";
  Log.info "Day 07, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d08.p1" ~tags:[ "d08"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day08.run 1 "resources/day08" = 1371) int)
    ~error_msg:"expected Day08.run 1 input = %R, got %L";
  Log.info "Day 08, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d08.p2" ~tags:[ "d08"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day08.run 2 "resources/day08" = 2117) int)
    ~error_msg:"expected Day08.run 2 input = %R, got %L";
  Log.info "Day 08, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d09.p1" ~tags:[ "d09"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day09.run 1 "resources/day09" = 207) int)
    ~error_msg:"expected Day09.run 1 input = %R, got %L";
  Log.info "Day 09, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d09.p2" ~tags:[ "d09"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day09.run 2 "resources/day09" = 804) int)
    ~error_msg:"expected Day09.run 2 input = %R, got %L";
  Log.info "Day 09, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d10.p1" ~tags:[ "d10"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day10.run 1 "resources/day10" = 360154) int)
    ~error_msg:"expected Day10.run 1 input = %R, got %L";
  Log.info "Day 10, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d10.p2" ~tags:[ "d10"; "p2"; "slow" ]
  @@ fun () ->
  Check.((Day10.run 2 "resources/day10" = 5103798) int)
    ~error_msg:"expected Day10.run 2 input = %R, got %L";
  Log.info "Day 10, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d11.p1" ~tags:[ "d11"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day11.run 1 "resources/day11" = 0) int)
    ~error_msg:"expected Day11.run 1 input = %R, got %L";
  Log.info "Day 11, part 1, is correct if 'vzbxxyzz'.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d11.p2" ~tags:[ "d11"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day11.run 2 "resources/day11" = 0) int)
    ~error_msg:"expected Day11.run 2 input = %R, got %L";
  Log.info "Day 11, part 2, is correct if 'vzcaabcc'.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d12.p1" ~tags:[ "d12"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day12.run 1 "resources/day12" = 191164) int)
    ~error_msg:"expected Day12.run 1 input = %R, got %L";
  Log.info "Day 12, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d12.p2" ~tags:[ "d12"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day12.run 2 "resources/day12" = 87842) int)
    ~error_msg:"expected Day12.run 2 input = %R, got %L";
  Log.info "Day 12, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d13.p1" ~tags:[ "d13"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day13.run 1 "resources/day13" = 709) int)
    ~error_msg:"expected Day13.run 1 input = %R, got %L";
  Log.info "Day 13, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d13.p2" ~tags:[ "d13"; "p2"; "slow" ]
  @@ fun () ->
  Check.((Day13.run 2 "resources/day13" = 668) int)
    ~error_msg:"expected Day13.run 2 input = %R, got %L";
  Log.info "Day 13, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d14.p1" ~tags:[ "d14"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day14.run 1 "resources/day14" = 2640) int)
    ~error_msg:"expected Day14.run 1 input = %R, got %L";
  Log.info "Day 14, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d14.p2" ~tags:[ "d14"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day14.run 2 "resources/day14" = 1102) int)
    ~error_msg:"expected Day14.run 2 input = %R, got %L";
  Log.info "Day 14, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d15.p1" ~tags:[ "d15"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day15.run 1 "resources/day15" = 18965440) int)
    ~error_msg:"expected Day15.run 1 input = %R, got %L";
  Log.info "Day 15, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d15.p2" ~tags:[ "d15"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day15.run 2 "resources/day15" = 15862900) int)
    ~error_msg:"expected Day15.run 2 input = %R, got %L";
  Log.info "Day 15, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d16.p1" ~tags:[ "d16"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day16.run 1 "resources/day16" = 213) int)
    ~error_msg:"expected Day16.run 1 input = %R, got %L";
  Log.info "Day 16, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d16.p2" ~tags:[ "d16"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day16.run 2 "resources/day16" = 323) int)
    ~error_msg:"expected Day16.run 2 input = %R, got %L";
  Log.info "Day 16, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d17.p1" ~tags:[ "d17"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day17.run 1 "resources/day17" = 654) int)
    ~error_msg:"expected Day17.run 1 input = %R, got %L";
  Log.info "Day 17, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d17.p2" ~tags:[ "d17"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day17.run 2 "resources/day17" = 57) int)
    ~error_msg:"expected Day17.run 2 input = %R, got %L";
  Log.info "Day 17, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d18.p1" ~tags:[ "d18"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day18.run 1 "resources/day18" = 1061) int)
    ~error_msg:"expected Day18.run 1 input = %R, got %L";
  Log.info "Day 18, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d18.p2" ~tags:[ "d18"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day18.run 2 "resources/day18" = 1006) int)
    ~error_msg:"expected Day18.run 2 input = %R, got %L";
  Log.info "Day 18, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d19.p1" ~tags:[ "d19"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day19.run 1 "resources/day19" = 576) int)
    ~error_msg:"expected Day19.run 1 input = %R, got %L";
  Log.info "Day 19, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d19.p2" ~tags:[ "d19"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day19.run 2 "resources/day19" = 207) int)
    ~error_msg:"expected Day19.run 2 input = %R, got %L";
  Log.info "Day 19, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d20.p1" ~tags:[ "d20"; "p1"; "slow" ]
  @@ fun () ->
  Check.((Day20.run 1 "resources/day20" = 831600) int)
    ~error_msg:"expected Day20.run 1 input = %R, got %L";
  Log.info "Day 20, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d20.p2" ~tags:[ "d20"; "p2"; "slow" ]
  @@ fun () ->
  Check.((Day20.run 2 "resources/day20" = 884520) int)
    ~error_msg:"expected Day20.run 2 input = %R, got %L";
  Log.info "Day 20, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d21.p1" ~tags:[ "d21"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day21.run 1 "resources/day21" = 78) int)
    ~error_msg:"expected Day21.run 1 input = %R, got %L";
  Log.info "Day 21, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d21.p2" ~tags:[ "d21"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day21.run 2 "resources/day21" = 148) int)
    ~error_msg:"expected Day21.run 2 input = %R, got %L";
  Log.info "Day 21, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d22.p1" ~tags:[ "d22"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day22.run 1 "resources/day22" = 953) int)
    ~error_msg:"expected Day22.run 1 input = %R, got %L";
  Log.info "Day 22, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d22.p2" ~tags:[ "d22"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day22.run 2 "resources/day22" = 1289) int)
    ~error_msg:"expected Day22.run 2 input = %R, got %L";
  Log.info "Day 22, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d23.p1" ~tags:[ "d23"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day23.run 1 "resources/day23" = 170) int)
    ~error_msg:"expected Day23.run 1 input = %R, got %L";
  Log.info "Day 23, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d23.p2" ~tags:[ "d23"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day23.run 2 "resources/day23" = 247) int)
    ~error_msg:"expected Day23.run 2 input = %R, got %L";
  Log.info "Day 23, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d24.p1" ~tags:[ "d24"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day24.run 1 "resources/day24" = 10439961859) int)
    ~error_msg:"expected Day24.run 1 input = %R, got %L";
  Log.info "Day 24, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d24.p2" ~tags:[ "d24"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day24.run 2 "resources/day24" = 72050269) int)
    ~error_msg:"expected Day24.run 2 input = %R, got %L";
  Log.info "Day 24, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d25.p1" ~tags:[ "d25"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day25.run 1 "resources/day25" = 9132360) int)
    ~error_msg:"expected Day25.run 1 input = %R, got %L";
  Log.info "Day 25, part 1, is correct.";
  unit

(* CALL the main function of Tezt so that it actually runs your tests. *)
let () = Test.run ()
