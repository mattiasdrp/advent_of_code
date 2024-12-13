open Tezt
open Tezt.Base
open Days

(* Run with: dune exec test/main.exe -- fast *)

let () =
  Test.register ~__FILE__ ~title:"d01.p1" ~tags:[ "d01"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day01.run 1 "resources/day01" = 1603498) int)
    ~error_msg:"expected Day01.run 1 input = %R, got %L";
  Log.info "Day 01, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d01.p2" ~tags:[ "d01"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day01.run 2 "resources/day01" = 25574739) int)
    ~error_msg:"expected Day01.run 2 input = %R, got %L";
  Log.info "Day 01, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d02.p1" ~tags:[ "d02"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day02.run 1 "resources/day02" = 218) int)
    ~error_msg:"expected Day02.run 1 input = %R, got %L";
  Log.info "Day 02, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d02.p2" ~tags:[ "d02"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day02.run 2 "resources/day02" = 290) int)
    ~error_msg:"expected Day02.run 2 input = %R, got %L";
  Log.info "Day 02, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d04.p1" ~tags:[ "d04"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day04.run 1 "resources/day04" = 2532) int)
    ~error_msg:"expected Day04.run 1 input = %R, got %L";
  Log.info "Day 04, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d04.p2" ~tags:[ "d04"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day04.run 2 "resources/day04" = 1941) int)
    ~error_msg:"expected Day04.run 2 input = %R, got %L";
  Log.info "Day 04, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d05.p1" ~tags:[ "d05"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day05.run 1 "resources/day05" = 5108) int)
    ~error_msg:"expected Day05.run 1 input = %R, got %L";
  Log.info "Day 05, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d05.p2" ~tags:[ "d05"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day05.run 2 "resources/day05" = 7380) int)
    ~error_msg:"expected Day05.run 2 input = %R, got %L";
  Log.info "Day 05, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d06.p1" ~tags:[ "d06"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day06.run 1 "resources/day06" = 4696) int)
    ~error_msg:"expected Day06.run 1 input = %R, got %L";
  Log.info "Day 06, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d06.p2" ~tags:[ "d06"; "p2"; "slow" ]
  @@ fun () ->
  Check.((Day06.run 2 "resources/day06" = 1443) int)
    ~error_msg:"expected Day06.run 2 input = %R, got %L";
  Log.info "Day 06, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d07.p1" ~tags:[ "d07"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day07.run 1 "resources/day07" = 975671981569) int)
    ~error_msg:"expected Day07.run 1 input = %R, got %L";
  Log.info "Day 07, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d07.p2" ~tags:[ "d07"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day07.run 2 "resources/day07" = 223472064194845) int)
    ~error_msg:"expected Day07.run 2 input = %R, got %L";
  Log.info "Day 07, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d08.p1" ~tags:[ "d08"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day08.run 1 "resources/day08" = 348) int)
    ~error_msg:"expected Day08.run 1 input = %R, got %L";
  Log.info "Day 08, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d08.p2" ~tags:[ "d08"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day08.run 2 "resources/day08" = 1221) int)
    ~error_msg:"expected Day08.run 2 input = %R, got %L";
  Log.info "Day 08, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d09.p1" ~tags:[ "d09"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day09.run 1 "resources/day09" = 6288599492129) int)
    ~error_msg:"expected Day09.run 1 input = %R, got %L";
  Log.info "Day 09, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d09.p2" ~tags:[ "d09"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day09.run 2 "resources/day09" = 6321896265143) int)
    ~error_msg:"expected Day09.run 2 input = %R, got %L";
  Log.info "Day 09, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d10.p1" ~tags:[ "d10"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day10.run 1 "resources/day10" = 698) int)
    ~error_msg:"expected Day10.run 1 input = %R, got %L";
  Log.info "Day 10, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d10.p2" ~tags:[ "d10"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day10.run 2 "resources/day10" = 1436) int)
    ~error_msg:"expected Day10.run 2 input = %R, got %L";
  Log.info "Day 10, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d11.p1" ~tags:[ "d11"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day11.run 1 "resources/day11" = 218956) int)
    ~error_msg:"expected Day11.run 1 input = %R, got %L";
  Log.info "Day 11, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d11.p2" ~tags:[ "d11"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day11.run 2 "resources/day11" = 259593838049805) int)
    ~error_msg:"expected Day11.run 2 input = %R, got %L";
  Log.info "Day 11, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d12.p1" ~tags:[ "d12"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day12.run 1 "resources/day12" = 1371306) int)
    ~error_msg:"expected Day12.run 1 input = %R, got %L";
  Log.info "Day 12, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d12.p2" ~tags:[ "d12"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day12.run 2 "resources/day12" = 805880) int)
    ~error_msg:"expected Day12.run 2 input = %R, got %L";
  Log.info "Day 12, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d13.p1" ~tags:[ "d13"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day13.run 1 "resources/day13" = 34787) int)
    ~error_msg:"expected Day13.run 1 input = %R, got %L";
  Log.info "Day 13, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d13.p2" ~tags:[ "d13"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day13.run 2 "resources/day13" = 85644161121698) int)
    ~error_msg:"expected Day13.run 2 input = %R, got %L";
  Log.info "Day 13, part 2, is correct.";
  unit

(* CALL the main function of Tezt so that it actually runs your tests. *)
let () = Test.run ()
