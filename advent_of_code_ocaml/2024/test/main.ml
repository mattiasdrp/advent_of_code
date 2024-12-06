open Tezt
open Tezt.Base
open Days

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

(* CALL the main function of Tezt so that it actually runs your tests. *)
let () = Test.run ()
