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

(* CALL the main function of Tezt so that it actually runs your tests. *)
let () = Test.run ()
