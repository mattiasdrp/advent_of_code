open Tezt
open Tezt.Base
open Days

let () =
  Test.register ~__FILE__ ~title:"d22.p2" ~tags:[ "d22"; "p2"; "fast" ]
  @@ fun () ->
  Check.((Day22.run 2 "resources/day22" = 142228) int)
    ~error_msg:"expected Day24.run 2 input = %R, got %L";
  Log.info "Day 24, part 2, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d24.p1" ~tags:[ "d24"; "p1"; "fast" ]
  @@ fun () ->
  Check.((Day24.run 1 "resources/day24" = 230) int)
    ~error_msg:"expected Day24.run 1 input = %R, got %L";
  Log.info "Day 24, part 1, is correct.";
  unit

let () =
  Test.register ~__FILE__ ~title:"d24.p2" ~tags:[ "d24"; "p2"; "slow" ]
  @@ fun () ->
  Check.((Day24.run 2 "resources/day24" = 713) int)
    ~error_msg:"expected Day24.run 2 input = %R, got %L";
  Log.info "Day 24, part 2, is correct.";
  unit

(* CALL the main function of Tezt so that it actually runs your tests. *)
let () = Test.run ()
