open Mdrp_lib

let f w sum1 sumw z divz =
  if (z mod 26) + sum1 <> w then (z / divz * 26) + w + sumw else z / divz

let rec compute_input z sum1 sumw divz input =
  match (sum1, sumw, divz, input) with
  | sum1 :: tl1, sumw :: tlw, divz :: tlz, w :: input ->
      (* Format.eprintf "%d: sum1: %d sumw: %d divz: %d " w sum1 sumw divz; *)
      let z = f w sum1 sumw z divz in
      (* Format.eprintf "%d@." z; *)
      compute_input z tl1 tlw tlz input
  | _ -> z

let rec relations ppf i prevsw sum1 sumw divz prev26 =
  match (sum1, sumw, divz) with
  | [], [], [] -> ()
  | sum1 :: tl1, sumw :: tlw, 1 :: tlz when i = 1 && sum1 > 9 ->
      Format.fprintf ppf "(assert (= z%d (+ w%d %d)))@." i i sumw;
      relations ppf (i + 1) sumw tl1 tlw tlz false
  | sum1 :: tl1, sumw :: tlw, 1 :: tlz when sum1 > 9 ->
      Format.fprintf ppf "(assert (= z%d (+ (* 26 z%d) w%d %d)))@." i (i - 1) i
        sumw;
      relations ppf (i + 1) sumw tl1 tlw tlz false
  | sum1 :: tl1, sumw :: tlw, 26 :: tlz when sum1 < 0 && prev26 ->
      Format.fprintf ppf "(assert (= z%d (div z%d 26)))@." i (i - 1);
      Format.fprintf ppf "(assert (= w%d (- (mod z%d 26) %d)))@." i (i - 1)
        (-sum1);
      relations ppf (i + 1) sumw tl1 tlw tlz true
  | sum1 :: tl1, sumw :: tlw, 26 :: tlz when sum1 < 0 && not prev26 ->
      Format.fprintf ppf "(assert (= z%d (div z%d 26)))@." i (i - 1);
      let sum = -(prevsw + sum1) in
      Format.fprintf ppf "(assert (= w%d (- w%d %d)))@." i (i - 1) sum;

      relations ppf (i + 1) sumw tl1 tlw tlz true
  | _ -> assert false

let () =
  (* let part = try Sys.argv.(1) with _ -> "2" in *)
  let file = try Sys.argv.(2) with _ -> "input" in

  let sum1, sumw, divz, _ =
    Parse.fold_lines
      (fun (sum1, sumw, divz, nextw) s ->
        match String.split_on_char ' ' s with
        | [ "div"; "z"; v ] -> (sum1, sumw, int_of_string v :: divz, nextw)
        | [ "add"; "y"; "w" ] -> (sum1, sumw, divz, true)
        | [ "add"; "y"; v ] when nextw ->
            (sum1, int_of_string v :: sumw, divz, false)
        | [ "add"; "x"; v ] -> (
            match int_of_string v with
            | exception _ -> (sum1, sumw, divz, nextw)
            | v -> (v :: sum1, sumw, divz, nextw))
        | _ -> (sum1, sumw, divz, nextw))
      ([], [], [], false) file
  in
  let sum1, sumw, divz = (List.rev sum1, List.rev sumw, List.rev divz) in
  let co = open_out (file ^ ".out") in
  let ppf = Format.formatter_of_out_channel co in
  Format.fprintf ppf "(declare-fun z0 () Int)@.";
  for i = 1 to 14 do
    Format.fprintf ppf "(declare-fun z%d () Int)@." i;
    Format.fprintf ppf "(declare-fun w%d () Int)@." i
  done;
  Format.fprintf ppf "; Constraints@.";
  Format.fprintf ppf "(assert (= z0 0))@.";
  Format.fprintf ppf "(assert (= z14 0))@.";
  for i = 1 to 14 do
    Format.fprintf ppf "(assert (>= w%d 1))@." i;
    Format.fprintf ppf "(assert (<= w%d 9))@." i
  done;
  relations ppf 1 0 sum1 sumw divz false;
  Format.fprintf ppf "(check-sat)@.";
  Format.fprintf ppf "(get-model)@.";
  compute_input 0 sum1 sumw divz (Int.Decimal.to_digits 91131151917893)
  |> Format.fprintf ppf "%d@.";
  Format.eprintf "See %s@." (file ^ ".out")
