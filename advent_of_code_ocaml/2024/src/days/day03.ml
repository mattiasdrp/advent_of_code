let common_part file part =
  let mul_do_dont_regexp =
    {|mul\((\d+),(\d+)\)|(do(n't)?)|} |> Re.Perl.re |> Re.compile
  in
  Mdrp_lib.Parse.fold_lines
    (fun (acc, do_mul) line ->
      let groups = Re.Seq.all mul_do_dont_regexp line in
      Seq.fold_left
        (fun (acc, do_mul) group ->
          match Re.Group.get group 0 with
          | "don't" -> (acc, false)
          | "do" -> (acc, true)
          | _ when part = 1 || do_mul ->
              (* This will always be enabled if doing part 1 without reading do_mul *)
              let left = Re.Group.get group 1 |> int_of_string in
              let right = Re.Group.get group 2 |> int_of_string in
              (acc + (left * right), do_mul)
          | _ -> (acc, do_mul))
        (acc, do_mul) groups)
    (0, true) file
  |> fst

let run part file = common_part file part
