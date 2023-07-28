(*
    - abc, bcd, cde, and so on, up to xyz
    - no i, o, or l
    - aa, bb ... or zz.
 *)

let increasing_3 b =
  let rec aux prev increasing i =
    i <= 8
    &&
    match Bytes.unsafe_get b i with
    | c when Char.code c = Char.code prev + 1 -> increasing || aux c true (i + 1)
    | c -> aux c false (i + 1)
  in
  aux (Bytes.unsafe_get b 0) false 1

let two_pairs b =
  let rec aux prev one_pair char_pair i =
    i <= 8
    &&
    match Bytes.unsafe_get b i with
    | c when Char.equal c prev ->
        (one_pair && not (Char.equal c (Option.get char_pair)))
        ||
        let i = i + 1 in
        i <= 7 && aux (Bytes.unsafe_get b i) true (Some c) (i + 1)
    | c -> aux c one_pair char_pair (i + 1)
  in
  aux (Bytes.unsafe_get b 0) false None 1

let check b = increasing_3 b && two_pairs b

let next_password b =
  let rec aux i =
    let c = Bytes.unsafe_get b i in
    match c with
    | 'z' ->
        Bytes.unsafe_set b i 'a';
        aux (i - 1)
    | 'h' | 'n' | 'k' ->
        for j = i + 1 to 7 do
          Bytes.unsafe_set b j 'a'
        done;
        Bytes.unsafe_set b i (Char.chr (Char.code c + 2));
        if not (check b) then aux 7
    | _ ->
        Bytes.unsafe_set b i (Char.chr (Char.code c + 1));
        if not (check b) then aux 7
  in
  aux 7

let normalize b =
  let rec aux i =
    if i = 8 then ()
    else
      match Bytes.unsafe_get b i with
      | ('i' | 'l' | 'o') as c ->
          for j = i + 1 to 7 do
            Bytes.unsafe_set b j 'a'
          done;
          Bytes.unsafe_set b i (Char.chr (Char.code c + 1))
      | _ -> aux (i + 1)
  in
  aux 0

let part_1 _file =
  let b = Bytes.of_string "vzbxkghb" in
  normalize b;
  next_password b;
  Format.eprintf "%s@." (Bytes.to_string b);
  0

let part_2 _file =
  let b = Bytes.of_string "vzbxkghb" in
  normalize b;
  next_password b;
  next_password b;
  Format.eprintf "%s@." (Bytes.to_string b);
  0

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
