open Mdrp_lib
module SM = Map.Make (String)

module PS = Multiset.Make (struct
  type t = string * string

  let pp _ _ = failwith "TODO"
  let compare = compare
end)

module SS = Multiset.Make (String)

let split_on_arrow s =
  let open String in
  let buf = Buffer.create 2 in
  let rec aux i =
    let c = unsafe_get s i in
    if c = ' ' then
      (Buffer.contents buf, sub s (i + 4) (String.length s - i - 4))
    else (
      Buffer.add_char buf c;
      aux (i + 1))
  in
  aux 0

let part_1 start rules chars max_loops =
  (* We can't generate the formula because it takes too much space *)
  (* and we're going to run out of memory. *)
  (* The easiest way is to store each pair in a bag and fold on the pairs *)
  (* to create the new ones. *)
  (* We keep the number of newly created characters for the end *)
  let aux (bag, chars) =
    PS.fold
      (fun (s1, s2) cpt (bag, chars) ->
        let ns = SM.find (s1 ^ s2) rules in
        ( PS.add (s1, ns) ~mult:cpt (PS.add ~mult:cpt (ns, s2) bag),
          SS.add ns ~mult:cpt chars ))
      bag (PS.empty, chars)
  in
  let rec loop l (bag, chars) =
    if l > max_loops then
      SS.fold
        (fun _ cpt (minc, maxc) -> (min cpt minc, max cpt maxc))
        chars (max_int, min_int)
      |> fun (min, max) -> Format.printf "%d@." (max - min)
    else loop (l + 1) (aux (bag, chars))
  in
  loop 1 (start, chars)

let run part file rounds =
  let ci = open_in file in
  let start, chars =
    input_line ci
    |> String.fold
         (fun (list, chars) c ->
           let s = Char.to_string c in
           (s :: list, SS.add s chars))
         ([], SS.empty)
  in
  let start =
    let rec aux bag = function
      | s1 :: (s2 :: _ as l) -> aux (PS.add (s1, s2) bag) l
      | _ -> bag
    in
    aux PS.empty (List.rev start)
  in
  let rules =
    ignore (input_line ci);
    let rec aux_parse acc =
      match input_line ci with
      | s ->
          let pair, insert = split_on_arrow s in
          aux_parse (SM.add pair insert acc)
      | exception End_of_file ->
          close_in ci;
          acc
    in
    aux_parse SM.empty
  in
  match part with
  | 1 -> part_1 start rules chars 10
  | 2 -> part_1 start rules chars rounds
  | _ -> ()
