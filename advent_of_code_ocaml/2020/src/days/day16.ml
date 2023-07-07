open Mdrp_lib

let invalid i array = match array.(i) with v -> not v | exception _ -> true

let parse file =
  let ci = open_in file in
  let re_field =
    Str.regexp {|\([a-z ]+\): \([0-9]+\)-\([0-9]+\) or \([0-9]+\)-\([0-9]+\)|}
  in
  let rec aux_parse nearby fields max_value tickets ticket =
    match input_line ci with
    | "" -> aux_parse nearby fields max_value tickets ticket
    | "your ticket:" ->
        aux_parse nearby fields max_value tickets
          (input_line ci |> String.split_on_char ',' |> List.map int_of_string)
    | "nearby tickets:" -> aux_parse true fields max_value tickets ticket
    | s when nearby ->
        aux_parse nearby fields max_value
          ((s |> String.split_on_char ',' |> List.map int_of_string) :: tickets)
          ticket
    | s ->
        if Str.string_match re_field s 0 then
          let last = Str.matched_group 5 s |> int_of_string in
          aux_parse nearby
            (( Str.matched_group 1 s,
               ( ( Str.matched_group 2 s |> int_of_string,
                   Str.matched_group 3 s |> int_of_string ),
                 (Str.matched_group 4 s |> int_of_string, last) ) )
            :: fields)
            (max last max_value) tickets ticket
        else (
          Format.eprintf "%s@." s;
          assert false)
    | exception End_of_file -> (fields, max_value, tickets, ticket)
  in
  aux_parse false [] 0 [] []

let part_1 file =
  let fields, max_value, tickets, _ = parse file in
  let array = Array.init (max_value + 1) (fun _ -> false) in
  List.iter
    (fun (_, ((min1, max1), (min2, max2))) ->
      for i = min1 to max1 do
        array.(i) <- true
      done;
      for i = min2 to max2 do
        array.(i) <- true
      done)
    fields;
  List.fold_left
    (fun acc ticket ->
      List.fold_left
        (fun acc v -> if invalid v array then v + acc else acc)
        acc ticket)
    0 tickets

let invalid ticket dict_values =
  let rec aux = function
    | [] -> false
    | hd :: tl -> (
        match dict_values.(hd) with
        | s when String.Set.is_empty s -> true
        | _ -> aux tl
        | exception _ -> true)
  in
  aux ticket

let parse ci =
  let re_field =
    Str.regexp {|\([a-z ]+\): \([0-9]+\)-\([0-9]+\) or \([0-9]+\)-\([0-9]+\)|}
  in
  let rec aux_parse fields max_value ticket =
    match input_line ci with
    | "" -> aux_parse fields max_value ticket
    | "your ticket:" ->
        aux_parse fields max_value
          (input_line ci |> String.split_on_char ',' |> List.map int_of_string
         |> Array.of_list)
    | "nearby tickets:" -> (fields, max_value, ticket)
    | s ->
        if Str.string_match re_field s 0 then
          let last = Str.matched_group 5 s |> int_of_string in
          aux_parse
            (( Str.matched_group 1 s,
               ( ( Str.matched_group 2 s |> int_of_string,
                   Str.matched_group 3 s |> int_of_string ),
                 (Str.matched_group 4 s |> int_of_string, last) ) )
            :: fields)
            (max last max_value) ticket
        else (
          Format.eprintf "%s@." s;
          assert false)
    | exception End_of_file -> assert false
  in

  aux_parse [] 0 [||]

let propagate_singleton change i new_set dict_fields =
  let field = String.Set.choose new_set in
  Array.fold_lefti
    (fun j acc set ->
      if i <> j then
        if String.Set.mem field set then (
          dict_fields.(j) <- String.Set.remove field set;
          true)
        else acc
      else acc)
    change dict_fields

let handle_singletons dict_fields =
  let rec aux () =
    if
      Array.fold_lefti
        (fun i acc set ->
          if String.Set.cardinal set = 1 then
            propagate_singleton acc i set dict_fields
          else acc)
        false dict_fields
    then aux ()
    else ()
  in
  aux ()

let aux_intersect dict_fields dict_values ticket =
  let rec aux i = function
    | [] -> ()
    | hd :: tl ->
        let new_set = String.Set.inter dict_fields.(i) dict_values.(hd) in
        dict_fields.(i) <- new_set;
        aux (i + 1) tl
  in
  aux 0 ticket

let intersect ci dict_fields dict_values =
  let rec aux () =
    match input_line ci with
    | s ->
        let ticket = s |> String.split_on_char ',' |> List.map int_of_string in
        if invalid ticket dict_values then aux ()
        else (
          aux_intersect dict_fields dict_values ticket;
          handle_singletons dict_fields;
          if Array.for_all (fun s -> String.Set.cardinal s = 1) dict_fields then
            ()
          else aux ())
    | exception End_of_file -> assert false
  in
  aux ()

let part_2 file =
  let ci = open_in file in
  let fields, max_value, ticket = parse ci in

  let dict_values = Array.init (max_value + 1) (fun _ -> String.Set.empty) in
  let set_fields =
    List.fold_left
      (fun field_set (field, ((min1, max1), (min2, max2))) ->
        for i = min1 to max1 do
          dict_values.(i) <- String.Set.add field dict_values.(i)
        done;
        for i = min2 to max2 do
          dict_values.(i) <- String.Set.add field dict_values.(i)
        done;
        String.Set.add field field_set)
      String.Set.empty fields
  in
  let dict_fields = Array.init (Array.length ticket) (fun _ -> set_fields) in
  intersect ci dict_fields dict_values;
  Array.fold_lefti
    (fun i acc set ->
      match String.sub (String.Set.choose set) 0 9 with
      | "departure" -> ticket.(i) * acc
      | _ | (exception _) -> acc)
    1 dict_fields

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
