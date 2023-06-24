open Mdrp_lib

let parse_matrix ci =
  match input_line ci with
  | _ -> Some (List.init 5 (fun _ -> input_line ci))
  | exception End_of_file -> None

exception Result of int array array * int

(* Updates the deletion in row and column and set the content to 0 since we're suming *)
let update_array a i j =
  let v = a.(i).(j) in
  a.(i).(j) <- 0;
  a.(0).(j) <- a.(0).(j) - 1;
  a.(i).(0) <- a.(i).(0) - 1;
  if a.(0).(j) = 0 || a.(i).(0) = 0 then raise (Result (a, v))

let part_1 map numbers matrices =
  try
    List.iter
      (fun v ->
        match Int.Map.find_opt v map with
        | Some idmap ->
            Int.Map.iter
              (fun id l ->
                List.iter (fun (i, j) -> update_array matrices.(id) i j) l)
              idmap
        | None -> ())
      numbers;
    assert false
  with Result (a, called) ->
    Array.fold_lefti
      (fun i acc a ->
        if i = 0 then acc
        else
          Array.fold_lefti (fun i acc v -> if i = 0 then acc else acc + v) acc a)
      0 a
    * called

(* Keep a set of all finished boards so once we reach the last one we return its value *)
let part_2 map numbers matrices =
  let exception Res of int in
  let bound = Array.length matrices in
  try
    ignore
      (List.fold_left
         (fun finished v ->
           match Int.Map.find_opt v map with
           | Some idmap ->
               Int.Map.fold
                 (fun id l finished ->
                   List.fold_left
                     (fun finished (i, j) ->
                       try
                         update_array matrices.(id) i j;
                         finished
                       with Result (a, called) ->
                         let finished = Int.Set.add id finished in
                         if Int.Set.cardinal finished = bound then
                           raise
                             (Res
                                (Array.fold_lefti
                                   (fun i acc a ->
                                     if i = 0 then acc
                                     else
                                       Array.fold_lefti
                                         (fun i acc v ->
                                           if i = 0 then acc else acc + v)
                                         acc a)
                                   0 a
                                * called))
                         else finished)
                     finished l)
                 idmap finished
           | None -> finished)
         Int.Set.empty numbers);
    assert false
  with Res i -> i

let run part file =
  let numbers, map, matrices =
    let ci = open_in file in
    let numbers =
      String.split_on_char ',' (input_line ci) |> List.map int_of_string
    in

    (* From rows of space separated numbers to lists of lists of lists of numbers *)
    (* 3D storing of 2D matrices *)
    let matrices =
      let rec get_matrix acc =
        match parse_matrix ci with
        | None -> acc
        | Some matrix ->
            get_matrix
              (let l =
                 List.map
                   (fun s ->
                     let l =
                       List.map int_of_string
                         (String.split_on_char_non_empty ' ' s)
                     in
                     List.length l :: l)
                   matrix
               in
               let length = List.length (List.hd l) in
               (List.init length (fun _ -> length - 1) :: l) :: acc)
      in
      get_matrix []
    in
    close_in ci;
    (* Fills a map with every position on each numbers to find them rapidly when *)
    (* solving the bingo *)
    (* If 24 is in the first matrix at (3, 2) and the third at (1, 4) the map will contain *)
    (* 24 -> 1 -> (3, 2) *)
    (*       3 -> (1, 4) *)
    (* Since the matrics are in an array, they are easily accessible *)
    (* The map is Map of values -> Map of array ids -> Set of coordinates *)
    let fill_map map array id =
      let rows = Array.length array - 1 in
      let cols = Array.length array.(0) - 1 in
      let rec aux map i j =
        if i > rows then map
        else if j > cols then aux map (i + 1) 1
        else
          let v = Array.unsafe_get (Array.unsafe_get array i) j in
          let map =
            Int.Map.update v
              (function
                | Some idmap ->
                    (* Map of ids -> Set of coordinates *)
                    Some
                      (Int.Map.update id
                         (function
                           | Some coords -> Some ((i, j) :: coords)
                           | None -> Some [ (i, j) ])
                         idmap)
                | None -> Some (Int.Map.singleton id [ (i, j) ]))
              map
          in
          aux map i (j + 1)
      in
      (* Starting at second row and second column because the first row and first *)
      (* column track the removed numbers and are not proper values *)
      aux map 1 1
    in

    (* Generate the map from the array of boards *)
    let map, matrices =
      let map, list =
        List.fold_lefti
          (fun (map, list) i matrix ->
            let matrix = Array.of_list (List.map Array.of_list matrix) in
            let map = fill_map map matrix i in
            (map, matrix :: list))
          (Int.Map.empty, []) matrices
      in
      (map, Array.of_list (List.rev list))
    in
    (numbers, map, matrices)
  in
  match part with
  | 1 -> part_1 map numbers matrices
  | _ -> part_2 map numbers matrices
