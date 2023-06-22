open Mdrp_lib

module Pod = struct
  type pod = Amber | Bronze | Copper | Desert
  type t = { pod : pod; mutable abs : int; mutable ord : int }

  let ( = ) t1 t2 = t1.pod = t2.pod

  let of_string = function
    | "A" -> Amber
    | "B" -> Bronze
    | "C" -> Copper
    | "D" -> Desert
    | _ -> failwith "Wrong pod character"

  let make s abs ord = { pod = of_string s; abs; ord }

  let compare t1 t2 =
    if t1 = t2 then
      let c = Int.compare t1.abs t2.abs in
      if Stdlib.(c = 0) then Int.compare t1.ord t2.ord else c
    else
      match (t1.pod, t2.pod) with
      | Amber, _ -> -1
      | _, Amber -> 1
      | Bronze, _ -> -1
      | _, Bronze -> 1
      | Copper, _ -> -1
      | _, Copper -> 1
      | _ -> assert false

  let pp ppf t =
    Format.fprintf ppf "{%s; abs: %d; ord: %d}"
      (match t.pod with
      | Amber -> "A"
      | Bronze -> "B"
      | Copper -> "C"
      | Desert -> "D")
      t.abs t.ord

  let pp_min ppf t =
    Format.fprintf ppf "%s"
      (match t.pod with
      | Amber -> "A"
      | Bronze -> "B"
      | Copper -> "C"
      | Desert -> "D")

  (* let energy t = *)
  (*   match t.pod with *)
  (*   | Amber -> 1 *)
  (*   | Bronze -> 10 *)
  (*   | Copper -> 100 *)
  (*   | Desert -> 1000 *)

  let energy s { pod; _ } =
    s
    *
    match pod with Amber -> 1 | Bronze -> 10 | Copper -> 100 | Desert -> 1000
end

let height = ref 0
let part_1 () = height := 2
let part_2 () = height := 4

module Grid = struct
  type cell = Empty | Wall | Pod of Pod.t

  (* t.(ord in [0..height]).(abs in [0..10]) *)
  type t = cell array array

  let pp_cell ppf = function
    | Empty -> Format.fprintf ppf "."
    | Wall -> Format.fprintf ppf "#"
    | Pod p -> Format.fprintf ppf "%a" Pod.pp_min p

  let pp ppf (t : t) =
    Format.fprintf ppf "@[<v 0>  ";
    for y = 0 to 10 do
      Format.fprintf ppf "%d" y
    done;
    Format.fprintf ppf "@,";
    Array.iteri
      (fun x a ->
        Format.fprintf ppf "%d|" x;
        Array.iter (fun p -> Format.fprintf ppf "%a" pp_cell p) a;
        Format.fprintf ppf "@,")
      t;
    Format.fprintf ppf "  ";
    for y = 0 to 10 do
      Format.fprintf ppf "%d" y
    done;
    Format.fprintf ppf "@]"

  let init file =
    let ci = open_in file in
    ignore (input_line ci);
    ignore (input_line ci);
    let grid = Array.make_matrix (1 + !height) 11 Wall in
    for abs = 0 to 10 do
      grid.(0).(abs) <- Empty
    done;
    let rec aux i pods =
      if i > !height then pods
      else
        let pods =
          let s = input_line ci in
          match String.split_on_char_non_empty '#' s with
          | [ p1; p2; p3; p4 ] | [ _; p1; p2; p3; p4 ] ->
              let p1 = Pod.make p1 2 i in
              let p2 = Pod.make p2 4 i in
              let p3 = Pod.make p3 6 i in
              let p4 = Pod.make p4 8 i in
              grid.(i).(2) <- Pod p1;
              grid.(i).(4) <- Pod p2;
              grid.(i).(6) <- Pod p3;
              grid.(i).(8) <- Pod p4;
              p1 :: p2 :: p3 :: p4 :: pods
          | _ -> assert false
        in
        aux (i + 1) pods
    in
    (grid, aux 1 [])

  let steps sabs sord eabs eord = abs (sabs - eabs) + sord + eord

  let abs_position = function
    | Pod t -> (
        match t.pod with
        | Pod.Amber -> 2
        | Bronze -> 4
        | Copper -> 6
        | Desert -> 8)
    | _ -> assert false

  let pod_from_abs_position = function
    | 2 -> Pod.Amber
    | 4 -> Bronze
    | 6 -> Copper
    | 8 -> Desert
    | _ -> assert false

  let rec aux_home (t : t) abs ord pod =
    if ord = 0 then (true, None)
    else
      match t.(ord).(abs) with
      | Empty -> (true, Some ord)
      | Pod p ->
          if Pod.(p = pod) then aux_home t abs (ord - 1) pod else (false, None)
      | _ -> failwith "aux_home"

  (* Checks that the cell is currently in its home *)
  let is_home (t : t) abs ord pod =
    ord > 0
    && abs = abs_position t.(ord).(abs)
    && aux_home t abs !height pod |> fst

  let interleave l1 l2 =
    let rec aux acc tl1 tl2 =
      match (tl1, tl2) with
      | [], l | l, [] -> List.rev_append acc l
      | hd1 :: tl1, hd2 :: tl2 -> aux (hd1 :: hd2 :: acc) tl1 tl2
    in
    aux [] l1 l2

  (* 1- Amphipods will never stop on the space immediately outside any room *)
  let valid_hallway (t : t) abs =
    let rec go_left acc abs =
      if abs < 0 || t.(0).(abs) <> Empty then acc
      else if abs = 0 || abs = 10 || abs mod 2 <> 0 then
        go_left ((abs, 0) :: acc) (abs - 1)
      else go_left acc (abs - 1)
    in
    let rec go_right acc abs =
      if abs > 10 || t.(0).(abs) <> Empty then acc
      else if abs = 0 || abs = 10 || abs mod 2 <> 0 then
        go_right ((abs, 0) :: acc) (abs + 1)
      else go_right acc (abs + 1)
    in
    go_right (go_left [] abs) abs
  (* interleave l1 l2 *)

  let rec free_top (t : t) abs ord =
    ord = 0
    || match t.(ord).(abs) with Empty -> free_top t abs (ord - 1) | _ -> false

  (* 2- Amphipods will never move into a room that is not their room or that *)
  (* contains a wrong amphipod *)
  (* pod is at abs ord *)
  (* - pod is already in its home then it needs to go out before the home *)
  (*   is not safe *)
  (* - Check that the hallway is empty between our pod and its home  *)
  (* - check that the home is either empty or only contains good pods *)
  let can_go_home (t : t) abs ord =
    match t.(ord).(abs) with
    | Pod p as pod ->
        let abs_pod = abs_position pod in
        if abs = abs_pod then None
        else
          let rec aux x limit =
            x = limit || (t.(0).(x) = Empty && aux (x + 1) limit)
          in
          if
            aux
              (if abs < abs_pod then abs + 1 else abs_pod + 1)
              (if abs < abs_pod then abs_pod else abs)
          then
            match aux_home t abs_pod !height p with
            | _, None -> None
            | _, Some ord -> Some [ (abs_pod, ord) ]
          else None
    | _ -> None

  (* 3- Amphipod can only stop once in the halfway. Once they go up, their next move will *)
  (*    be to go down *)

  let pod_moves (t : t) abs ord =
    match t.(ord).(abs) with
    | Pod pod -> (
        if
          is_home t abs ord pod || (ord > 1 && not (free_top t abs (ord - 1)))
          (* The Pod is already home or there's a pod above it, don't move it *)
        then None
        else
          (* The pod is either in the hallway and rule 2 applies, they can *)
          (* only go down to their home or can't move or *)
          (* The pod is in a room but it's either not its home or its *)
          (* not properly filled and there's nobody above it *)
          match can_go_home t abs ord with
          (* else if *)
          | None ->
              if ord = 1 || (ord > 1 && free_top t abs (ord - 1)) then
                match valid_hallway t abs with [] -> None | l -> Some l
              else None
          | d -> d)
    | _ -> assert false

  let move_pod (t : t) pod (eabs, eord) =
    assert (eabs <> 0 || eord = 0 || eord = 10 || eord mod 2 <> 0);
    let prev = Pod.(pod.abs, pod.ord) in
    t.(pod.ord).(pod.abs) <- Empty;
    pod.abs <- eabs;
    pod.ord <- eord;
    t.(eord).(eabs) <- Pod pod;
    prev

  let pod_equal p = function Pod pod -> pod.pod = p | _ -> false

  let is_valid (t : t) =
    pod_equal Pod.Amber t.(!height).(2)
    && pod_equal Pod.Bronze t.(!height).(4)
    && pod_equal Pod.Copper t.(!height).(6)
    && pod_equal Pod.Desert t.(!height).(8)
    && pod_equal Pod.Amber t.(1).(2)
    && pod_equal Pod.Bronze t.(1).(4)
    && pod_equal Pod.Copper t.(1).(6)
    && pod_equal Pod.Desert t.(1).(8)

  let closer pod (abs1, ord1) (abs2, ord2) =
    abs (pod.Pod.abs - abs1)
    + abs (pod.ord - ord1)
    - (abs (pod.abs - abs2) + abs (pod.ord - ord2))

  let moves t pods =
    List.fold_left
      (fun lmoves (Pod.{ abs; ord; _ } as pod) ->
        match pod_moves t abs ord with
        | None -> lmoves
        | Some moves ->
            List.fold_left
              (fun lmoves move -> (pod, move) :: lmoves)
              lmoves moves)
      [] pods
    |> List.fast_sort (fun (pod1, move1) (pod2, move2) ->
           let c = Pod.compare pod2 pod1 in
           if c = 0 then closer pod1 move1 move2 else c)

  let cost pod (abs, ord) =
    let open Pod in
    energy (steps pod.abs pod.ord abs ord) pod

  let h_cost (t : t) pods =
    List.fold_left
      (fun acc pod ->
        let abs_t = abs_position t.(pod.Pod.ord).(pod.abs) in
        if is_home t pod.abs pod.ord pod then acc
        else if abs_t = pod.abs then
          let c = cost pod (abs_t + 5, !height) in
          c + acc
        else
          let c = cost pod (abs_t, !height) in
          c + acc)
      0 pods

  let solve (t : t) pods =
    let rec solve min_cost min_path current_cost curr_path =
      if is_valid t then (current_cost, curr_path)
      else
        let moves = moves t pods in
        let min_cost, min_path, _, _ =
          List.fold_left
            (fun (min_cost, min_path, current_cost, curr_path) (pod, move) ->
              let cost = cost pod move + current_cost in

              if cost > min_cost then
                (min_cost, min_path, current_cost, curr_path)
              else
                let ppod = { pod with abs = pod.abs; ord = pod.ord } in
                let prev = move_pod t pod move in
                let min_cost, min_path =
                  solve min_cost min_path cost ((ppod, move) :: curr_path)
                in
                let _ = move_pod t pod prev in
                (min_cost, min_path, current_cost, curr_path))
            (min_cost, min_path, current_cost, curr_path)
            moves
        in
        (min_cost, min_path)
    in
    solve max_int [] 0 []

  let replay t moves =
    List.iter
      (fun (pod, move) ->
        (* Format.eprintf "%a to %a@." Pod.pp pod Pair.(pp Int.pp Int.pp) move; *)
        ignore (move_pod t pod move)
        (* Format.eprintf "@.%a@.@." pp t *))
      moves
end

let run part file =
  (match part with 1 -> part_1 () | 2 -> part_2 () | _ -> ());
  let grid, pods = Grid.init file in
  let min_cost, _min_path = Grid.solve grid pods in
  Format.printf "%d@." min_cost
