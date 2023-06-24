open Mdrp_lib

module SM = struct
  include Map.Make (String)

  let update s c t =
    update s (function Some l -> Some (c :: l) | None -> Some [ c ]) t
end

module SS = Set.Make (String)

let pathes m =
  let rec fold (cpt, visited) = function
    | [] -> cpt
    | cave :: tl ->
        let cpt =
          match cave with
          | "end" -> cpt + 1
          | _ ->
              if
                (not (String.is_lowercase_ascii cave))
                || not (SS.mem cave visited)
                (* cave is either a big one (uppercase) or we never went in it *)
              then pathes_from cave (cpt, SS.add cave visited)
              else cpt
        in
        fold (cpt, visited) tl
  and pathes_from cave (cpt, visited) = fold (cpt, visited) (SM.find cave m) in
  pathes_from "start" (0, SS.singleton "start")

let part_1 m = pathes m

let pathes m =
  let rec fold (cpt, visited, twice) = function
    | [] -> cpt
    | cave :: tl ->
        let cpt =
          match cave with
          | "end" -> cpt + 1
          | _ ->
              if
                (not (String.is_lowercase_ascii cave))
                || not (SS.mem cave visited)
                (* cave is either a big one (uppercase) or we never went in it *)
              then pathes_from cave (cpt, SS.add cave visited, twice)
              else if
                (not twice) && cave <> "start"
                (* Now, if we went in it, if it's the first time a small cave is visited twice *)
                (* we can keep visiting *)
              then pathes_from cave (cpt, SS.add cave visited, true)
              else cpt
        in
        fold (cpt, visited, twice) tl
  and pathes_from cave (cpt, visited, twice) =
    fold (cpt, visited, twice) (SM.find cave m)
  in
  pathes_from "start" (0, SS.singleton "start", false)

let part_2 m = pathes m

let run part file =
  let m =
    Parse.fold_lines
      (fun acc s ->
        match String.split_on_char '-' s with
        | [ c1; c2 ] -> SM.update c1 c2 (SM.update c2 c1 acc)
        | _ -> assert false)
      SM.empty file
  in
  match part with 1 -> part_1 m | _ -> part_2 m
