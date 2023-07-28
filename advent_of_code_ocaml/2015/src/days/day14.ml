open Mdrp_lib

module Reindeer = struct
  type t = { id : int; speed : int; fly_time : int; rest : int }

  let compare t1 t2 = Int.compare t1.id t2.id

  let pp ppf { id; speed; fly_time; rest } =
    Format.fprintf ppf "{id: %d; speed: %d; fly_time: %d; rest: %d}" id speed
      fly_time rest

  let distance_traveled time t =
    let split = t.fly_time + t.rest in
    let nb_splits = time / split in
    let rest = time mod split in
    if rest <= t.fly_time then
      (nb_splits * t.speed * t.fly_time) + (rest * t.speed)
    else (nb_splits + 1) * t.speed * t.fly_time

  (* We don't need to compute the whole position, just to know
     if the reindeer is resting or moving forward.
     Look out for the cycle = 0 case, this is when the reindeer
     wakes up, he still hasn't moved on *)
  let delta_position time { speed; fly_time; rest; _ } =
    let cycle = time mod (fly_time + rest) in
    if cycle > 0 && cycle <= fly_time then speed else 0
end

module RSet = struct
  include Set.Make (Reindeer)

  let get_id id t = find Reindeer.{ id; speed = 0; fly_time = 0; rest = 0 } t
end

let parse =
  let id = ref 0 in
  ( (fun set line ->
      let re =
        Str.regexp
          {|[a-zA-Z]+ can fly \([0-9]+\) km/s for \([0-9]+\) seconds, but then must rest for \([0-9]+\) seconds.|}
      in
      if Str.string_match re line 0 then (
        let uid = !id in
        incr id;
        let speed = Str.matched_group 1 line |> int_of_string in
        let fly_time = Str.matched_group 2 line |> int_of_string in
        let rest = Str.matched_group 3 line |> int_of_string in
        RSet.add { id = uid; speed; fly_time; rest } set)
      else assert false),
    fun () -> !id )

let part_1 file =
  let parse, _ = parse in
  let set = Parse.fold_lines parse RSet.empty file in
  RSet.fold (fun t acc -> max acc (Reindeer.distance_traveled 2503 t)) set 0

let part_2 file =
  let open Reindeer in
  let parse, get_id = parse in
  let set = Parse.fold_lines parse RSet.empty file in
  let positions = Array.init (get_id ()) (fun _ -> 0) in
  let scores = Array.init (get_id ()) (fun _ -> 0) in
  let rec aux time (max_pos, ids) =
    if time > 2503 then Array.fold_left max 0 scores
    else
      let max_pos, ids =
        RSet.fold
          (fun reindeer ((max_pos, ids) as acc) ->
            let next_pos =
              positions.(reindeer.id) + Reindeer.delta_position time reindeer
            in
            positions.(reindeer.id) <- next_pos;
            if next_pos > max_pos then (next_pos, [ reindeer.id ])
            else if next_pos = max_pos then (max_pos, reindeer.id :: ids)
            else acc)
          set (max_pos, ids)
      in
      List.iter (fun id -> scores.(id) <- scores.(id) + 1) ids;
      aux (time + 1) (max_pos, [])
  in
  aux 1 (0, [])

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
