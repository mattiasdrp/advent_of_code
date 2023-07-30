open Mdrp_lib

module Sue = struct
  type t = {
    id : int;
    children : int option;
    cats : int option;
    samoyeds : int option;
    pomeranians : int option;
    akitas : int option;
    vizslas : int option;
    goldfish : int option;
    trees : int option;
    cars : int option;
    perfumes : int option;
  }

  let of_list id l =
    let sue =
      {
        id;
        children = None;
        cats = None;
        samoyeds = None;
        pomeranians = None;
        akitas = None;
        vizslas = None;
        goldfish = None;
        trees = None;
        cars = None;
        perfumes = None;
      }
    in
    List.fold_left
      (fun sue (id, v) ->
        match id with
        | "children" -> { sue with children = Some v }
        | "cats" -> { sue with cats = Some v }
        | "samoyeds" -> { sue with samoyeds = Some v }
        | "pomeranians" -> { sue with pomeranians = Some v }
        | "akitas" -> { sue with akitas = Some v }
        | "vizslas" -> { sue with vizslas = Some v }
        | "goldfish" -> { sue with goldfish = Some v }
        | "trees" -> { sue with trees = Some v }
        | "cars" -> { sue with cars = Some v }
        | "perfumes" -> { sue with perfumes = Some v }
        | _ -> assert false)
      sue l

  let compare t1 t2 = Int.compare t1.id t2.id

  let equal_field f1 f2 =
    match (f1, f2) with
    | _, None | None, _ -> true
    | Some v1, Some v2 -> Int.equal v1 v2

  let pp ppf t =
    Format.fprintf ppf
      "id : %d; children : %a; cats : %a; samoyeds : %a; pomeranians : %a; \
       akitas : %a; vizslas : %a; goldfish : %a; trees : %a; cars : %a; \
       perfumes : %a;"
      t.id
      Option.(pp Int.pp)
      t.children
      Option.(pp Int.pp)
      t.cats
      Option.(pp Int.pp)
      t.samoyeds
      Option.(pp Int.pp)
      t.pomeranians
      Option.(pp Int.pp)
      t.akitas
      Option.(pp Int.pp)
      t.vizslas
      Option.(pp Int.pp)
      t.goldfish
      Option.(pp Int.pp)
      t.trees
      Option.(pp Int.pp)
      t.cars
      Option.(pp Int.pp)
      t.perfumes

  let equal t1 t2 =
    equal_field t1.children t2.children
    && equal_field t1.cats t2.cats
    && equal_field t1.samoyeds t2.samoyeds
    && equal_field t1.pomeranians t2.pomeranians
    && equal_field t1.akitas t2.akitas
    && equal_field t1.vizslas t2.vizslas
    && equal_field t1.goldfish t2.goldfish
    && equal_field t1.trees t2.trees
    && equal_field t1.cars t2.cars
    && equal_field t1.perfumes t2.perfumes

  let compare_field pred f1 f2 =
    match (f1, f2) with
    | _, None | None, _ -> true
    | Some v1, Some v2 -> pred v1 v2

  let equal_v2 t1 t2 =
    equal_field t1.children t2.children
    && equal_field t1.samoyeds t2.samoyeds
    && equal_field t1.akitas t2.akitas
    && equal_field t1.vizslas t2.vizslas
    && equal_field t1.cars t2.cars
    && equal_field t1.perfumes t2.perfumes
    && compare_field (fun v1 v2 -> v1 < v2) t1.cats t2.cats
    && compare_field (fun v1 v2 -> v1 < v2) t1.trees t2.trees
    && compare_field (fun v1 v2 -> v1 > v2) t1.pomeranians t2.pomeranians
    && compare_field (fun v1 v2 -> v1 > v2) t1.goldfish t2.goldfish
end
