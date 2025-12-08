module IS = Set.Make (struct
  type t = int
  let compare = compare
end
)

let all_idx s =
  let rec go i acc = match String.index_from_opt s i '^' with
    | None -> acc
    | Some x -> go (x + 1) (x :: acc)
  in go 0 []

let part_a inp =
  string_of_int @@ fst @@
  Util.fold_input
    (fun acc x ->
      let is = all_idx x in
      List.fold_left
        (fun (c, a) i ->
          if IS.mem i a
          then (c + 1, IS.add (i - 1) @@ IS.add (i + 1) @@ IS.remove i a)
          else (c, a)
        )
        acc is
    )
    (0, IS.singleton 70)
    inp

module IM = Map.Make (struct
  type t = int
  let compare = compare
end
)

let part_b inp =
  string_of_int @@ (fun m -> IM.fold (fun _ a aa -> a + aa) m 0) @@
  Util.fold_input
    (fun acc x ->
      let is = all_idx x in
      List.fold_left
        (fun a i ->
          match IM.find_opt i a with
          | None -> a
          | Some c ->
            IM.update (i - 1) (function
                | None -> Some c
                | Some d -> Some (c + d)
              )
            @@ IM.update (i + 1) (function
                | None -> Some c
                | Some d -> Some (c + d)
              )
            @@ IM.remove i a
        )
        acc is
    )
    (IM.singleton 70 1)
    inp

