let parse_rng s =
  match String.split_on_char '-' s with
  | [a; b] -> Some (int_of_string a, int_of_string b)
  | _ -> None

let part_a inp =
  let (rgs, ids) = Util.fold_input
      (fun ((rgs, ids) as acc) x ->
        match x with
        | "" -> acc
        | _ -> match parse_rng x with
          | None -> (rgs, int_of_string x :: ids)
          | Some r -> (r :: rgs, ids)
      )
      ([], [])
      inp
  in string_of_int @@
      List.length @@
      List.filter (fun i -> List.exists (fun (l, u) -> i >= l && i <= u) rgs) ids

let comb (l1, u1) (l2, u2) =
  if l2 > u1 then [(l2, u2); (l1, u1)] else
  if u2 <= u1 then [(l1, u1)]
  else [(l1, u2)]

let part_b inp =
  let rgs = Util.fold_input
      (fun rgs x ->
        match parse_rng x with
          | None -> rgs
          | Some r -> r :: rgs
      )
      []
      inp in
  let sorted = List.sort compare rgs
  in string_of_int
  @@ List.fold_left (+) 0
  @@ List.map (fun (l, u) -> u - l + 1)
  @@ List.fold_left
        (fun acc x -> match acc with
          | [] -> [x]
          | (h :: rs) -> List.append (comb h x) rs
        )
        []
        sorted
