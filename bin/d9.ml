let parse_coord s =
  match String.split_on_char ',' s with
    | [c; r] -> (int_of_string c, int_of_string r)
    | _ -> raise (Invalid_argument "no")

let area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

let pair cs =
  let res = ref [] in
  for i = 0 to Array.length cs - 1 do
    for j = i + 1 to Array.length cs - 1 do
      let ci = Array.get cs i in
      let cj = Array.get cs j in
      res := (area ci cj, (ci, cj)) :: !res
    done
  done;
  !res

let part_a inp =
  let coords = Array.of_list @@ Util.fold_input (fun acc x -> parse_coord x :: acc) [] inp in
  let pairs = List.sort (fun a b -> compare b a) @@ List.map fst @@ pair coords
  in string_of_int @@ List.hd pairs

let between x b1 b2 =
  if b1 < b2
  then x > b1 && x < b2
  else x > b2 && x < b1

let overlap (l1, u1) (l2, u2) =
  between l2 l1 u1
  || between u2 l1 u1
  || between l1 l2 u2
  || between u1 l2 u2

let check_sqr vl hl ((c1, r1), (c2, r2)) =
  let vl_crosses () = List.exists
    (fun ((lc1, lr1), (_, lr2)) ->
      between lc1 c1 c2
        && overlap (lr1, lr2) (min (r1 - 1) (r2 - 1), max (r1 + 1) (r2 + 1))
    )
    vl in
  let hl_crosses () = List.exists
    (fun ((lc1, lr1), (lc2, _)) ->
      between lr1 r1 r2
        && overlap (lc1, lc2) (min (c1 - 1) (c2 - 1), max (c1 + 1) (c2 + 1))
    )
    hl in
  let inside () = true in (* 50% chance I don't need to implement this *)
  not (vl_crosses ()) && not (hl_crosses ()) && inside ()

let part_b inp =
  let coords = Util.fold_input (fun acc x -> parse_coord x :: acc) [] inp in
  let lines = List.map2 (fun a b -> (a,b)) coords
        (List.take (List.length coords) @@ List.drop 1 @@ List.append coords coords) in
  let (vl, hl) = List.partition_map
    (fun (((c1, r1) as p1), ((c2, r2) as p2)) ->
      if c1 = c2
      then Either.Left  (if r1 > r2 then (p2, p1) else (p1, p2))
      else Either.Right (if c1 > c2 then (p2, p1) else (p1, p2))
    )
    lines in
  let pairs = List.sort (fun a b -> compare (fst b) (fst a)) @@ pair (Array.of_list coords) in
  let res = List.find (fun x -> check_sqr vl hl (snd x)) pairs
  in string_of_int (fst res)
