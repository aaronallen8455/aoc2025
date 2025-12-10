let dist (x1, y1, z1) (x2, y2, z2) =
  let dx = x1 - x2 in
  let dy = y1 - y2 in
  let dz = z1 - z2 in
  dx * dx + dy * dy + dz * dz

let parse_coord s =
  match String.split_on_char ',' s with
    | [x; y; z] -> (int_of_string x, int_of_string y, int_of_string z)
    | _ -> raise (Invalid_argument "no")

let all_dists cs =
  let res = ref [] in
  for i = 0 to Array.length cs - 1 do
    for j = i + 1 to Array.length cs - 1 do
      let ci = Array.get cs i in
      let cj = Array.get cs j in
      res := (dist ci cj, (ci, cj)) :: !res
    done
  done;
  !res

module S = Set.Make( struct
  type t = (int * int * int)
  let compare = compare
end
)

let connect cs b1 b2 =
  match Util.break (fun x -> S.mem b1 x || S.mem b2 x) cs with
  | (_, []) -> S.of_list [b1; b2] :: cs
  | (h, x :: a) ->
    match ((S.mem b1 x : bool), (S.mem b2 x : bool)) with
      | (true, true) -> cs
      | (true, false) ->
        (match Util.break (S.mem b2) a with
          | (_, []) -> List.append h @@ (S.add b2 x) :: a
          | (hh, y :: aa) -> (S.union x y) :: List.append h (List.append hh aa))
      | (false, true) ->
        (match Util.break (S.mem b1) a with
          | (_, []) -> List.append h @@ (S.add b1 x) :: a
          | (hh, y :: aa) -> (S.union x y) :: List.append h (List.append hh aa))
      | _ -> cs

let part_a inp =
  let coords = Util.fold_input (fun acc x -> parse_coord x :: acc) [] inp in
  let sorted = List.map snd @@ List.take 1000 @@ List.sort compare
    @@ all_dists (Array.of_list coords) in
  let res = List.fold_left (fun acc (x, y) -> connect acc x y) [] sorted in
  let res2 = List.take 3 @@ List.sort (fun a b -> compare b a) @@ List.map S.cardinal res in
  string_of_int @@ List.fold_left ( * ) 1 res2

let part_b inp =
  let coords = Util.fold_input (fun acc x -> parse_coord x :: acc) [] inp in
  let sorted = List.map snd @@ List.sort compare
    @@ all_dists (Array.of_list coords) in
  let exception Found of int in
  try
    let _ = List.fold_left
      (fun acc (((x1,_,_) as x), ((x2,_,_) as y)) ->
        match connect acc x y with
        | [_] -> raise (Found (x1 * x2))
        | r -> r
      )
      (List.map S.singleton coords) sorted in
    ""
  with
    | Found r -> string_of_int r
