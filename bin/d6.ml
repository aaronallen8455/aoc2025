let transpose xss =
  List.fold_right
    (fun xs acc ->
      List.map2 (fun x a -> x :: a) xs acc
    )
    xss
    (List.init (List.length (List.hd xss)) (fun _ -> []))

let do_prob p = match p with
  | "+" :: ns -> List.fold_left ( + ) 0 @@ List.map (fun x -> int_of_string x) ns
  | "*" :: ns -> List.fold_left ( * ) 1 @@ List.map (fun x -> int_of_string x) ns
  | _ -> 0

let part_a inp =
  let ps = transpose @@
    Util.fold_input
      (fun acc x ->
        let r = List.filter (fun x -> x <> "") @@ String.split_on_char ' ' x
        in r :: acc
      )
      []
      inp
  in string_of_int
  @@ List.fold_left (+) 0
  @@ List.map do_prob ps

let do_prob2 p = match p with
  | "+" :: ns -> List.fold_left ( + ) 0 @@ List.map (fun x -> int_of_string x) ns
  | "*" :: ns -> List.fold_left ( * ) 1 @@ List.map (fun x -> int_of_string x) ns
  | _ -> 0

let do_op mop stack = match mop with
  | Some '+' -> List.fold_left ( + ) 0 stack
  | Some '*' -> List.fold_left ( * ) 1 stack
  | _ -> 0

let go (res, (stack, mop)) x =
  let parse_num r ss =
    let s = String.trim @@ String.of_seq @@ List.to_seq @@ List.rev r in
    if s = "" then ss else int_of_string s :: ss
  in
  match x with
  | '+' :: rest ->
    (res + do_op mop stack
    , (parse_num rest [], Some '+')
    )
  | '*' :: rest ->
    (res + do_op mop stack
    , (parse_num rest [], Some '*')
    )
  | ns ->
    (res
    , (parse_num ns stack, mop)
    )

let part_b inp =
  let ps = transpose @@
    Util.fold_input
      (fun acc x -> List.of_seq (String.to_seq x) :: acc
      )
      []
      inp
  in string_of_int
  @@ (fun (res, (stack, mop)) -> res + do_op mop stack)
  @@ List.fold_left go (0, ([], None)) ps

