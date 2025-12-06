let get r c (a : 'a array array) =
  try Some (Array.get (Array.get a r) c)
  with Invalid_argument _ -> None

let adj r c =
  [ (r-1, c-1); (r-1, c); (r-1, c+1)
    ; (r, c-1); (r, c+1)
    ; (r+1, c-1); (r+1, c); (r+1, c+1)
  ]

let part_a inp =
  let a = Array.of_list @@ List.rev
    (Util.fold_input
      (fun acc x -> Array.of_seq (String.to_seq x) :: acc)
      []
      inp
    ) in
  let res = Array.mapi
    (fun ri r ->
      Array.mapi
        (fun ci x ->
          match x with
          | '@' ->
            let nadj =
              List.length @@
              List.filter
                (fun (r,c) -> get r c a = Some '@')
              @@ adj ri ci
            in if nadj < 4
            then 1
            else 0
          | _ -> 0
        ) r
    )
    a in
  string_of_int (Array.fold_left (fun acc x ->
    Array.fold_left (+) acc x) 0 res)

let part_b inp =
  let a = Array.of_list @@ List.rev
    (Util.fold_input
      (fun acc x -> Array.of_seq (String.to_seq x) :: acc)
      []
      inp
    ) in
  let rec res ar n =
    let nn = ref 0 in
    let () = Array.mapi_inplace
      (fun ri r ->
        Array.mapi
          (fun ci x ->
            match x with
            | '@' ->
              let nadj =
                List.length @@
                List.filter
                  (fun (r,c) -> get r c a = Some '@')
                @@ adj ri ci
              in if nadj < 4
              then (nn := !nn + 1; '.')
              else x
            | _ -> x
          ) r
      ) ar in
    if !nn = 0
    then n
    else res ar (n + !nn)
  in string_of_int (res a 0)
