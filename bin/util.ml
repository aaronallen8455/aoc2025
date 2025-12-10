let fold_input f acc inp =
  let rec go res =
    try
      let line = input_line inp in
      go (f res line)
    with End_of_file ->
      close_in inp;
      res
  in go acc

let break f xs =
  let rec go res b =
    match res with
    | [] -> (b, [])
    | x :: r ->
      if f x
      then (b, x :: r)
      else go r (x :: b)
  in let (b, a) = go xs []
  in (List.rev b, a)
