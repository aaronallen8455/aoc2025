let () =
  let day = Sys.argv.(1) in
  let part = Sys.argv.(2) in
  let input = In_channel.open_text (Printf.sprintf "input/d%s.txt" day) in
  let f = (match (day, part) with
    | ("1", "a") -> D1.part_a
    | ("1", "b") -> D1.part_b
    | ("2", "a") -> D2.part_a
    | ("2", "b") -> D2.part_b
    | ("3", "a") -> D3.part_a
    | ("3", "b") -> D3.part_b
    | ("4", "a") -> D4.part_a
    | ("4", "b") -> D4.part_b
    | ("5", "a") -> D5.part_a
    | ("5", "b") -> D5.part_b
    | _ -> (fun _ -> "unknown day")
  ) in
  print_endline (f input)
