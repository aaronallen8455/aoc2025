let part_a inp =
  let rec read_lines acc res =
    try
      let line = input_line inp in
      let i = match line.[0] with
        | 'L' -> int_of_string (String.sub line 1 (String.length line - 1)) * -1
        | 'R' -> int_of_string (String.sub line 1 (String.length line - 1))
        | _ -> 0
      in
      let n = (acc + i) mod 100 in
      if n = 0
      then read_lines n (res + 1)
      else read_lines n res
    with End_of_file ->
      close_in inp;
      res
  in string_of_int (read_lines 50 0)

let part_b inp =
  let rec read_lines acc res =
    try
      let line = input_line inp in
      let i = match line.[0] with
        | 'L' -> int_of_string (String.sub line 1 (String.length line - 1)) * -1
        | 'R' -> int_of_string (String.sub line 1 (String.length line - 1))
        | _ -> 0
      in
      let n = acc + i in
      let modded = n mod 100 in
      let modded = if modded < 0 then modded + 100 else modded in
      let remain = if i < 0 then n else i - (100 - acc) in
      let divides = abs (remain / 100) in
      if (n <= 0 && acc != 0) || (remain >= 0 && i > 0)
      then read_lines modded (1 + divides + res)
      else read_lines modded (divides + res)
    with End_of_file ->
      close_in inp;
      res
  in string_of_int (read_lines 50 0)
