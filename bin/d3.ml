let find_max =
  Seq.fold_lefti
    (fun (c, ci) i x -> if x > c then (x, i) else (c, ci))
    ('0', 0)

let rec do_bank n s =
  if n = 0 then Seq.empty else
  let (m1, m1i) = find_max s in
  let l = Seq.length s - 1 in
  if l - m1i < n - 1
  then Seq.append (do_bank (n - 1 - (l - m1i)) (Seq.take m1i s))
     (Seq.cons m1 (do_bank (l - m1i) (Seq.drop (m1i + 1) s)))
  else Seq.cons m1 (do_bank (n - 1) (Seq.drop (m1i + 1) s))

let int_from_seq s = int_of_string (String.of_seq s)

let part_a inp =
  string_of_int (Util.fold_input (fun acc x -> int_from_seq (do_bank 2 (String.to_seq x)) + acc) 0 inp)

let part_b inp =
  string_of_int (Util.fold_input (fun acc x -> int_from_seq (do_bank 12 (String.to_seq x)) + acc) 0 inp)
