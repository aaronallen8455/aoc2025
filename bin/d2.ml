open Option

let find_max x =
  let hl = String.length x / 2 in
  let l = int_of_string (String.sub x 0 hl) in
  let r = int_of_string (String.sub x hl hl) in
  if l <= r then l else l - 1

let find_min x =
  let hl = String.length x / 2 in
  let l = int_of_string (String.sub x 0 hl) in
  let r = int_of_string (String.sub x hl hl) in
  if l >= r then l else l + 1

let l_bound i =
  "1" ^ String.make (i - 1) '0'

let l_base l =
  if String.length l < 2
  then "11"
  else
    if (String.length l) mod 2 = 0
    then l
    else let s = "1" ^ String.make (String.length l / 2) '0'
         in s ^ s

let u_base u =
  if String.length u < 2
  then None
  else Some begin
    if (String.length u) mod 2 = 0
    then u
    else String.make (String.length u - 1) '9'
  end

let batches l u : (int * int) Seq.t =
  Seq.unfold
    (function
      | (ll, _) when String.length ll = 0 -> None
      | (ll, rr) when String.length ll = String.length rr ->
          Some ((int_of_string ll, int_of_string rr), ("", ""))
      | (ll, rr) -> Some
        ((int_of_string ll, int_of_string (String.make (String.length ll) '9'))
        , (l_bound (String.length ll + 1), rr))
    )
    (string_of_int l, string_of_int u)

let add_to x = (int_of_float (float_of_int x ** 2.0) + x) / 2

let calc_part l u =
  let r = add_to u - add_to (l - 1) in
  let s = String.length (string_of_int l) in
  r + r * int_of_float (10.0 ** float_of_int s)

let calc r = match String.split_on_char '-' r with
  | [l; u] -> begin match (l_base l, u_base u) with
      | (lb, Some ub) ->
        let ll = find_min lb in
        let uu = find_max ub in
        if ll > uu
        then 0
        else
          Seq.fold_left (+) 0
            (Seq.map (fun (l, u) -> calc_part l u) (batches ll uu))
      | _ -> 0
    end
  | _ -> 0

let part_a inp =
  let line = input_line inp in
  let rngs = String.split_on_char ',' line in
  string_of_int (List.fold_left (fun acc x -> acc + calc x) 0 rngs)

let is_valid x =
  try
    for i = 1 to (String.length x / 2) do
      if String.length x mod i = 0
      then
        let d = String.length x / i in
        let s = String.sub x 0 i in
        let c = String.concat "" (List.init d (fun _ -> s)) in
        if c = x
        then raise Exit
    done;
    false
  with Exit -> true

let add_range s = match String.split_on_char '-' s with
  | [l ; r] ->
      let acc = ref 0 in
      for n = int_of_string l to int_of_string r do
        if is_valid (string_of_int n)
        then acc := !acc + n
      done;
      !acc
  | _ -> 0

let part_b inp =
  let line = input_line inp in
  let rngs = String.split_on_char ',' line in
  string_of_int
    (List.fold_left
      (fun acc x -> acc + add_range x)
      0 rngs
      )
