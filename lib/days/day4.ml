let parse input =
  let parseline line = String.to_seq line |> Array.of_seq in
  input |> String.split_on_char '\n' |> List.map parseline |> Array.of_list

(*Starting from X, find all XMAS instances*)

let in_bounds r c g =
  0 <= r && r < Array.length g && 0 <= c && c < Array.length g.(0)

let letter_match l r c g = l = g.(r).(c)

let rec find_word (w : char list) (dir : int * int) r c g =
  if in_bounds r c g then
    match w with
    | [] -> 0
    | [ last ] -> if letter_match last r c g then 1 else 0
    | head :: tail ->
        if letter_match head r c g then
          find_word tail dir (r + fst dir) (c + snd dir) g
        else 0
  else 0

let neighbors =
  [ (0, -1); (-1, 0); (0, 1); (1, 0); (-1, -1); (-1, 1); (1, -1); (1, 1) ]

(*Part 1 Solution*)
let part1 input =
  let graph = input |> parse in
  graph
  |> Array.mapi (fun r_index row ->
         Array.mapi
           (fun c_index _ ->
             List.fold_left
               (fun acc n ->
                 acc
                 + find_word
                     (List.of_seq (String.to_seq "XMAS"))
                     n r_index c_index graph)
               0 neighbors)
           row)
  |> Array.fold_left (fun acc row -> acc + Array.fold_left ( + ) 0 row) 0

let find_s dir r c = find_word [ 'S' ] dir (fst dir + r) (snd dir + c)
let find_m dir r c = find_word [ 'M' ] dir (fst dir + r) (snd dir + c)

let left_x_mas r c graph =
  (find_m (-1, -1) r c graph <> 0 && find_s (1, 1) r c graph <> 0)
  || (find_s (-1, -1) r c graph <> 0 && find_m (1, 1) r c graph <> 0)

let right_x_mas r c graph =
  (find_m (-1, 1) r c graph <> 0 && find_s (1, -1) r c graph <> 0)
  || (find_s (-1, 1) r c graph <> 0 && find_m (1, -1) r c graph <> 0)

let findx_mas r c graph = left_x_mas r c graph && right_x_mas r c graph

let part2 input =
  let graph = input |> parse in
  graph
  |> Array.mapi (fun r_index row ->
         Array.mapi
           (fun c_index letter ->
             match letter with
             | 'A' -> findx_mas r_index c_index graph
             | _ -> false)
           row)
  |> Array.fold_left
       (fun acc row ->
         acc + Array.fold_left (fun r_acc c -> r_acc + if c then 1 else 0) 0 row)
       0

let solutions : Types.day = { part1; part2 }
