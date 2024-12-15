let run day partial test =
  let solutions =
    match List.nth_opt Util.solutions (day - 1) with
    | None -> failwith (Printf.sprintf "Day %d not implemented yet" day)
    | Some k -> k
  in
  let input =
    match test with
    | true -> Util.get_test_file day
    | false -> Util.get_data_file day
  in
  match partial with
  | true -> Printf.printf "Part 1 Result: %d\n" (solutions.part1 input)
  | false ->
      Printf.printf "Part 1 Result: %d\n" (solutions.part1 input);
      Printf.printf "Part 2 Result: %d\n" (solutions.part2 input)
