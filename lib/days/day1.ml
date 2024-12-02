let parse input =
  let parseline line =
    match String.split_on_char ' ' line |> List.filter (fun x -> x <> "") with
    | [ x; y ] -> (int_of_string x, int_of_string y)
    | _ -> failwith "invalid"
  in
  input |> String.split_on_char '\n' |> List.map parseline

let part1 input =
  let parsed = parse input in
  let firsts = List.map (fun x -> fst x) parsed in
  let seconds = List.map (fun x -> snd x) parsed in
  List.fold_left
    (fun acc x -> acc + abs (fst x - snd x))
    0
    (List.combine (List.sort compare firsts) (List.sort compare seconds))

module IntMap = Map.Make (Int)

let part2 input =
  let parsed = parse input in
  let keys =
    List.fold_left
      (fun acc x -> acc |> IntMap.add x 0)
      IntMap.empty
      (List.map (fun x -> fst x) parsed)
  in
  let freqs =
    List.fold_left
      (fun acc x ->
        if IntMap.mem x acc then
          IntMap.update x (Option.map (fun x -> x + 1)) acc
        else acc)
      keys
      (List.map (fun x -> snd x) parsed)
  in
  List.fold_left
    (fun acc x ->
      match IntMap.find_opt x freqs with None -> acc | Some y -> (x * y) + acc)
    0
    (List.map (fun x -> fst x) parsed)
