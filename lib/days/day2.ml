let parse input =
  let parseline line =
    List.map int_of_string
      (String.split_on_char ' ' line |> List.filter (fun x -> x <> ""))
  in
  input |> String.split_on_char '\n' |> List.map parseline

let rec ascending sequence =
  match sequence with
  | [] -> true
  | [ _ ] -> true
  | first :: second :: rest -> (
      let diff = second - first in
      match diff >= 1 && diff <= 3 with
      | true -> ascending (second :: rest)
      | false -> false)

let rec descending sequence =
  match sequence with
  | [] -> true
  | [ _ ] -> true
  | first :: second :: rest -> (
      let diff = first - second in
      match diff >= 1 && diff <= 3 with
      | true -> descending (second :: rest)
      | false -> false)

let part1 input =
  let parsed_input = parse input in
  List.length
    (List.filter (fun seq -> ascending seq || descending seq) parsed_input)

let rec tolerant_ascending sequence acc =
  match sequence with
  | [] -> true
  | [ _ ] -> true
  | first :: second :: rest -> (
      let diff = second - first in
      match diff >= 1 && diff <= 3 with
      | true -> tolerant_ascending (second :: rest) (first :: acc)
      | false ->
          ascending (List.rev acc @ (first :: rest))
          || ascending (List.rev acc @ (second :: rest)))

let rec tolerant_descending sequence acc =
  match sequence with
  | [] -> true
  | [ _ ] -> true
  | first :: second :: rest -> (
      let diff = first - second in
      match diff >= 1 && diff <= 3 with
      | true -> tolerant_descending (second :: rest) (first :: acc)
      | false ->
          descending (List.rev acc @ (first :: rest))
          || descending (List.rev acc @ (second :: rest)))

let part2 input =
  let parsed_input = parse input in
  List.length
    (List.filter
       (fun seq -> tolerant_ascending seq [] || tolerant_descending seq [])
       parsed_input)
