let parse input = input |> String.split_on_char '\n'
let mul_pattern = Str.regexp "mul([0-9]+,[0-9]+)"
let num_pattern = Str.regexp "[0-9]+"
let conditional_mul_pattern = Str.regexp "mul([0-9]+,[0-9]+)\\|do()\\|don't()"
let grab regex s = Str.full_split regex s

let get_delim_for_regex reg s =
  s |> grab reg
  |> List.filter_map (fun r ->
         match r with Str.Text _ -> None | Str.Delim x -> Some x)

let get_nums s = s |> get_delim_for_regex num_pattern |> List.map int_of_string
let multiply vals = List.fold_left (fun res v -> res * v) 1 vals

let part1 input =
  input |> parse
  |> List.map (get_delim_for_regex mul_pattern)
  |> List.flatten |> List.map get_nums
  |> List.fold_left (fun acc vals -> acc + multiply vals) 0

let rec apply_conditions apply acc statement =
  match statement with
  | [] -> acc
  | "do()" :: rst -> apply_conditions true acc rst
  | "don't()" :: rst -> apply_conditions false acc rst
  | fst :: rst ->
      apply_conditions apply
        (if apply then acc + multiply (get_nums fst) else acc)
        rst

let part2 input =
  input |> parse
  |> List.map (get_delim_for_regex conditional_mul_pattern)
  |> List.flatten |> apply_conditions true 0