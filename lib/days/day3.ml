let parse input =
  input |> String.split_on_char '\n'

let mul_pattern = Str.regexp "mul([0-9]+,[0-9]+)"
let num_pattern = Str.regexp "[0-9]+"
let grab regex s = Str.full_split regex s

let get_nums s =
  List.filter_map
    (fun r ->
      match r with Str.Text _ -> None | Str.Delim x -> Some (int_of_string x))
    (grab num_pattern s)

let multiply vals = List.fold_left (fun res v -> res * v) 1 vals

let part1 input =
  input |> parse
  |> List.map (grab mul_pattern)
  |> List.map
       (List.filter_map (fun y ->
            match y with Str.Text _ -> None | Str.Delim x -> Some x))
  |> List.flatten |> List.map get_nums
  |> List.fold_left (fun acc vals -> acc + multiply vals) 0

let conditional_mul_pattern = Str.regexp "mul([0-9]+,[0-9]+)\\|do()\\|don't()"

let rec apply_conditions apply acc statement =
  match statement with
  | [] -> acc
  | x :: v -> (
      match x with
      | "do()" -> apply_conditions true acc v
      | "don't()" -> apply_conditions false acc v
      | nums ->
          if apply then
            apply_conditions apply (acc + multiply (get_nums nums)) v
          else apply_conditions apply acc v)

let part2 input =
  input |> parse
  |> List.map (grab conditional_mul_pattern)
  |> List.map
       (List.filter_map (fun y ->
            match y with Str.Text _ -> None | Str.Delim x -> Some x))
  |> List.flatten
  |> List.filter (fun s ->
         s = "do()" || s = "don't()" || Str.string_match mul_pattern s 0)
  |> apply_conditions true 0
