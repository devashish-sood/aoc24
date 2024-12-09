module IntPairSet = Set.Make (struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match compare x0 x1 with 0 -> compare y0 y1 | c -> c
end)

let parse input =
  let parseline line = String.to_seq line |> Array.of_seq in
  input |> String.split_on_char '\n' |> List.map parseline |> Array.of_list

let next_dir dir = (snd dir, -1 * fst dir)

let find_start grid =
  let rec find_start_helper r c =
    if r = Array.length grid then None
    else if c = Array.length grid.(0) then find_start_helper (r + 1) 0
    else if grid.(r).(c) = '^' then Some (r, c)
    else find_start_helper r (c + 1)
  in
  find_start_helper 0 0

let rec walk x y dir steps graph =
  let nx, ny = (x + fst dir, y + snd dir) in
  if not (Day4.in_bounds nx ny graph) then steps
  else
    match graph.(nx).(ny) with
    | '#' -> walk x y (next_dir dir) steps graph
    | _ -> walk nx ny dir (IntPairSet.add (x, y) steps) graph

let part1 input =
  let graph = input |> parse in
  let sx, sy =
    match find_start graph with
    | None -> failwith "no start found"
    | Some pos -> (fst pos, snd pos)
  in
  IntPairSet.cardinal (walk sx sy (-1, 0) IntPairSet.empty graph) + 1
