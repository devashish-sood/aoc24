module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let segment input =
  match Str.split (Str.regexp "\n\n") input with
  | [ x; y ] -> (x, y)
  | _ -> failwith "invalid"

let add_node k v m =
  StringMap.update k
    (function
      | None -> Some (StringSet.of_list [ v ])
      | Some set -> Some (StringSet.add v set))
    m

let check_node cur pre graph =
  match StringMap.find_opt pre graph with
  | None -> false
  | Some nodes -> StringSet.mem cur nodes

let parse_edge edge : string * string =
  match String.split_on_char '|' edge with
  | [ x; y ] -> (x, y)
  | _ -> failwith "invalid edge"

let parse_graph g =
  g |> String.split_on_char '\n' |> List.map parse_edge
  |> List.fold_left (fun acc x -> add_node (snd x) (fst x) acc) StringMap.empty

let parse_updates u =
  u |> String.split_on_char '\n' |> List.map (String.split_on_char ',')

let parse input =
  let segmented = segment input in
  (parse_graph (fst segmented), parse_updates (snd segmented))

let rec valid_pages pre suc graph =
  match suc with
  | [] -> true
  | hd :: tl ->
      List.for_all (fun p -> not (check_node hd p graph)) pre
      && valid_pages (hd :: pre) tl graph

let part1 input =
  let graph, updates = input |> parse in
  List.filter (fun pages -> valid_pages [] pages graph) updates |> List.map (fun l -> int_of_string (List.nth l (List.length l / 2))) |>  List.fold_left (+) 0;;

(*Topological sort, grab the numbers that are needed, remove any edges not showing up, and then topo sort based on the length of the set*)