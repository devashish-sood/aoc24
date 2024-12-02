open Core

let file = "lib/inputs/1/actual.txt"

let res_part_1 = let raw_file = In_channel.read_all file
in 
raw_file |> Days.Day1.part1;;


let res_part_2 = let raw_file = In_channel.read_all file
in 
raw_file |> Days.Day1.part2;;



printf "Result: %d\n" res_part_1;;
printf "Result: %d\n" res_part_2;;

(* let print_map_as_list map = 
  let pairs = Days.Day1.IntMap.to_list map in
  List.iter ~f:(fun p -> Printf.printf "(%d, %d)\n" (fst p) (snd p)) pairs;; *)

(* print_map_as_list res_part_2;; *)
