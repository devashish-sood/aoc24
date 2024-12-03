open Core

let file = "inputs/2/actual.txt"

let res_part_1 =
  let raw_file = In_channel.read_all file in
  raw_file |> Days.Day2.part1;;

printf "Result: %d\n" res_part_1;;

let res_part_2 =
  let raw_file = In_channel.read_all file in
  raw_file |> Days.Day2.part2
;;

printf "Result: %d\n" res_part_2;;

(* let () = List.iter ~f:(fun x -> Printf.printf "%b " x) res_part_2; *)
