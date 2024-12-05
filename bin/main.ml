open Core

let file = "inputs/3/actual.txt"

let res_part_1 =
  let raw_file = In_channel.read_all file in
  raw_file |> Days.Day3.part1
;;

printf "Result: %d\n" res_part_1

let res_part_2 =
  let raw_file = In_channel.read_all file in
  raw_file |> Days.Day3.part2
;;

printf "Result: %d\n" res_part_2;;
