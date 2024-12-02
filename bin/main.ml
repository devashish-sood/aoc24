open Core

let file = "inputs/1/actual.txt"

let res_part_1 =
  let raw_file = In_channel.read_all file in
  raw_file |> Days.Day1.part1

let res_part_2 =
  let raw_file = In_channel.read_all file in
  raw_file |> Days.Day1.part2
;;

printf "Result: %d\n" res_part_1;;
printf "Result: %d\n" res_part_2
