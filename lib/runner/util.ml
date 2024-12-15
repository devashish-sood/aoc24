open Core

let get_file day file_type = 
  let file_path = Printf.sprintf "inputs/%d/%s.txt" day file_type in
  In_channel.read_all file_path

let get_test_file day = get_file day "test";;
let get_data_file day = get_file day "actual";;
let solutions = [
  Days.Day1.solutions; 
  Days.Day2.solutions;
  Days.Day3.solutions;
  Days.Day4.solutions;
  Days.Day5.solutions;
  Days.Day6.solutions;
]