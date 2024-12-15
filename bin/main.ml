open Core

let command =
  Command.basic ~summary:"Advent of Code 2024"
    (let%map_open.Command day = anon ("day" %: int)
     and partial = flag "--partial" no_arg ~doc:"Run partial solution"
     and test = flag "--test" no_arg ~doc:"Run against test" in
     fun () -> Runner.run day partial test)

let () = Command_unix.run command
