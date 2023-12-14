open Utils
open Containers

let solve0 (input : string list) : int = 0
let solve1 (input : string list) : int = 0

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ None;
  (* solve_file "input-real0.txt" @@ None; *)
  ()
