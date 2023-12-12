open Utils
open Containers

let rec diff_list : int list -> int list = function
| [] -> fail "list got too short"
| [_] -> []
| x :: y :: rest -> y - x :: diff_list (y :: rest)

let rec extrapolate : int list -> int = function
| [] | [_] -> 0
| l -> List.hd (List.rev l) + extrapolate (diff_list l)

let solve0 : string list -> int =
  List.fold_left (+) 0 % List.map (extrapolate % List.map int_of_string % words)
let solve1 : string list -> int =
  List.fold_left (+) 0 % List.map (extrapolate % List.rev % List.map int_of_string % words)

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected:expected ~actual:result

let () = time @@ fun () ->
 solve_file "input-ex0.txt" @@ Some (114, 2);
 solve_file "input-real0.txt" @@ Some (1884768153, 1031);
 ()
