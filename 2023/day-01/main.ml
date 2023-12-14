open Utils

let char_list_digit0 : char list -> int option = function
  | ('0' .. '9' as c) :: _ -> Some Char.(code c - code '0')
  | _ -> None

let char_list_digit1 : char list -> int option = function
  | 'z' :: 'e' :: 'r' :: 'o' :: _ -> Some 0
  | 'o' :: 'n' :: 'e' :: _ -> Some 1
  | 't' :: 'w' :: 'o' :: _ -> Some 2
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> Some 3
  | 'f' :: 'o' :: 'u' :: 'r' :: _ -> Some 4
  | 'f' :: 'i' :: 'v' :: 'e' :: _ -> Some 5
  | 's' :: 'i' :: 'x' :: _ -> Some 6
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> Some 7
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> Some 8
  | 'n' :: 'i' :: 'n' :: 'e' :: _ -> Some 9
  | c -> char_list_digit0 c

let digits char_list_digit (str : string) : int list =
  List.filter_map char_list_digit (tails (List.of_seq (String.to_seq str)))

let solve_line (line : int list) : int = (10 * List.hd (line @ [ 0 ])) + List.hd (List.rev line @ [ 0 ])

let solve char_list_digit (input : string list) : int =
  List.fold_left ( + ) 0 (List.map (solve_line % digits char_list_digit) input)

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  (* print_endline Fmt.(str "%s: %a" filename Dump.(list (list int)) (List.map digits input)); *)
  let result = (solve char_list_digit0 input, solve char_list_digit1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (142, 142);
  solve_file "input-ex1.txt" @@ Some (209, 281);
  solve_file "input-real0.txt" @@ Some (56506, 56017);
  ()
