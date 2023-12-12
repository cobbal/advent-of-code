open Utils
open Containers

module IIMemo = Memo.Make(struct
    type t = int * int [@@deriving ord]
end)

let parse (line : string) : string * int array =
  match words line with
  | [springs; num_str] -> (
    springs,
    Array.of_list @@ List.map int_of_string (String.split_on_char ',' num_str)
  )
  | _ -> fail "bad parse"

let solve_line springs runs =
  let springs_len = String.length springs in
  let runs_len = Array.length runs in
  let isolve : int * int -> int = IIMemo.memo_fix (fun isolve (i, j) ->
    if i = springs_len && j = runs_len then 1 else
    if i >= springs_len then 0 else
    let eat_run () =
      if j >= runs_len then 0 else
      let len = runs.(j) in
      if i + len > springs_len then 0 else
      if not (String.for_all (String.contains "#?") (String.sub springs i len)) then 0 else
      let more_left = i + len < springs_len in
      if more_left && Char.(String.get springs (i + len) = '#') then 0 else
      isolve (min (i + len + 1) springs_len, j + 1)
    in
    let pass () = isolve (i + 1, j) in
    let res = match String.get springs i with
    | '.' -> pass ()
    | '#' -> eat_run ()
    | '?' -> pass () + eat_run ()
    | _ -> fail "bad char"
    in res
  ) in isolve (0, 0)

let solve0 (input : string list) : int =
  let results = List.map (uncurry solve_line % parse) input in
  List.fold_left (+) 0 results

let unfold (springs, runs) =
  (
    String.concat "?" (List.init 5 (const springs)),
    Array.concat (List.init 5 (const runs))
  )

let solve1 (input : string list) : int =
  let results = List.map (uncurry solve_line % unfold % parse) input in
  List.fold_left (+) 0 results

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected:expected ~actual:result

let () = time @@ fun () ->
 solve_file "input-ex0.txt" @@ Some (21, 525152);
 solve_file "input-real0.txt" @@ Some (7379, 7732028747925);
 ()