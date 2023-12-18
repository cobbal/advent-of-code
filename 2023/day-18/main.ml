open Utils
open Containers

type dir = U | R | D | L [@@deriving eq, ord, show]

let dir_of_string = function "U" -> U | "R" -> R | "D" -> D | "L" -> L | _ -> fail "bad dir"
let dir_of_int = function 3 -> U | 0 -> R | 1 -> D | 2 -> L | _ -> fail "bad dir"

let parse0 : string list -> (dir * int) list =
  List.map @@ fun line ->
  match words line with [ ds; is; _ ] -> (dir_of_string ds, int_of_string is) | _ -> fail "bad parse"

let parse1 : string list -> (dir * int) list =
  List.map @@ fun line ->
  match words line with
  | [ _; _; word ] ->
      (dir_of_int Char.(code (String.get word 7) - code '0'), int_of_string @@ "0x" ^ String.sub word 2 5)
  | _ -> fail "bad parse"

let go x y len = function U -> (x, y - len) | R -> (x + len, y) | D -> (x, y + len) | L -> (x - len, y)
let perimeter = sum % List.map snd

let integrate plan =
  let _, _, area =
    List.fold_left
      (fun (x, y, total) (dir, len) ->
        let x', y' = go x y len dir in
        (x', y', total + ((y' + y) / 2 * (x' - x))))
      (0, 0, 0) plan
  in
  area

let solve0 (input : string list) : int =
  let plan = parse0 input in
  abs (integrate plan) + (perimeter plan / 2) + 1

let solve1 (input : string list) : int =
  let plan = parse1 input in
  abs (integrate plan) + (perimeter plan / 2) + 1

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (62, 952408144115);
  solve_file "input-real0.txt" @@ Some (62365, 159485361249806);
  ()
