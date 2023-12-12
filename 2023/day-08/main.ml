open Utils
open Containers

module SM = struct
  include Map.Make(String)
  let pp = pp String.pp
end
module SS = struct
  include Set.Make(String)
  let pp = pp String.pp
end
module IS = struct
  include Set.Make(Int)
  let pp = pp Int.pp
end
module IM = struct
  include Map.Make(Int)
  let pp = pp Int.pp
end

module Stream = struct
  type 'a t = Cell of ('a * 'a t) Lazy.t
  let uncons (Cell (lazy (head, tail))) = (head, tail)

  let cycle (l : 'a list) : 'a t =
    let rec help : 'a list -> 'a t = function
    | [] -> help l
    | x :: xs -> Cell (lazy (x, help xs))
    in
    help l

  let rec take = function
    | 0 -> fun _ -> []
    | i -> fun (Cell (lazy (head, tail))) -> head :: take (i - 1) tail
end

let map_starts : 'a SM.t -> SS.t =
  SS.filter (String.ends_with ~suffix:"A") % SS.of_iter % SM.keys
let map_ends : 'a SM.t -> SS.t =
  SS.filter (String.ends_with ~suffix:"Z") % SS.of_iter % SM.keys

let parse_line line =
  match words (String.filter (not % String.contains "()=,") line) with
  | [key; left; right] -> (key, (left, right))
  | _ -> fail "parse error"
let parse_map : string list -> (string * string) SM.t = SM.of_list % List.map parse_line

let follow map suffix =
  let rec follow i dirs here =
    if String.ends_with ~suffix here then i else
    let (dir, dirs) = Stream.uncons dirs in
    begin match SM.get here map with
    | None -> -1
    | Some (left, right) ->
      let next = Char.(if dir = 'L' then left else right) in
      follow (i + 1) dirs next
    end in
  follow 0

let solve0 : string list -> int = function
| dir_str :: "" :: mapping_strings ->
  let map = parse_map mapping_strings in
  let dirs = Stream.cycle @@ List.of_seq @@ String.to_seq dir_str in
  follow map "ZZZ" dirs "AAA"
| _ -> fail "parse error"

let solve1 : string list -> int = function
| dir_str :: "" :: mapping_strings ->
  let map = parse_map mapping_strings in
  let dirs = Stream.cycle @@ List.of_seq @@ String.to_seq dir_str in
  let periods = List.map (follow map "Z" dirs) @@ SS.to_list (map_starts map) in
  (* Unstated SUPER simplifying assumption: all orbits contain 1 final state and then loop back to start *)
  List.fold_left lcm 1 periods
| _ -> fail "parse error"

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected:expected ~actual:result

let () = time @@ fun () ->
 solve_file "input-ex0.txt" @@ Some (2, 2);
 solve_file "input-ex1.txt" @@ Some (6, 6);
 solve_file "input-ex2.txt" @@ Some (-1, 6);
 solve_file "input-real0.txt" @@ Some (17141, 10818234074807);
 ()
