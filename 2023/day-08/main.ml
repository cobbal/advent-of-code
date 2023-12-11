open Utils 
open Containers

(* type loc = int
let loc_of_string s = 
  let uncode c = Char.(code c - code 'A' + 1) in
  match List.of_seq (String.to_seq s) with
  | [a; b; c] -> 10000 * uncode a + 100 * uncode b + uncode c
  | _ -> fail "parse error"
let string_of_loc i =
  let code i = Char.(chr (i - 1 + code 'A')) in
  String.of_list [code (i % *)

module SM = struct
  include Map.Make(String)
  let pp = pp String.pp
end
module SS = struct
  include Set.Make(String)
  let pp = pp String.pp
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
end

let map_starts : 'a SM.t -> SS.t =
  SS.filter (String.ends_with ~suffix:"A") % SS.of_iter % SM.keys
let map_ends : 'a SM.t -> SS.t =
  SS.filter (String.ends_with ~suffix:"Z") % SS.of_iter % SM.keys

let parse_line line = 
  match words (String.filter (not % String.contains "()=,") line) with
  | [key; left; right] -> (key, (left, right))
  | _ -> fail "parse error"
let parse_map = SM.of_list % List.map parse_line

let follow map = 
  let rec follow i dirs = function
  | "ZZZ" -> i
  | here -> 
    let (dir, dirs) = Stream.uncons dirs in
    begin match SM.get here map with
    | None -> -1
    | Some (left, right) -> 
      let next = Char.(if dir = 'L' then left else right) in
      follow (i + 1) dirs next
    end in
  follow 0

let follow_set map =
  let ends = map_ends map in
  let rec follow_set i dirs here = 
(*     print_endline @@ [%show: string list] @@ SS.to_list here; *)
    if SS.subset here ends then i else
    let (dir, dirs) = Stream.uncons dirs in
    let next = flip SS.map here @@ fun node -> 
      let (left, right) = Option.get_exn_or "dead end" @@ SM.get node map in
      if Char.(dir = 'L') then left else right
    in
    follow_set (i + 1) dirs next
  in follow_set 0

let solve0 : string list -> int = function
| dir_str :: "" :: mapping_strings -> 
  let map = parse_map mapping_strings in
  let dirs = Stream.cycle @@ List.of_seq @@ String.to_seq dir_str in
  follow map dirs "AAA"
| _ -> fail "parse error"

let solve1 : string list -> int = function
| dir_str :: "" :: mapping_strings -> 
  let map = parse_map mapping_strings in
  let dirs = Stream.cycle @@ List.of_seq @@ String.to_seq dir_str in
  follow_set map dirs (map_starts map)
| _ -> fail "parse error"

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected:expected ~actual:result

let () = time @@ fun () ->
 solve_file "input-ex0.txt" @@ None;
 solve_file "input-ex1.txt" @@ None;
 solve_file "input-ex2.txt" @@ None;
(*  solve_file "input-real0.txt" @@ None; *)
 ()
