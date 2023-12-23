open Utils
open Containers

type grid = { h : int; w : int; grid : char array array }
type 'a vector = 'a Vector.vector

let pp_vector pp_a fmt = List.pp pp_a fmt % Vector.to_list
let pp_grid fmt g = Array.iter (Format.fprintf fmt "%s\n" % String.of_array) g.grid

let parse lines : grid =
  let grid = Array.of_list (List.map String.to_array lines) in
  { grid; h = Array.length grid; w = Array.length grid.(0) }

type pos = { y : int; x : int } [@@deriving ord]

let pp_pos fmt p = Format.fprintf fmt "(%d, %d)" p.y p.x

module Pos = struct
  type t = pos [@@deriving ord, show]
end

module PS = struct
  include Set.Make (Pos)

  let pp = pp Pos.pp
end

let dirs_of g p =
  let n, e, s, w = ({ p with y = p.y - 1 }, { p with x = p.x + 1 }, { p with y = p.y + 1 }, { p with x = p.x - 1 }) in
  let unfiltered =
    match g.grid.(p.y).(p.x) with
    | '^' -> [ n ]
    | '>' -> [ e ]
    | 'v' -> [ s ]
    | '<' -> [ w ]
    | '.' -> [ n; e; s; w ]
    | '#' -> []
    | _ -> fail "bad char"
  in
  List.filter (fun { y; x } -> 0 <= y && y < g.h && 0 <= x && x < g.w) unfiltered

module M = Memo.Make (Pos)

type vertex = Start | End | Junction of pos

let pos_is_vertex p = function Junction p' -> p.y = p'.y && p.x = p'.x | _ -> false

let graphify (g : grid) =
  let edges = Array.map (Array.map (fun _ -> Vector.create ())) g.grid in
  let visited = ref PS.empty in
  let rec walk from p =
    if not (PS.mem p !visited) then (
      visited := PS.add p !visited;
      flip List.iter (dirs_of g p) @@ fun p' ->
      if (not (pos_is_vertex p from)) && Char.(g.grid.(p'.y).(p'.x) <> '#') then (
        Vector.push edges.(p.y).(p.x) p';
        walk (Junction p) p'))
  in
  let start_x = fst @@ Option.get_exn_or "start not found" @@ Array.find_idx Char.(( = ) '.') g.grid.(0) in
  let end_x = fst @@ Option.get_exn_or "start not found" @@ Array.find_idx Char.(( = ) '.') g.grid.(g.h - 1) in
  walk Start { y = 0; x = start_x };
  print_endline @@ [%show: pos vector array array] edges;
  ()

let solve0 (input : string list) : int =
  let g = parse input in
  print_endline @@ [%show: grid] g;
  let graph = graphify g in
  0

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
