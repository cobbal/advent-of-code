open Utils
open Containers

type dir = N | E | S | W [@@deriving ord, show { with_path = false }]

let opp : dir -> dir = function N -> S | E -> W | S -> N | W -> E
let int_of_dir = function N -> 0 | E -> 1 | S -> 2 | W -> 3
let dir_of_int = function 0 -> N | 1 -> E | 2 -> S | 3 -> W | i -> fail ("bad dir " ^ string_of_int i)
let div_mod a b = (a / b, a mod b)

module Pos = struct
  type t = { y : int; x : int; last_dir : dir; straight : int }

  let pp fmt p = Format.fprintf fmt "(%d, %d) %a %d" p.y p.x pp_dir p.last_dir p.straight
  let to_int w p : int = p.straight + (11 * (int_of_dir p.last_dir + (4 * (p.x + (w * p.y)))))

  let of_int (w : int) (i : int) : t =
    let i, straight = div_mod i 11 in
    let i, di = div_mod i 4 in
    let y, x = div_mod i w in
    { y; x; last_dir = dir_of_int di; straight }
end

type grid = int array array

let pp_grid fmt : grid -> unit =
  Array.iter (Format.fprintf fmt "%s\n" % String.of_array % Array.map Char.(fun i -> chr (i + code '0')))

let parse : string list -> grid =
  Array.of_list % List.map (Array.map Char.(fun c -> code c - code '0') % String.to_array)

let go (p : Pos.t) (d : dir) : Pos.t =
  let straight = if Stdlib.(p.last_dir = d) then p.straight + 1 else 1 in
  match d with
  | N -> { y = p.y - 1; x = p.x; last_dir = d; straight }
  | E -> { y = p.y; x = p.x + 1; last_dir = d; straight }
  | S -> { y = p.y + 1; x = p.x; last_dir = d; straight }
  | W -> { y = p.y; x = p.x - 1; last_dir = d; straight }

module G = Graph.Pack.Digraph

let solve_grid min_run max_run (g : grid) : int =
  let h = Array.length g in
  let w = Array.length g.(0) in
  let gr = G.create () in

  let edge (p0 : Pos.t) (p1 : Pos.t) : int option =
    if
      Stdlib.(p1.last_dir != opp p0.last_dir)
      && 0 <= p1.y && p1.y < h && 0 <= p1.x && p1.x < w && p1.straight <= max_run
      && Stdlib.(p1.last_dir = p0.last_dir || p0.straight >= min_run)
    then Some g.(p1.y).(p1.x)
    else None
  in

  let verts = Array.init (11 * 4 * w * h) G.V.create in
  let v p = verts.(Pos.to_int w p) in
  let start_vert = G.V.create (-1) in
  let end_vert = G.V.create (-2) in
  let pp_vert fmt v =
    match G.V.label v with
    | -1 -> Format.fprintf fmt "start"
    | -2 -> Format.fprintf fmt "end"
    | i -> Pos.pp fmt (Pos.of_int w i)
  in
  let pp_edge fmt e = Format.fprintf fmt "%a -%d-> %a" pp_vert (G.E.src e) (G.E.label e) pp_vert (G.E.dst e) in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      for di = 0 to 3 do
        let last_dir = dir_of_int di in
        for straight = 0 to 10 do
          let pos = Pos.{ y; x; last_dir; straight } in
          let posi = Pos.to_int w pos in
          if y = h - 1 && x = w - 1 && straight >= min_run then G.add_edge_e gr (G.E.create (v pos) 0 end_vert);
          if y = 0 && x = 0 && straight = 0 then G.add_edge_e gr (G.E.create start_vert 0 (v pos));
          flip List.iter [ N; E; S; W ] @@ fun next_dir ->
          let new_pos = go pos next_dir in
          match edge pos new_pos with
          | None -> ()
          | Some cost -> G.add_edge_e gr (G.E.create (v pos) cost (v new_pos))
        done
      done
    done
  done;
  let start_pos = Pos.{ y = 0; x = 0; last_dir = E; straight = 1 } in
  let end_pos = Pos.{ y = h - 1; x = w - 1; last_dir = E; straight = 1 } in
  print_endline "starting solve";
  let edges, dist = G.shortest_path gr start_vert end_vert in
  print_endline "done";
  (* List.iter (print_endline % [%show: edge]) edges; *)
  dist

let solve0 (input : string list) : int =
  let g = parse input in
  (* print_endline @@ [%show: grid] g; *)
  solve_grid 0 3 g

let solve1 (input : string list) : int =
  let g = parse input in
  (* print_endline @@ [%show: grid] g; *)
  solve_grid 4 10 g

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (102, 94);
  solve_file "input-ex1.txt" @@ Some (59, 71);
  solve_file "input-real0.txt" @@ Some (684, 822);
  ()
