open Utils
open Containers

type dir = N | E | S | W [@@deriving eq, ord, show { with_path = false }]

let opp : dir -> dir = function N -> S | E -> W | S -> N | W -> E
let int_of_dir = function N -> 0 | E -> 1 | S -> 2 | W -> 3
let dir_of_int = function 0 -> N | 1 -> E | 2 -> S | 3 -> W | i -> fail ("bad dir " ^ string_of_int i)
let div_mod a b = (a / b, a mod b)

module Pos = struct
  type t = { y : int; x : int; last_dir : dir; straight : int } [@@deriving ord, show { with_path = false }]

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

module Vert = struct
  type t = Start | End | Pos of Pos.t [@@deriving ord, show { with_path = false }]

  let of_int w = function -1 -> Start | -2 -> End | i -> Pos (Pos.of_int w i)
  let to_int w = function Start -> -1 | End -> -2 | Pos p -> Pos.to_int w p
end

module G = Graphs.Graph (Int)

let solve_grid min_run max_run (g : grid) : int =
  let h = Array.length g in
  let w = Array.length g.(0) in

  let valid_pos (p : Pos.t) = 0 <= p.y && p.y < h && 0 <= p.x && p.x < w && p.straight <= max_run in

  let edges (v0i : int) : (int * int) OSeq.iter =
    let v0 = Vert.of_int w v0i in
    fun yield ->
      let yield (v, cost) = yield (Vert.to_int w v, cost) in
      match v0 with
      | Start ->
          yield @@ (Pos { x = 0; y = 0; last_dir = E; straight = 0 }, 0);
          yield @@ (Pos { x = 0; y = 0; last_dir = S; straight = 0 }, 0)
      | End -> ()
      | Pos p0 ->
          if p0.y = h - 1 && p0.x = w - 1 && p0.straight >= min_run then yield (End, 0);
          flip List.iter [ N; E; S; W ] @@ fun dir ->
          if Stdlib.(dir = opp p0.last_dir || (dir != p0.last_dir && p0.straight < min_run)) then ()
          else
            let p1 = go p0 dir in
            if valid_pos p1 then yield @@ (Pos p1, g.(p1.y).(p1.x))
  in

  let dist, trace = G.shortest_path ~edges (Vert.to_int w Start) (Vert.to_int w End) in
  dist

let solve0 (input : string list) : int = solve_grid 0 3 (parse input)
let solve1 (input : string list) : int = solve_grid 4 10 (parse input)

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
