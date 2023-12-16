open Utils
open Containers

type dir = N | E | S | W [@@deriving ord, show]
type pos = int * int [@@deriving ord, show]

module Beam = struct
  type t = dir * pos [@@deriving ord, show]
end

let move : Beam.t -> Beam.t = function
  | N, (y, x) -> (N, (y - 1, x))
  | E, (y, x) -> (E, (y, x + 1))
  | S, (y, x) -> (S, (y + 1, x))
  | W, (y, x) -> (W, (y, x - 1))

module BS = struct
  type t = int array array

  let flag_of : dir -> int = function N -> 1 | E -> 2 | S -> 4 | W -> 8
  let empty height width = Array.make_matrix height width 0
  let mem ((dir, (y, x)) : Beam.t) (arr : t) : bool = arr.(y).(x) land flag_of dir <> 0
  let add ((dir, (y, x)) : Beam.t) (arr : t) : unit = arr.(y).(x) <- arr.(y).(x) lor flag_of dir

  let mem_add ((dir, (y, x)) : Beam.t) (arr : t) : bool =
    let old = arr.(y).(x) in
    if old land flag_of dir = 0 then (
      arr.(y).(x) <- old lor flag_of dir;
      false)
    else true
end

type grid = char array array [@@deriving ord]

let pp_grid fmt : grid -> unit = Array.iter (Format.fprintf fmt "%s\n" % String.of_array)
let parse : string list -> grid = Array.of_list % List.map String.to_array
let mat_map (f : 'a -> 'b) : 'a array array -> 'b array array = Array.map (Array.map f)

let count : BS.t -> int =
  Array.fold_left (fun acc row -> acc + Array.fold_left (fun acc flags -> acc + if flags = 0 then 0 else 1) 0 row) 0

let bounce (g : grid) start : BS.t =
  let height = Array.length g in
  let width = Array.length g.(0) in
  let seen = BS.empty height width in
  let rec trace (b : Beam.t) : unit =
    let dir, ((y, x) as pos) = b in
    if y < 0 || height <= y || x < 0 || width <= x || BS.mem_add b seen then ()
    else
      match (g.(y).(x), dir) with
      | '.', _ | '|', N | '|', S | '-', E | '-', W -> trace (move b)
      | '/', N -> trace (move (E, pos))
      | '/', E -> trace (move (N, pos))
      | '/', S -> trace (move (W, pos))
      | '/', W -> trace (move (S, pos))
      | '\\', N -> trace (move (W, pos))
      | '\\', E -> trace (move (S, pos))
      | '\\', S -> trace (move (E, pos))
      | '\\', W -> trace (move (N, pos))
      | '|', _ ->
          trace (move (N, pos));
          trace (move (S, pos))
      | '-', _ ->
          trace (move (E, pos));
          trace (move (W, pos))
      | _ -> fail "bad tile"
  in
  trace start;
  seen

let solve0 (input : string list) : int =
  let g = parse input in
  (*  print_endline @@ [%show: grid] g; *)
  let res = bounce g (E, (0, 0)) in
  (*  print_endline @@ [%show: grid] @@ mat_map (function true -> '#' | false -> '.')  res; *)
  count res

let solve1 (input : string list) : int =
  let g = parse input in
  let height = Array.length g in
  let width = Array.length g.(0) in
  let starts =
    List.init width (fun x -> (S, (0, x)))
    @ List.init width (fun x -> (N, (height - 1, x)))
    @ List.init height (fun y -> (E, (y, 0)))
    @ List.init height (fun y -> (W, (y, width - 1)))
  in
  List.fold_left max 0 @@ List.map (count % bounce g) starts

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (46, 51);
  solve_file "input-real0.txt" @@ Some (7242, 7572);
  ()
