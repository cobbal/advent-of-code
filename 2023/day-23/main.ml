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
  let n, e, s, w =
    ( ({ p with y = p.y - 1 }, "^."),
      ({ p with x = p.x + 1 }, ">."),
      ({ p with y = p.y + 1 }, "v."),
      ({ p with x = p.x - 1 }, "<.") )
  in
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
  List.filter (fun ({ y; x }, _) -> 0 <= y && y < g.h && 0 <= x && x < g.w) unfiltered

type vertex = Start | End | Junction of pos [@@deriving show { with_path = false }, ord]

module Vertex = struct
  type t = vertex [@@deriving ord]
end

let pos_is_vertex p = function Junction p' -> p.y = p'.y && p.x = p'.x | _ -> false

module VM = Map.Make (Vertex)
module VS = Set.Make (Vertex)
module M = Memo.Make (Vertex)

module Graph = struct
  type edge = { start : vertex; end_ : vertex; weight : int } [@@deriving show { with_path = false }]
  type node = { out_edges : edge vector; in_edges : edge vector } [@@deriving show { with_path = false }]
  type t = { mutable nodes : node VM.t }

  let node g v =
    match VM.get v g.nodes with
    | Some n -> n
    | None ->
        let n = { out_edges = Vector.create (); in_edges = Vector.create () } in
        g.nodes <- VM.add v n g.nodes;
        n

  let add_edge (g : t) start end_ weight =
    let nu, nv = (node g start, node g end_) in
    let edge = { start; end_; weight } in
    Vector.push nu.out_edges edge;
    Vector.push nv.in_edges edge

  let remove_edge g e =
    let out_edges = (node g e.start).out_edges in
    let in_edges = (node g e.end_).in_edges in
    Vector.filter_in_place Stdlib.(fun e' -> e <> e') out_edges;
    Vector.filter_in_place Stdlib.(fun e' -> e <> e') in_edges

  let clean_nodes g =
    g.nodes <-
      VM.filter (fun _ { in_edges; out_edges } -> not (Vector.is_empty in_edges && Vector.is_empty out_edges)) g.nodes

  let dot g =
    print_endline "digraph {";
    flip VM.iter g.nodes (fun v { out_edges; _ } ->
        flip Vector.iter out_edges @@ fun e ->
        print_endline @@ Fmt.str "\"%a\" -> \"%a\" [label=\"%d\"];" pp_vertex e.start pp_vertex e.end_ e.weight);
    print_endline "}"
end

let graphify (g : grid) =
  let graph = Graph.{ nodes = VM.empty } in
  let visited = ref PS.empty in
  let rec walk from p =
    if not (PS.mem p !visited) then (
      visited := PS.add p !visited;
      flip List.iter (dirs_of g p) @@ fun (p', permitted_dests) ->
      if (not (pos_is_vertex p' from)) && String.contains permitted_dests g.grid.(p'.y).(p'.x) then (
        Graph.add_edge graph (Junction p) (Junction p') 1;
        walk (Junction p) p'))
  in
  let start_x = fst @@ Option.get_exn_or "start not found" @@ Array.find_idx Char.(( = ) '.') g.grid.(0) in
  let end_x = fst @@ Option.get_exn_or "start not found" @@ Array.find_idx Char.(( = ) '.') g.grid.(g.h - 1) in
  Graph.add_edge graph Start (Junction { y = 0; x = start_x }) 0;
  Graph.add_edge graph (Junction { y = g.h - 1; x = end_x }) End 0;
  walk Start { y = 0; x = start_x };
  flip VM.iter graph.nodes (fun v _ ->
      let node = Graph.node graph v in
      match (Vector.to_list node.in_edges, Vector.to_list node.out_edges) with
      | [ e0 ], [ e1 ] ->
          Graph.remove_edge graph e0;
          Graph.remove_edge graph e1;
          Graph.add_edge graph e0.start e1.end_ (e0.weight + e1.weight)
      | _, _ -> ());
  Graph.clean_nodes graph;
  (* Graph.dot graph; *)
  graph

let solve0 (input : string list) : int =
  let g = parse input in
  (* print_endline @@ [%show: grid] g; *)
  let graph = graphify g in
  let max_dist =
    M.memo_fix @@ fun max_dist v ->
    let node = Graph.node graph v in
    Vector.fold (fun acc e -> max acc Graph.(e.weight + max_dist e.start)) 0 node.in_edges
  in
  max_dist End

let opt_max =
  curry @@ function None, None -> None | None, Some x | Some x, None -> Some x | Some x, Some y -> Some (max x y)

let solve1 (input : string list) : int =
  let g = graphify (parse input) in
  let edges =
    flip VM.map g.nodes (fun { in_edges; out_edges } ->
        Vector.to_list (Vector.map (fun e -> Graph.(e.start, e.weight)) in_edges)
        @ Vector.to_list (Vector.map (fun e -> Graph.(e.end_, e.weight)) out_edges))
  in
  let rec find_longest dist visited here =
    if VS.mem here visited then None
    else if Stdlib.(here = End) then Some dist
    else
      let visited = VS.add here visited in
      List.fold_left
        (fun acc (next, w) -> opt_max acc (find_longest (dist + w) visited next))
        None (VM.get_or ~default:[] here edges)
  in
  Option.get_exn_or "??" @@ find_longest 0 VS.empty Start

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ None;
  solve_file "input-real0.txt" @@ None;
  ()
