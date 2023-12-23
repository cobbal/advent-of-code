open Utils
open Containers
module ID = Int

module IDSet = struct
  include Set.Make (ID)

  let pp fmt = Format.fprintf fmt "%s" % [%show: ID.t list] % to_list
end

type grid = (int * ID.t) array array [@@deriving show]
type range = { lo : int; hi : int }

let pp_range fmt r = Format.fprintf fmt "%d~%d" r.lo r.hi

type block = { x : range; y : range; z : range }

let block_cmp a b = compare a.z.lo b.z.lo
let range a b = { lo = min a b; hi = max a b }

let block_seq (b : block) : (int * int * int) OSeq.seq =
  OSeq.(product3 (b.x.lo -- b.x.hi) (b.y.lo -- b.y.hi) (b.z.lo -- b.z.hi))

let block_xy_seq (b : block) : (int * int) OSeq.seq = OSeq.(product (b.x.lo -- b.x.hi) (b.y.lo -- b.y.hi))

let parse : string list -> block list =
  List.sort block_cmp
  % List.map (fun line ->
        match List.map (List.map int_of_string % String.split_on_char ',') (String.split_on_char '~' line) with
        | [ [ x0; y0; z0 ]; [ x1; y1; z1 ] ] -> { x = range x0 x1; y = range y0 y1; z = range z0 z1 }
        | _ -> fail "bad parse")

let grid_size =
  let rec loop xl xh yl yh = function
    | [] -> (yh - yl + 1, xh - xl + 1)
    | b :: rest -> loop (min xl b.x.lo) (max xh b.x.hi) (min yl b.y.lo) (max yh b.y.hi) rest
  in
  loop 0 0 0 0

let drop (grid : grid) bid b : ID.t list =
  let shadow = OSeq.to_list @@ OSeq.map (fun (x, y) -> grid.(y).(x)) (block_xy_seq b) in
  let resting_height = List.fold_left max 0 (List.map fst shadow) in
  let new_height = resting_height + (b.z.hi - b.z.lo + 1) in
  Seq.iter (fun (x, y) -> grid.(y).(x) <- (new_height, bid)) (block_xy_seq b);
  IDSet.to_list @@ IDSet.of_list
  @@ List.filter_map (fun (h, bid) -> if h = resting_height then Some bid else None) shadow

let solve0 (input : string list) : int =
  let blocks = parse input in
  let grid = uncurry Array.make_matrix (grid_size blocks) (0, -1) in
  let restings = List.mapi (drop grid) blocks in
  let critical = IDSet.of_list @@ List.filter_map (function [ s ] -> Some s | _ -> None) restings in
  List.length blocks - (IDSet.cardinal critical - 1)

module M = Memo.Make (ID)

let solve1 (input : string list) : int =
  let blocks = parse input in
  let grid = uncurry Array.make_matrix (grid_size blocks) (0, -1) in
  let restings = List.mapi (drop grid) blocks in
  let collapse : IDSet.t -> (int * IDSet.t) list -> IDSet.t =
    List.fold_left @@ fun fallen (i, supports) -> if IDSet.subset supports fallen then IDSet.add i fallen else fallen
  in
  let collapse start =
    IDSet.remove start @@ collapse (IDSet.singleton start) (List.mapi (fun i x -> (i, IDSet.of_list x)) restings)
  in
  let all_collapses = List.map collapse List.(0 --^ length blocks) in
  sum (List.map IDSet.cardinal all_collapses)

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (5, 7);
  solve_file "input-real0.txt" @@ Some (454, 74287);
  ()
