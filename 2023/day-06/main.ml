open Utils

type race = int64 * int64 [@@deriving show]

let parse0 : string list -> race list = function
  | [ time_str; dist_str ] ->
      let times = List.map Int64.of_string @@ List.tl @@ words time_str in
      let dists = List.map Int64.of_string @@ List.tl @@ words dist_str in
      List.map2 (fun x y -> (x, y)) times dists
  | _ -> fail "bad parse"

let parse1 : string list -> race = function
  | [ time_str; dist_str ] ->
      let time = Int64.of_string @@ String.concat "" @@ List.tl @@ words time_str in
      let dist = Int64.of_string @@ String.concat "" @@ List.tl @@ words dist_str in
      (time, dist)
  | _ -> fail "bad parse"

let hold_time_works (time, dist) hold =
  let open Base.Int64.O in
  hold * (time - hold) > dist

let ways_slow race =
  let time, dist = race in
  let smalltime = Int64.to_int time in
  List.length @@ flip List.filter (List.init smalltime Int64.of_int) @@ hold_time_works race

let ways race =
  let time, dist = race in
  (*
    hold * (time - hold) = dist
    hold * time - hold * hold = dist
    h^2 - t*h + dist = 0
  *)
  let open Base.Int64.O in
  let disc = sqrt (Int64.to_float ((time * time) - (4L * dist))) in
  let lo = Int64.of_float @@ ((Int64.to_float time -. disc) /. 2.0) in
  let hi = Int64.of_float @@ ((Int64.to_float time +. disc) /. 2.0) in
  let first_win = List.hd @@ List.filter (hold_time_works race) [ lo - 1L; lo; lo + 1L ] in
  let last_win = List.hd @@ List.filter (hold_time_works race) [ hi + 1L; hi; hi - 1L ] in
  Int64.to_int (last_win - first_win + 1L)

let solve0 (input : string list) : int =
  let races = parse0 input in
  List.fold_left ( * ) 1 @@ List.map ways races

let solve1 (input : string list) : int =
  let race = parse1 input in
  ways race

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (288, 71503);
  solve_file "input-real0.txt" @@ Some (1312850, 36749103);
  ()
