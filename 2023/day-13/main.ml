open Utils
open Containers

type grid = string list

let pp_grid fmt : grid -> unit = List.iter (Format.fprintf fmt "%s\n")

let line_flaws (s : string) (t : string) : int =
  let fold acc c d = if Char.(c = d) then acc else acc + 1 in
  Seq.fold_left2 fold 0 (String.to_seq s) (String.to_seq t)

let mirror_flaws (g : grid) (y : int) : int =
  let len = min y (List.length g - y) in
  let top_refl = List.take len @@ List.rev @@ List.take y g in
  let bot = List.take len @@ List.drop y g in
  sum @@ List.map2 line_flaws top_refl bot

let find_mirror (flaws : int) (g : grid) : int option =
  List.find_opt (fun y -> mirror_flaws g y = flaws) List.(1 --^ length g)

let tr_grid : grid -> grid = List.map string_of_list % transpose % List.map list_of_string

let solve_grid (flaws : int) (g : grid) : int =
  let row = find_mirror flaws g in
  let col = find_mirror flaws (tr_grid g) in
  let value = Option.(map (( * ) 100) row <+> col) in
  Option.get_exn_or "failed" value

let solve0 (input : string list) : int =
  let solutions = List.map (solve_grid 0) (paragraphs input) in
  sum solutions

let solve1 (input : string list) : int =
  let solutions = List.map (solve_grid 1) (paragraphs input) in
  sum solutions

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (405, 400);
  solve_file "input-real0.txt" @@ Some (34772, 35554);
  ()
