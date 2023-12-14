open Utils
open Containers

type grid = char array array

let pp_grid fmt : grid -> unit = Array.iter (Format.fprintf fmt "%s\n" % String.of_array)

type loc = int * int [@@deriving show]

let galaxies (g : grid) : loc list =
  let locs = ref [] in
  ( flip Array.iteri g @@ fun y row ->
    flip Array.iteri row @@ fun x c -> if Char.(c = '#') then locs := (y, x) :: !locs );
  List.rev !locs

let parse : string list -> grid = Array.of_list % List.map String.to_array
let array_of_2d_list = Array.of_list % List.map Array.of_list
let list_of_2d_array = Array.to_list % Array.map Array.to_list

let empty_rows (g : grid) : int list =
  let res = ref [] in
  Array.iteri (fun i row -> if Array.for_all Char.(fun c -> c = '.') row then res := i :: !res) g;
  List.rev !res

let expand (factor : int) (g : grid) : loc -> loc =
  let rows = empty_rows g in
  let cols = empty_rows @@ array_of_2d_list @@ transpose @@ list_of_2d_array g in
  fun (y, x) ->
    ( y + ((factor - 1) * List.length (List.filter (( > ) y) rows)),
      x + ((factor - 1) * List.length (List.filter (( > ) x) cols)) )

let rec unique_pairs : 'a list -> ('a * 'a) list = function
  | [] -> []
  | x :: xs -> List.map (fun y -> (x, y)) xs @ unique_pairs xs

let dist (l : loc) (m : loc) = abs (fst l - fst m) + abs (snd l - snd m)

let solve0 (input : string list) : int =
  let g = parse input in
  let locs = List.map (expand 2 g) @@ galaxies g in
  List.fold_left ( + ) 0 @@ List.map (uncurry dist) @@ unique_pairs @@ locs

let solve1 (input : string list) : int =
  let g = parse input in
  let locs = List.map (expand 1000000 g) @@ galaxies g in
  List.fold_left ( + ) 0 @@ List.map (uncurry dist) @@ unique_pairs @@ locs

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (374, 82000210);
  solve_file "input-real0.txt" @@ Some (10885634, 707505470642);
  ()
