open Utils
open Containers

type dir = N | E | S | W [@@deriving show]

let bend (start : dir) (tile : char) : dir option =
  match (tile, start) with
  | '|', N | 'J', E | 'L', W -> Some N
  | 'F', N | '-', E | 'L', S -> Some E
  | '7', E | '|', S | 'F', W -> Some S
  | '7', N | 'J', S | '-', W -> Some W
  | _ -> None

type grid = char array array

let pp_grid fmt : grid -> unit = Array.iter (Format.fprintf fmt "%s\n" % String.of_array)

type loc = int * int [@@deriving show]

let peek (g : grid) ((y, x) : loc) : char =
  if y < 0 || Array.length g <= y then '.'
  else
    let row = g.(y) in
    if x < 0 || Array.length row <= x then '.' else row.(x)

let move (y, x) = function N -> (y - 1, x) | E -> (y, x + 1) | S -> (y + 1, x) | W -> (y, x - 1)

let start (g : grid) : loc =
  Option.get_exn_or "No start found" @@ flip Array.find_mapi g
  @@ fun y row -> match Array.find_index Char.(fun c -> c = 'S') row with None -> None | Some x -> Some (y, x)

let walk (g : grid) : (loc * dir) list =
  let rec recur trail l d =
    let l' = move l d in
    let trail' = (l, d) :: trail in
    if Char.(peek g l' = 'S') then Some (List.rev trail')
    else match bend d (peek g l') with None -> None | Some d' -> recur trail' l' d'
  in
  List.hd (List.filter_map (recur [] (start g)) [ N; E; S; W ])

let parse : string list -> grid = Array.of_list % List.map String.to_array

let solve0 (input : string list) : int =
  let g = parse input in
  let trail = walk g in
  (*  print_endline @@ [%show: (loc * dir) list] @@ trail; *)
  List.length trail / 2

let verts (g : grid) : grid =
  let height = Array.length g in
  let width = Array.length g.(0) in
  let mat = Array.make_matrix ((2 * height) - 1) ((2 * width) - 1) ' ' in

  ( flip List.iter (walk g) @@ fun ((y, x), d) ->
    mat.(2 * y).(2 * x) <- peek g (y, x);
    match d with
    | N -> mat.((2 * y) - 1).(2 * x) <- '|'
    | E -> mat.(2 * y).((2 * x) + 1) <- '>'
    | S -> mat.((2 * y) + 1).(2 * x) <- '|'
    | W -> mat.(2 * y).((2 * x) - 1) <- '<' );
  ( flip List.iter (List.init (height - 1) id) @@ fun y ->
    flip List.iter (List.init width id) @@ fun x ->
    if Char.(mat.(2 * y).(2 * x) = ' ') then mat.((2 * y) + 1).(2 * x) <- '.' );
  mat

let count_in : grid -> int =
  let rec recur inside i = function
    | [] -> i
    | '.' :: rest when inside -> recur inside (i + 1) rest
    | '|' :: rest -> recur (not inside) i rest
    | _ :: rest -> recur inside i rest
  in
  Array.fold_left ( + ) 0 % Array.map (recur false 0 % Array.to_list)

let solve1 (input : string list) : int =
  let g = parse input in
  let v = verts g in
  (*  print_endline @@ [%show: grid] g; *)
  (*  print_endline @@ [%show: grid] v; *)
  count_in v

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (4, 1);
  solve_file "input-ex1.txt" @@ Some (8, 1);
  solve_file "input-ex2.txt" @@ Some (80, 10);
  solve_file "input-real0.txt" @@ Some (6931, 357);
  ()
