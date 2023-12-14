open Utils
open Containers

type grid = char array array [@@deriving ord]
let pp_grid fmt : grid -> unit =
  Array.iter (Format.fprintf fmt "%s\n" % String.of_array)

let mut_roll_north (g : grid) : unit =
  let w = Array.length g.(0) in
  let h = Array.length g in
  for x = 0 to w - 1 do
    let rec yloop ystop y =
      if y = h then () else
      match g.(y).(x) with
      | '#' -> yloop (y + 1) (y + 1)
      | 'O' ->
        let swap = g.(ystop).(x) in
        g.(ystop).(x) <- 'O';
        g.(y).(x) <- swap;
        yloop (ystop + 1) (y + 1)
      | _ -> yloop ystop (y + 1)
    in yloop 0 0
  done

 let rotate (g: grid) : grid =
  let w = Array.length g.(0) in
  let h = Array.length g in
  let rot = Array.make_matrix w h (Char.chr 0) in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      rot.(x).(h - 1 - y) <- g.(y).(x)
    done
  done;
  rot

let grid_copy : grid -> grid = Array.map Array.copy

let boulder_count : char array -> int =
  List.length % List.filter Char.((=) 'O') % Array.to_list

let grid_weight (g : grid) : int =
  let len = Array.length g in
  let line_weights = Array.to_list @@ Array.mapi (fun i l -> (len - i, boulder_count l)) g in
  sum (List.map (uncurry ( * )) line_weights)

let parse_grid : string list -> grid = Array.of_list % List.map String.to_array

let solve0 (input : string list) : int =
  let g = parse_grid input in
  mut_roll_north g;
  grid_weight g

let cycle : grid -> grid =
  repeatedly 4 (fun g -> mut_roll_north g; rotate g) % grid_copy

module GM = struct
  include Map.Make(struct
    type t = grid [@@deriving show, ord]
  end)
  let pp = pp pp_grid
end

let find_cycle (g : grid) : int * int * grid =
  let rec loop (previous_states : int GM.t) (i : int) (g : grid) =
    match GM.get g previous_states with
    | Some j -> (j, i, g)
    | None -> loop (GM.add g i previous_states) (i + 1) (cycle g)
  in loop GM.empty 0 g

let megacycle (g : grid) (n : int) : grid =
  let (loop_start, loop_end, loop_grid) = find_cycle g in
  let period = loop_end - loop_start in
  repeatedly ((n - loop_start) mod period) cycle loop_grid

let solve1 (input : string list) : int =
  grid_weight @@ megacycle (parse_grid input) 1000000000

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected:expected ~actual:result

let () = time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (136, 64);
  solve_file "input-real0.txt" @@ Some (106517, 79723);
  ()
