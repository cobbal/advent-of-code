open Utils
open Containers

module Pos = struct
  type t = int * int [@@deriving show, ord]
end

module PS = struct
  include Set.Make (Pos)

  let pp = pp Pos.pp
end

type grid = char array array

let pp_grid fmt : grid -> unit = Array.iter (Format.fprintf fmt "%s\n" % String.of_array)
let parse : string list -> grid = Array.of_list % List.map String.to_array

let find_start : grid -> Pos.t =
  Option.get_exn_or "start not found"
  % Array.find_map_i (fun y -> Option.map (fun (x, _) -> (y, x)) % Array.find_idx Char.(( = ) 'S'))

let supermod a b = ((a mod b) + b) mod b

let step (wrap : bool) (g : grid) ((y, x) : Pos.t) : Pos.t OSeq.iter =
 fun yield ->
  let h, w = (Array.length g, Array.length g.(0)) in
  let try_step (y, x) = if Char.(g.(supermod y h).(supermod x w) <> '#') then yield (y, x) in
  if wrap || y > 0 then try_step (y - 1, x);
  if wrap || y < h - 1 then try_step (y + 1, x);
  if wrap || x > 0 then try_step (y, x - 1);
  if wrap || x < w - 1 then try_step (y, x + 1)

let step_all wrap (g : grid) (s : PS.t) : PS.t = PS.of_iter @@ fun yield -> PS.iter (flip (step wrap g) yield) s

let march (g : grid) (f : PS.t -> 'a option) : 'a =
  let rec loop set = match f set with Some res -> res | None -> loop (step_all true g set) in
  loop (PS.singleton (find_start g))

let solve0 (input : string list) : int =
  let g = parse input in
  let steps = if Array.length g < 20 then 6 else 64 in
  PS.cardinal (repeatedly steps (step_all false g) (PS.singleton (find_start g)))

let gather_periodicity_data name (g : grid) =
  let compute () =
    let h, w = (Array.length g, Array.length g.(0)) in
    let cards = Vector.create () in
    march g (fun set ->
        Vector.push cards (PS.cardinal set);
        if Vector.length cards < 4 * (w + h) then None else Some ());
    Vector.to_array @@ cards
  in
  let cache_name = name ^ ".cache" in
  function
  | `Read -> Array.of_list @@ List.map int_of_string @@ Core.In_channel.read_lines cache_name
  | `Write ->
      let res = compute () in
      Core.Out_channel.write_lines cache_name (List.map string_of_int (Array.to_list res));
      res
  | `Compute -> compute ()

let solve1 name (input : string list) : int =
  let g = parse input in
  let h, w = (Array.length g, Array.length g.(0)) in
  let data = gather_periodicity_data name g `Compute in
  let target = 26501365 in
  let i, j = (target / (w + h), target mod (w + h)) in
  let sample n = data.(j + (n * (w + h))) in
  let x0, x1, x2, x3 = (sample 0, sample 1, sample 2, sample 3) in
  let v0, v1, v2 = (x1 - x2, x2 - x1, x3 - x2) in
  let a0, a1 = (v1 - v0, v2 - v1) in
  let extrapolate ii = x1 + (v1 * (ii - 1)) + (a1 * (ii - 1) * (ii - 2) / 2) in
  extrapolate i

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 filename input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (16, 473051787715644);
  solve_file "input-real0.txt" @@ Some (3615, 602259568764234);
  ()
