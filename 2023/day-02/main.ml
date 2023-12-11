open Utils

module Cubes = struct
  type t = { r : int; g : int; b : int } [@@deriving show]

  let plus { r = r0; g = g0; b = b0 } { r = r1; g = g1; b = b1 } : t =
    { r = r0 + r1; g = g0 + g1; b = b0 + b1 }
  let zero : t = { r = 0; g = 0; b = 0 }
  let red r : t = { r = r; g = 0; b = 0 }
  let green g : t = { r = 0; g = g; b = 0 }
  let blue b : t = { r = 0; g = 0; b = b }

  let of_color str : t = match String.split_on_char ' ' (String.trim str) with
    | [r; "red"] -> red (int_of_string r)
    | [g; "green"] -> green (int_of_string g)
    | [b; "blue"] -> blue (int_of_string b)
    | _ -> fail ("bad parse: " ^ str)
  let of_string str : t = List.fold_left plus zero @@ List.map of_color (String.split_on_char ',' str)

  let bag : t = { r = 12; g = 13; b = 14 }
  let valid (c : t) = c.r <= bag.r && c.g <= bag.g && c.b <= bag.b

  let join { r = r0; g = g0; b = b0 } { r = r1; g = g1; b = b1 } : t =
    { r = max r0 r1; g = max g0 g1; b = max b0 b1 }

  let power (c : t) = c.r * c.g * c.b
end
module Game = struct
  type t = { id : int; draws : Cubes.t list } [@@deriving show]

  let of_string str : t =
    match List.map String.trim (String.split_on_char ':' str) with
    | [idString; drawString] ->
      begin match String.split_on_char ' ' idString with
        | ["Game"; id] -> { id = int_of_string id; draws = List.map Cubes.of_string @@ String.split_on_char ';' drawString }
        | _ -> fail "bad parse"
      end
    | _ -> fail "bad parse"

  let valid (g : t) = List.for_all Cubes.valid g.draws
  let minBag (g : t) = List.fold_left Cubes.join Cubes.zero g.draws
end

let solve0 (input : string list) : int =
  let games = List.map Game.of_string input in
  (* print_endline @@ [%show: Game.t list] games; *)
  let valid_games = List.filter Game.valid games in
  List.fold_left (+) 0 @@ List.map (fun (g : Game.t) -> g.id) valid_games

let solve1 (input : string list) : int =
  let games = List.map Game.of_string input in
  List.fold_left (+) 0 @@ List.map (Cubes.power % Game.minBag) games

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected:expected ~actual:result

let () = time @@ fun () ->
  solve_file "input-ex0.txt" @@ (Some (8, 2286));
  solve_file "input-real0.txt" @@ (Some (2439, 63711));
  ()
