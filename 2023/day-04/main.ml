open Utils
open Containers

module SS = struct
  include Set.Make(String)
  let pp = pp String.pp
end

let parse (line : string): SS.t * SS.t =
  match String.split ~by:": " line with
  | [_; line] -> begin match String.split ~by: " | " line with
      | [left; right] -> (SS.of_list (words left), SS.of_list (words right))
      | _ -> fail @@ "bad parse of line: " ^ line
    end
  | _ -> fail @@ "bad parse of line: " ^ line

let count_wins (winners, guesses) = SS.cardinal @@ SS.inter winners guesses

let solve0 (input : string list) : int =
  let wins = List.map (count_wins % parse) input in
  let scores = List.map (fun n -> if n = 0 then 0 else Int.shift_left 1 (n - 1)) wins in
  sum scores

let solve1 (input : string list) : int =
  let wins = List.map (count_wins % parse) input in
  let rec count_cards : (int * int) list -> int list = function
    | [] -> []
    | (n, wins) :: rest -> n :: count_cards (incr n wins rest)
  and incr multiplicity count = function
    | (i, wins) :: cards when count > 0 -> (i + multiplicity, wins) :: incr multiplicity (count - 1) cards
    | l -> l
  in
  sum @@ count_cards @@ List.map (fun x -> (1, x)) wins

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected:expected ~actual:result

let () = time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (13, 30);
  solve_file "input-real0.txt" @@ Some (27454, 6857330);
  ()
