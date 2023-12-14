open Utils
open Containers

module IM = struct
  include Map.Make (Int)

  let pp = pp Int.pp
end

let card_order0 = "__23456789TJQKA"
let card_order1 = "J_23456789T_QKA"

let parse_hand card_order (input : string) =
  match words input with
  | [ hand; bid ] -> (List.map (String.index card_order) (List.of_seq (String.to_seq hand)), int_of_string bid)
  | _ -> fail "parse error"

let unparse_hand card_order = String.of_list % List.map (String.get card_order)
let joker = String.index card_order1 'J'

let kind_of_hand (h : int list) =
  let incr_opt = function None -> Some 1 | Some i -> Some (i + 1) in
  let hm = List.fold_left (fun map c -> IM.update c incr_opt map) IM.empty h in
  let joker_count = IM.get_or joker hm ~default:0 in
  let non_jokers = IM.remove joker hm in
  let raw_counts = List.rev @@ (0 :: (List.sort ( - ) @@ List.map snd @@ IM.to_list non_jokers)) in
  let counts = List.filter (fun i -> i > 1) @@ ((List.hd raw_counts + joker_count) :: List.tl raw_counts) in
  counts

type sort_me_t = int list * int list * int [@@deriving show, ord]

let solve (hands : (int list * int) list) : int =
  let sort_me = List.map (fun (hand, bid) -> (kind_of_hand hand, hand, bid)) hands in
  let sorted = List.sort compare_sort_me_t sort_me in
  let scores = List.mapi (fun i (_, _, bid) -> (i + 1) * bid) sorted in
  (* List.iter (print_endline % [%show: sort_me_t]) sorted; *)
  List.fold_left ( + ) 0 scores

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve @@ List.map (parse_hand card_order0) input, solve @@ List.map (parse_hand card_order1) input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (6440, 5905);
  solve_file "input-real0.txt" @@ Some (248422077, 249817836);
  ()
