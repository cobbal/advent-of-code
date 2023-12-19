open Utils
open Containers

type var = X | M | A | S [@@deriving show { with_path = false }]
type test = LT of var * int | GT of var * int [@@deriving show { with_path = false }]
type action = Accept | Reject | Goto of string [@@deriving show { with_path = false }]
type workflow = (test * action) list * action [@@deriving show]
type part = { x : int; m : int; a : int; s : int } [@@deriving show { with_path = false }]

type part_range = { xr : int * int; mr : int * int; ar : int * int; sr : int * int }
[@@deriving show { with_path = false }]

let parse_var : char -> var = function 'x' -> X | 'm' -> M | 'a' -> A | 's' -> S | _ -> fail "bad parse"
let parse_action : string -> action = function "A" -> Accept | "R" -> Reject | l -> Goto l

let parse_test (s : string) : test =
  match list_of_string s with
  | v :: '<' :: i -> LT (parse_var v, int_of_string (string_of_list i))
  | v :: '>' :: i -> GT (parse_var v, int_of_string (string_of_list i))
  | _ -> fail "bad parse"

let parse_step (s : string) : test * action =
  match String.split_on_char ':' s with
  | [ test_str; action_str ] -> (parse_test test_str, parse_action action_str)
  | _ -> fail "bad parse"

let parse_workflow (line : string) : string * workflow =
  match String.split_on_char '{' (Option.get_exn_or "bad parse" (String.chop_suffix ~suf:"}" line)) with
  | [ name; step_str ] ->
      let step_parts = List.rev (String.split_on_char ',' step_str) in
      let last_act = parse_action (List.hd step_parts) in
      let steps = List.map parse_step (List.rev (List.tl step_parts)) in
      (name, (steps, last_act))
  | _ -> fail "bad parse"

let parse_part (s : string) : part =
  match List.map int_of_string @@ words (String.map (function '0' .. '9' as c -> c | _ -> ' ') s) with
  | [ x; m; a; s ] -> { x; m; a; s }
  | _ -> fail "bad parse"

module SM = struct
  include Map.Make (String)

  let pp = pp String.pp
end

let parse (input : string list) : workflow SM.t * part list =
  match paragraphs input with
  | [ workflow_lines; part_lines ] ->
      (SM.of_list @@ List.map parse_workflow workflow_lines, List.map parse_part part_lines)
  | _ -> fail "bad parse"

let eval (flows : workflow SM.t) (p : part) : bool =
  let rec eval_flow : workflow -> bool = function
    | [], act -> eval_act act
    | (test, act) :: rest, final -> if eval_test test then eval_act act else eval_flow (rest, final)
  and eval_act = function
    | Accept -> true
    | Reject -> false
    | Goto l -> eval_flow (Option.get_exn_or "no workflow" (SM.get l flows))
  and eval_test = function LT (v, i) -> eval_var v < i | GT (v, i) -> eval_var v > i
  and eval_var = function X -> p.x | M -> p.m | A -> p.a | S -> p.s in
  eval_act (Goto "in")

let solve0 (input : string list) : int =
  let workflows, parts = parse input in
  let accepted = List.filter (eval workflows) parts in
  sum @@ List.map (fun p -> p.x + p.m + p.a + p.s) accepted

let cut (p : part_range) : test -> part_range list * part_range list =
  let range = function X -> p.xr | M -> p.mr | A -> p.ar | S -> p.sr in
  let update v r =
    match v with X -> { p with xr = r } | M -> { p with mr = r } | A -> { p with ar = r } | S -> { p with sr = r }
  in
  function
  | LT (v, i) ->
      let vlo, vhi = range v in
      if vhi < i then ([ p ], [])
      else if vlo >= i then ([], [ p ])
      else ([ update v (vlo, i - 1) ], [ update v (i, vhi) ])
  | GT (v, i) ->
      let vlo, vhi = range v in
      if vlo > i then ([ p ], [])
      else if vhi <= i then ([], [ p ])
      else ([ update v (i + 1, vhi) ], [ update v (vlo, i) ])

let eval_abstract (flows : workflow SM.t) : part_range list =
  let rec eval_flow p : workflow -> part_range list = function
    | [], act -> eval_act p act
    | (test, act) :: rest, final ->
        let true_parts, false_parts = cut p test in
        List.concat_map (flip eval_act act) true_parts @ List.concat_map (flip eval_flow (rest, final)) false_parts
  and eval_act p = function
    | Accept -> [ p ]
    | Reject -> []
    | Goto l -> eval_flow p (Option.get_exn_or "no workflow" (SM.get l flows))
  in
  eval_act { xr = (1, 4000); mr = (1, 4000); ar = (1, 4000); sr = (1, 4000) } (Goto "in")

let count_parts p =
  let vcount (lo, hi) = hi - lo + 1 in
  vcount p.xr * vcount p.mr * vcount p.ar * vcount p.sr

let solve1 (input : string list) : int =
  let workflows, parts = parse input in
  let abs_parts = eval_abstract workflows in
  (* List.iter (print_endline % [%show: part_range]) abs_parts; *)
  sum @@ List.map count_parts abs_parts

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (19114, 167409079868000);
  solve_file "input-real0.txt" @@ Some (376008, 124078207789312);
  ()
