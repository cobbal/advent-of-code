open Utils
open Containers
module SM = Map.Make (String)
module SS = Set.Make (String)

type ff_modl = { mutable state : bool; outputs : int array } [@@deriving show]
type conj_modl = { mutable state : int; outputs : int array } [@@deriving show]
type modl = FF of ff_modl | Conj of conj_modl | Special of int array [@@deriving show]
type network = { modls : modl array; labels : string array }

let unlabel_opt network s = Option.map fst @@ Array.find_idx (String.( = ) s) network.labels
let unlabel network s = Option.get_exn_or "bad label" @@ unlabel_opt network s
let outputs_of = function FF { outputs; _ } -> outputs | Conj { outputs; _ } -> outputs | Special outputs -> outputs

let parse_network (input : string list) : network =
  let parse_modl (line : string) =
    match String.split ~by:" ->" line with
    | [ left; right ] -> (
        let outputs = List.map String.trim (String.split_on_char ',' right) in
        match String.get left 0 with
        | '%' -> (String.drop 1 left, (`FF, outputs))
        | '&' -> (String.drop 1 left, (`Conj, outputs))
        | 'b' -> (left, (`Special, outputs))
        | _ -> fail "bad parse")
    | _ -> fail "bad parse"
  in
  let raw_modls = SM.of_list @@ List.map parse_modl input in
  let raw_labels =
    SS.to_list
    @@ SM.fold (fun name (typ, outputs) labels -> SS.union labels @@ SS.of_list (name :: outputs)) raw_modls SS.empty
  in
  let labels = Array.of_list ("broadcaster" :: List.filter (not % String.( = ) "broadcaster") raw_labels) in
  let unlabel = unlabel { labels; modls = Array.empty } in
  let modls =
    flip Array.mapi labels @@ fun index name ->
    match SM.get name raw_modls with
    | None -> Special Array.empty
    | Some (typ, outputs) -> (
        let outputs = Array.of_list @@ List.map unlabel outputs in
        match typ with
        | `FF -> FF { state = false; outputs }
        | `Conj -> Conj { state = -1; outputs }
        | `Special -> Special outputs)
  in

  let add_input sender receiver =
    match modls.(unlabel receiver) with
    | Conj conj -> conj.state <- conj.state land lnot (1 lsl unlabel sender)
    | _ -> ()
  in
  SM.iter (fun name (_, outputs) -> List.iter (add_input name) outputs) raw_modls;
  (*  Array.iter (print_endline % [%show: modl]) modls; *)
  { modls; labels }

let copy_network { modls; labels } =
  let copy_modl = function
    | FF { state; outputs } -> FF { state; outputs }
    | Conj { state; outputs } -> Conj { state; outputs }
    | Special outputs -> Special outputs
  in
  { modls = Array.map copy_modl modls; labels }

let preds (n : network) (target : int) : int list =
  Array.foldi
    (fun acc src modl -> Array.fold (fun acc dst -> if dst = target then src :: acc else acc) acc (outputs_of modl))
    [] n.modls

let queue : (int * bool * int) Queue.t = Queue.create ()

let press_button (network : network) (f : bool -> int -> unit) : unit =
  let push sender signal receiver =
    f signal receiver;
    (* Format.printf "%s -%s-> %s\n" network.labels.(sender) (if signal then "high" else "low") network.labels.(receiver); *)
    Queue.push (sender, signal, receiver) queue
  in
  let rec loop () =
    match Queue.take_opt queue with
    | None -> ()
    | Some (sender, signal, name) ->
        (match network.modls.(name) with
        | Special outputs -> Array.iter (push name signal) outputs
        | FF ({ state; outputs } as modl) ->
            if signal then ()
            else
              let state = not state in
              modl.state <- state;
              Array.iter (push name state) outputs
        | Conj ({ state; outputs } as modl) ->
            let state = if signal then state lor (1 lsl sender) else state land lnot (1 lsl sender) in
            modl.state <- state;
            let all_on = state = -1 in
            Array.iter (push name (not all_on)) outputs);
        loop ()
  in
  push 0 false 0;
  loop ()

let solve0 (input : string list) : int =
  let network = parse_network input in
  let lows, highs = (ref 0, ref 0) in
  for _ = 1 to 1000 do
    press_button network (fun signal _ -> incr @@ if signal then highs else lows)
  done;
  (*  print_endline @@ [%show: int * int] (!lows, !highs); *)
  !lows * !highs

let solve1 (input : string list) : int =
  let network = parse_network input in
  let find_rise target =
    let network = copy_network network in
    let finished = ref false in
    let presses = ref 0 in
    while not !finished do
      incr presses;
      press_button network (fun signal dest -> if (not signal) && dest = target then finished := true)
    done;
    !presses
  in
  match Option.map (preds network) (unlabel_opt network "rx") with
  (* This is somewhat specific to the puzzle input *)
  | Some [ pred ] ->
      let subcycles = List.map find_rise (preds network pred) in
      List.fold_left lcm 1 subcycles
  | _ -> -1

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (32000000, -1);
  solve_file "input-ex1.txt" @@ Some (11687500, -1);
  solve_file "input-real0.txt" @@ Some (681194780, 238593356738827);
  ()
