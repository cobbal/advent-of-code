open Utils
open Containers

module SM = struct
  include Map.Make (String)

  let pp = pp String.pp
end

module SS = Set.Make (String)

module Queue = struct
  include Queue

  let pp element_pp fmt : 'a t -> unit = List.pp element_pp fmt % List.of_seq % to_seq
end

type ff_modl = { mutable state : bool; outputs : int array } [@@deriving show]
type conj_modl = { mutable state : int64; outputs : int array } [@@deriving show]
type modl = FF of ff_modl | Conj of conj_modl | Special of int array [@@deriving show]
type network = { modls : modl array; labels : string array }

let unlabel network s = fst @@ Option.get_exn_or "bad label" @@ Array.find_idx (String.( = ) s) network.labels

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
        | `Conj -> Conj { state = -1L; outputs }
        | `Special -> Special outputs)
  in

  let add_input sender receiver =
    match modls.(unlabel receiver) with
    | Conj conj -> conj.state <- Int64.(conj.state land lnot (1L lsl unlabel sender))
    | _ -> ()
  in
  SM.iter (fun name (_, outputs) -> List.iter (add_input name) outputs) raw_modls;
(*  Array.iter (print_endline % [%show: modl]) modls; *)
  { modls; labels }

let queue : (int * bool * int) Queue.t = Queue.create ()
let press_button (network : network) (f : bool -> int -> unit) : unit =
  let push sender signal receiver =
(*    let lsend = network.labels.(sender) in *)
(*    let lrecv = network.labels.(receiver) in *)
    f signal receiver;
(*    Format.printf "%s -%s-> %s\n" lsend (if signal then "high" else "low") lrecv; *)
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
            let state = Int64.(if signal then state lor (1L lsl sender) else state land lnot (1L lsl sender)) in
            modl.state <- state;
            let all_on = Int64.(state = -1L) in
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
  match Array.find_idx (String.( = ) "rx") network.labels with
  | None -> -1
  | Some (rx_idx, _) ->
    let finished = ref false in
    let presses = ref 0 in
    let ticks = 1000000 in
    while not !finished do
      incr presses;
      if !presses mod ticks = 0 then print_endline ("click " ^ string_of_int (!presses / ticks) ^ "_" ^ String.drop 1 (string_of_int ticks));
      press_button network (fun signal dest -> if not signal && dest = rx_idx then finished := true)
    done;
    (*  print_endline @@ [%show: int * int] (!lows, !highs); *)
    !presses

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ None;
  solve_file "input-ex1.txt" @@ None;
    solve_file "input-real0.txt" @@ None;
  ()
