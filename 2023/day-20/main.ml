open Utils
open Containers

module SM = struct
  include Map.Make (String)

  let pp = pp String.pp
end

module Queue = struct
  include Queue

  let pp element_pp fmt : 'a t -> unit = List.pp element_pp fmt % List.of_seq % to_seq
end

type ff_modl = { mutable state : bool; outputs : string list } [@@deriving show]
type conj_modl = { mutable state : bool SM.t; outputs : string list } [@@deriving show]
type modl = FF of ff_modl | Conj of conj_modl | BC of string list [@@deriving show]
type network = modl SM.t

let outputs_of : modl -> string list = function FF f -> f.outputs | Conj c -> c.outputs | BC outputs -> outputs

let parse_network (input : string list) : network =
  let parse_modl (line : string) : string * modl =
    match String.split ~by:" ->" line with
    | [ left; right ] -> (
        let outputs = List.map String.trim (String.split_on_char ',' right) in
        match String.get left 0 with
        | '%' -> (String.drop 1 left, FF { state = false; outputs })
        | '&' -> (String.drop 1 left, Conj { state = SM.empty; outputs })
        | 'b' -> (left, BC outputs)
        | _ -> fail "bad parse1")
    | _ -> fail "bad parse"
  in
  let modls = SM.of_list @@ List.map parse_modl input in
  let add_input sender receiver =
    match SM.get receiver modls with Some (Conj conj) -> conj.state <- SM.add sender false conj.state | _ -> ()
  in
  SM.iter (fun name modl -> List.iter (add_input name) (outputs_of modl)) modls;
  modls

let press_button (network : network) (f : string -> bool -> string -> unit) : unit =
  let queue : (string * bool * string) Queue.t = Queue.create () in
  let push sender signal receiver =
    f sender signal receiver;
    (*    Format.printf "%s -%s-> %s\n" sender (if signal then "high" else "low") receiver; *)
    Queue.push (sender, signal, receiver) queue
  in
  let rec loop () =
    match Queue.take_opt queue with
    | None -> ()
    | Some (sender, signal, name) ->
        (match SM.get name network with
        | Some (BC outputs) -> List.iter (push name signal) outputs
        | Some (FF ({ state; outputs } as modl)) ->
            if signal then ()
            else
              let state = not state in
              modl.state <- state;
              List.iter (push name state) outputs
        | Some (Conj ({ state; outputs } as modl)) ->
            let state = SM.add sender signal state in
            modl.state <- state;
            let all_on = SM.for_all (fun _ v -> v) state in
            List.iter (push name (not all_on)) outputs
        | None ->
            (*        print_endline @@  "module \"" ^ name ^ "\" not found"; *)
            ());
        loop ()
  in
  push "button" false "broadcaster";
  loop ()

let solve0 (input : string list) : int =
  let network = parse_network input in
  let lows, highs = (ref 0, ref 0) in
  for _ = 1 to 1000 do
    press_button network (fun _ signal _ -> incr @@ if signal then highs else lows)
  done;
  (*  print_endline @@ [%show: int * int] (!lows, !highs); *)
  !lows * !highs

let solve1 (input : string list) : int =
  if not (List.exists (String.mem ~sub:"rx") input) then -1 else
  let network = parse_network input in
  let finished = ref false in
  let presses = ref 0 in
  while not !finished do
    incr presses;
    if !presses mod 100000 = 0 then print_endline ("click " ^ string_of_int !presses);
    press_button network (fun _ signal dest -> if not signal && String.(dest = "rx") then finished := true)
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
