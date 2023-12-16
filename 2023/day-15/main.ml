open Utils
open Containers

let hash : string -> int = List.fold_left (fun a c -> (a + Char.code c) * 17 mod 256) 0 % list_of_string

let solve0 : string list -> int = function
  | [ line ] -> sum @@ List.map hash @@ String.split_on_char ',' line
  | _ -> fail "parse error"

type boxes = (string * int) list list

let print_boxes : boxes -> unit =
  List.iteri @@ fun i l ->
  if not (List.is_empty l) then
    Format.printf "Box %d: %s\n" i @@ String.concat " " @@ List.map (fun (s, fl) -> Fmt.str "[%s %d]" s fl) (List.rev l)

let rec list_update (l : 'a list) (f : 'a -> 'a) : int -> 'a list = function
  | 0 -> f (List.hd l) :: List.tl l
  | i -> List.hd l :: list_update (List.tl l) f (i - 1)

let rec update_assoc ~eq (k : 'a) (v : 'b) : ('a * 'b) list -> ('a * 'b) list = function
  | [] -> [ (k, v) ]
  | (k', _) :: rest when eq k k' -> (k, v) :: rest
  | kv :: rest -> kv :: update_assoc ~eq k v rest

let do_box (bs : boxes) (cmd : string) : boxes =
  let eq = String.( = ) in
  match String.chop_suffix ~suf:"-" cmd with
  | Some label -> list_update bs (List.remove_assoc ~eq label) (hash label)
  | None -> (
      match String.split_on_char '=' cmd with
      | [ label; fl_str ] ->
          let fl = int_of_string fl_str in
          list_update bs (update_assoc ~eq label fl) (hash label)
      | _ -> fail ("bad command: " ^ cmd))

let score_boxes : boxes -> int = sum % List.mapi (fun i -> sum % List.mapi (fun j (_, fl) -> (i + 1) * (j + 1) * fl))

let solve1 : string list -> int = function
  | [ line ] ->
      let empty = List.init 256 (const []) in
      score_boxes @@ List.fold_left do_box empty @@ String.split_on_char ',' line
  | _ -> fail "parse error"

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (1320, 145);
  solve_file "input-real0.txt" @@ Some (521341, 252782);
  ()
