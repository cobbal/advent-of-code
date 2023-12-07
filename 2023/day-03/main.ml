open Utils

type pchar =
  | Digit of int
  | Symbol of char
  | Blank

let pp_pchar fmt = function
  | Digit i -> Format.fprintf fmt "%d" i
  | Symbol '*' -> Format.fprintf fmt "*"
  | Symbol _ -> Format.fprintf fmt "@"
  | Blank -> Format.fprintf fmt "."

let is_symbol = function | Symbol _ -> true | _ -> false
let is_gear = function | Symbol '*' -> true | _ -> false

let print_line (line : pchar list) : unit =
  List.iter (comp print_string [%show: pchar]) line;
  print_newline ()

let print_board (board : pchar list list) : unit =
  List.iter print_line board;
  print_newline ()

let print_bools (bs : bool list) : unit =
  List.iter (print_int % function | false -> 0 | true -> 1) bs;
  print_newline ()
let print_bools_2d (bss : bool list list) : unit =
  List.iter print_bools bss;
  print_newline ()

let parse_char : char -> pchar = function
  | ('0' .. '9') as c -> Digit Char.(code c - code '0')
  | '.' -> Blank
  | c -> Symbol c

module PartFlag = struct
  type loc = int * int [@@deriving show]
  type t = loc option [@@deriving show]
  let of_pchar pred x y p = if pred p then Some (x, y) else None
  let join a b = Option.fold ~none:b ~some:Option.some a
end

let symbols (pred : pchar -> bool) : pchar list list -> PartFlag.t list list =
  List.mapi @@ fun x -> List.mapi @@ fun y -> PartFlag.of_pchar pred x y

let blur : PartFlag.t list -> PartFlag.t list =
  let rec blur2 prev = function
    | [] -> []
    | [cur] -> [PartFlag.join prev cur]
    | cur :: ((next :: _) as rest) -> PartFlag.join prev (PartFlag.join cur next) :: blur2 cur rest
  in blur2 None

let blur2d : PartFlag.t list list -> PartFlag.t list list =
  transpose % List.map blur % transpose % List.map blur

let rec split_while_map (f : 'a -> 'b option) : 'a list -> ('b list * 'a list) = function
  | [] -> ([], [])
  | x :: xs -> begin match f x with
      | Some y -> let (left, right) = split_while_map f xs in (y :: left, right)
      | None -> ([], x :: xs)
    end

let rec extract_numbers : (PartFlag.t * pchar) list -> (PartFlag.loc * int) list = function
  | [] -> []
  | ((_, Digit _) :: _) as l ->
    let (digits, rest) = split_while_map (function | (b, Digit i) -> Some (b, i) | _ -> None) l in
    begin match List.filter_map fst digits with
    | [] -> extract_numbers rest
    | loc :: _ -> (loc, undigits (List.map snd digits)) :: extract_numbers rest
    end
  | (_ :: rest) -> extract_numbers rest

let solve0 (input : string list) : int =
  let board = List.map (comp (List.map parse_char) (comp List.of_seq String.to_seq)) input in
  let annotated_board = zip2d (blur2d (symbols is_symbol board)) board in
  let part_numbers = List.concat_map extract_numbers annotated_board in
  sum (List.map snd part_numbers)

let solve1 (input : string list) : int =
  let board = List.map (comp (List.map parse_char) (comp List.of_seq String.to_seq)) input in
  let annotated_board = zip2d (blur2d (symbols is_gear board)) board in
  let part_numbers = List.sort compare (List.concat_map extract_numbers annotated_board) in
  let grouped = List.map (List.map snd) @@ group_on fst part_numbers in
  let ratios = List.filter_map (function | [x; y] -> Some (x * y) | _ -> None) grouped in
  sum ratios

let solve_file (filename : string) =
  let input = Core.In_channel.read_lines filename in
  print_endline Fmt.(str "%s: %d %d" filename (solve0 input) (solve1 input))

let () = solve_file "input-ex0.txt"
let () = solve_file "input-real0.txt"
