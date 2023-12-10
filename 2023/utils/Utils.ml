open Containers

let fail s = raise (Failure s)
let (%) f g x = f (g x)
let id x = x
let flip f x y = f y x

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

let rec tails : 'a list -> 'a list list = function
  | [] -> []
  | (_ :: xs) as l -> l :: tails xs

let rec transpose : 'a list list -> 'a list list = function
  | [] -> []
  | [] :: _ -> []
  | (x :: xs) :: xss -> (x :: List.map List.hd xss) :: (transpose (xs :: List.map List.tl xss))

let rec zip2d (xs : 'a list list) (ys : 'b list list) : ('a * 'b) list list =
  match xs, ys with
  | [], _ -> []
  | _, [] -> []
  | x :: xs, y :: ys -> List.combine x y :: zip2d xs ys

let sum = List.fold_left (+) 0

let rec span (p : 'a -> bool) : 'a list -> 'a list * 'a list = function
  | head :: tail when p head ->
    let (left, right) = span p tail in
    (head :: left, right)
  | l -> ([], l)

let rec group_by (f : 'a -> 'a -> bool) : 'a list -> 'a list list = function
  | [] -> []
  | x :: xs ->
    let (left, right) = span (f x) xs in
    (x :: left) :: group_by f right

let group_on (key : 'a -> 'b) : 'a list -> 'a list list = group_by (fun x y -> Stdlib.(key x = key y))
let group (key : 'a -> 'b) : 'a list -> 'a list list = group_by Stdlib.(=)

let words = List.filter (String.(<>) "") % String.split_on_char ' '

let undigits : int list -> int = List.fold_left (fun sum digit -> sum * 10 + digit) 0

let check_results ~expected ~actual = match expected with
  | Some expectation ->
    if Stdlib.(expectation = actual)
    then print_endline " \u{2705}"
    else (print_endline " \u{274c}"; fail "Wrong answer")
  | None ->
    print_newline ()

let rec split_by (pred : 'a -> bool) : 'a list -> 'a list list = function
  | [] -> [[]]
  | x :: xs when pred x -> [] :: split_by pred xs
  | x :: xs -> let (first, rest) = List.hd_tl (split_by pred xs) in (x :: first) :: rest

let paragraphs : string list -> string list list = split_by (String.(=) "")

let rec pairs = function
  | [] -> []
  | [x] -> fail "odd length list to pairs"
  | x :: y :: rest -> (x, y) :: pairs rest
