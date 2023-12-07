let fail s = raise (Failure s)
let comp f g x = f (g x)
let (%) = comp

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

let group (l : 'a list) : 'a list list = group_by (==) l
let group_on (key : 'a -> 'b) : 'a list -> 'a list list = group_by (fun x y -> key x == key y)

let undigits : int list -> int = List.fold_left (fun sum digit -> sum * 10 + digit) 0
