open Utils

type range = { lo : int ; hi : int }

let pp_range fmt r = Format.fprintf fmt "(%d..%d)" r.lo r.hi

let range_mem i r = r.lo <= i && i <= r.hi
let is_range_empty r = r.lo > r.hi

type t = range list [@@deriving show]

let empty = []

let range_canon r = if is_range_empty r then [] else [r]

let range lo hi : t = range_canon { lo = lo; hi = hi }

let pred i = if i == min_int then i else i - 1
let succ i = if i == max_int then i else i + 1

let range_union x y =
  if is_range_empty x then range_canon y
  else if is_range_empty y then range_canon x
  else if range_mem (pred y.lo) x || range_mem (succ y.hi) y
    || range_mem x.lo y || range_mem y.lo x
    then range (min x.lo y.lo) (max x.hi y.hi)
    else [x; y]

let canon : t -> t = 
  let rec help = function
  | [] -> []
  | [x] -> range_canon x
  | x :: y :: rest -> begin match range_union x y with
    | [xy] -> help (xy :: rest)
    | _ -> range_canon x @ help (y :: rest)
    end 
  in help % List.sort (fun x y -> x.lo - y.lo)

let union a b = canon (a @ b)

let inter (a : t) (b : t) : t = 
  let range_intersect r s = range (max r.lo s.lo) (min r.hi s.hi) in
  let intersect_b r = List.concat_map (range_intersect r) b in
  canon @@ List.concat_map intersect_b a

let is_empty = List.for_all is_range_empty

(* TODO: efficiency *)
let disjoint a b = is_empty (inter a b)

(* TODO: efficiency *)
let min_elt a = (List.hd (canon a)).lo
let max_elt a = (List.hd (List.rev (canon a))).hi

let mem i = List.exists (range_mem i)

let complement =
  let rec comp lo = function
  | [] -> if lo < max_int then range lo max_int else []
  | r :: rest -> (if r.lo > lo then range lo (pred r.lo) else []) @ comp (succ r.hi) rest
  in comp min_int

let diff a b = inter a (complement b)
(*   let range_diff (r : range) (s : range) : t =
    if r.hi <= s.lo || s.hi <= r.lo then [r] 
  in
  [] *)

let offset diff = List.map (fun r -> { lo = r.lo + diff; hi = r.hi + diff })