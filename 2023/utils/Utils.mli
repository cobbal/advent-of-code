val fail : string -> 'a
val ( % ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val id : 'a -> 'a
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val tails : 'a list -> 'a list list
val transpose : 'a list list -> 'a list list
val zip2d : 'a list list -> 'b list list -> ('a * 'b) list list
val sum : int list -> int
val span : ('a -> bool) -> 'a list -> 'a list * 'a list
val group_by : ('a -> 'a -> bool) -> 'a list -> 'a list list
val group_on : ('a -> 'b) -> 'a list -> 'a list list
val group : ('a -> 'b) -> 'a list -> 'a list list
val words : string -> Containers.String.t Containers.List.t
val undigits : int list -> int
val check_results : expected:('a option) -> actual:'a -> unit
val split_by : ('a -> bool) -> 'a list -> 'a list list
val paragraphs : string list -> string list list
val pairs : 'a list -> ('a * 'a) list
val time : (unit -> 'a) -> 'a
val gcd : int -> int -> int
val lcm : int -> int -> int