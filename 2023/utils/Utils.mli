val fail : string -> 'a
val ( % ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val id : 'a -> 'a
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val const : 'a -> 'b -> 'a
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val repeatedly : int -> ('a -> 'a) -> 'a -> 'a
val tails : 'a list -> 'a list list
val transpose : 'a list list -> 'a list list
val zip2d : 'a list list -> 'b list list -> ('a * 'b) list list
val sum : int list -> int
val prod : int list -> int
val span : ('a -> bool) -> 'a list -> 'a list * 'a list
val group_by : ('a -> 'a -> bool) -> 'a list -> 'a list list
val group_on : ('a -> 'b) -> 'a list -> 'a list list
val group : ('a -> 'b) -> 'a list -> 'a list list
val words : string -> string list
val undigits : int list -> int
val check_results : expected:'a option -> actual:'a -> unit
val split_by : ('a -> bool) -> 'a list -> 'a list list
val paragraphs : string list -> string list list
val list_of_string : string -> char list
val string_of_list : char list -> string
val pairs : 'a list -> ('a * 'a) list
val time : (unit -> 'a) -> 'a
val gcd : int -> int -> int
val lcm : int -> int -> int
