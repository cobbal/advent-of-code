val fail : string -> 'a
val ( % ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
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