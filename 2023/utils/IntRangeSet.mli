type t
val pp : Format.formatter -> t -> unit
val empty : t
val range : int -> int -> t
val union : t -> t -> t
val inter : t -> t -> t
val disjoint : t -> t -> bool
val diff : t -> t -> t
val is_empty : t -> bool
val min_elt : t -> int
val max_elt : t -> int
val mem : int -> t -> bool
val offset : int -> t -> t