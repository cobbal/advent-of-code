module type VS = sig
  include Map.OrderedType

  val pp : Format.formatter -> t -> unit
end

module Graph (V : VS) : sig
  val shortest_path : edges:(V.t -> (V.t * int) OSeq.iter) -> V.t -> V.t -> int * V.t list
end

module IntGraph : sig
  val shortest_path : edges:(int -> (int * int) OSeq.iter) -> int -> int -> int * int list
end
