module Make : functor (Ord : Map.OrderedType) -> sig
  val memo_fix : ((Ord.t -> 'a) -> Ord.t -> 'a) -> Ord.t -> 'a
end
