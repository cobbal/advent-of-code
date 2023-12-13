open Containers

module Make(Ord : Map.OrderedType) = struct
  module Map = Map.Make(Ord)
  let memo_fix (f : (Ord.t -> 'a) -> Ord.t -> 'a) : Ord.t -> 'a =
    let table : 'a Map.t ref = ref Map.empty in
    let rec recur x =
      match Map.get x !table with
      | Some cached -> cached
      | None ->
        let result = f recur x in
        table := Map.add x result !table;
        result
    in recur
end
