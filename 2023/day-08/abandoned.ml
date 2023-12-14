module type BoolVecType = sig
  type t

  val of_list : bool list -> t
  val lnot : t -> t
  val ( land ) : t -> t -> t
  val ( lor ) : t -> t -> t
  val exists : t -> bool
  val all : t -> bool
  val pp : Format.formatter -> t -> unit
end

module type BoolMatType = sig
  type t

  module Vec : BoolVecType

  val of_sparse : (int * int) list -> t
  val apply : t -> Vec.t -> Vec.t
  val pp : Format.formatter -> t -> unit
end

module MachineF (Mat : BoolMatType) : sig
  type t

  val pp : Format.formatter -> t -> unit
  val create : string list -> t
  val start : t -> Mat.Vec.t
  val final : t -> Mat.Vec.t -> bool
end = struct
  module Vec = Mat.Vec

  type t = {
    size : int;
    labels : string array;
    index : string -> int;
    left_matrix : Mat.t;
    right_matrix : Mat.t;
    start_states : Vec.t;
    end_states : Vec.t;
  }
  [@@deriving show]

  let create (lines : string list) : t =
    let parsed = List.map parse_line lines in
    let labels = Array.of_list @@ List.map fst parsed in
    let rev_labels = SM.of_list @@ List.mapi (fun i (label, _) -> (label, i)) parsed in
    let index l = Option.get_exn_or "bad label" (SM.get l rev_labels) in
    let size = Array.length labels in
    let left_edges, right_edges =
      List.split @@ flip List.map parsed
      @@ fun (from, (left, right)) -> ((index from, index left), (index from, index right))
    in
    let left_matrix = Mat.of_sparse left_edges in
    let right_matrix = Mat.of_sparse right_edges in
    let start_states = Vec.of_list @@ List.map (String.ends_with ~suffix:"A" % fst) parsed in
    let end_states = Vec.of_list @@ List.map (String.ends_with ~suffix:"Z" % fst) parsed in
    { size; labels; index; left_matrix; right_matrix; start_states; end_states }

  let start m = m.start_states
  let final m v = Vec.(all (lnot v lor m.end_states))
end

module ArrayBoolMat : BoolMatType = struct
  module Vec = struct
    type t = bool array [@@deriving show]

    let of_list = Array.of_list
    let lnot = Array.map Stdlib.not
    let ( land ) = Array.map2 ( && )
    let ( lor ) = Array.map2 ( || )
    let exists = Array.exists id
    let all = Array.for_all id
    let dot = Array.exists2 ( && )
  end

  type t = Vec.t array [@@deriving show]

  let of_sparse l : t =
    let size = List.length l in
    let rows = Array.init size (fun _ -> Array.make size false) in
    List.iter (fun (i, j) -> Array.set (Array.get rows i) j true) l;
    rows

  let apply m v = Array.map (Vec.dot v) m
end

module Machine = MachineF (ArrayBoolMat)

let stream_ends m (dirs : string) : int -> int Stream.t =
  let memo : int Stream.t IM.t IM.t ref = ref IM.empty in
  let period = String.length dirs in
  let rec recur dirs tick here =
    match IM.get here @@ IM.get_or tick !memo ~default:IM.empty with
    | Some result -> result
    | None ->
        let distance, next, dirs = run m 0 dirs here in
        let result = Stream.Cell (lazy (distance, recur dirs ((tick + distance + 1) mod period) next)) in
        memo := IM.update tick (fun old -> Some (IM.add here result (Option.get_or old ~default:IM.empty))) !memo;
        result
  in
  recur (Stream.cycle (List.of_seq (String.to_seq dirs))) 0

module Machine = struct
  type t = {
    labels : string array;
    index : string -> int;
    left_tx : int array;
    right_tx : int array;
    start_states : int list;
    end_states : IS.t;
  }
  [@@deriving show]

  let create (lines : string list) start_sufix end_suffix : t =
    let parsed = List.map parse_line lines in

    let labels = Array.of_list @@ List.map fst parsed in
    let rev_labels = SM.of_list @@ List.mapi (fun i (label, _) -> (label, i)) parsed in
    let index l = Option.get_exn_or "bad label" (SM.get l rev_labels) in
    let left_tx = Array.of_list (List.map (index % fst % snd) parsed) in
    let right_tx = Array.of_list (List.map (index % snd % snd) parsed) in
    let suffixed_indices s =
      List.concat % List.mapi (fun i (name, _) -> if String.ends_with ~suffix:s name then [ i ] else [])
    in
    let start_states = suffixed_indices start_sufix parsed in
    let end_states = IS.of_list (suffixed_indices end_suffix parsed) in
    { labels; index; left_tx; right_tx; start_states; end_states }

  let left m : int -> int = Array.get m.left_tx
  let right m : int -> int = Array.get m.right_tx
  let ended m = List.for_all (flip IS.mem m.end_states)

  let rec run m i dirs here =
    if i > 0 && IS.mem here m.end_states then (i, here, dirs)
    else
      let dir, dirs = Stream.uncons dirs in
      let next = if Char.(dir = 'L') then left m here else right m here in
      run m (i + 1) dirs next

  let rec run_many m i dirs here =
    if ended m here then i
    else
      let dir, dirs = Stream.uncons dirs in
      run_many m (i + 1) dirs @@ List.map (if Char.(dir = 'L') then left m else right m) here

  let stream_ends m : char Stream.t -> int -> int Stream.t =
    let rec recur dirs here =
      let distance, next, dirs = run m 0 dirs here in
      let result = Stream.Cell (lazy (distance, recur dirs next)) in
      result
    in
    recur
end
