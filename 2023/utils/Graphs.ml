open Utils
open Containers

module type VS = sig
  include Map.OrderedType

  val pp : Format.formatter -> t -> unit
end

module Graph (V : VS) = struct
  module M = struct
    include Map.Make (V)

    let pp = pp V.pp
  end

  type heap_entry = V.t * int [@@deriving show]

  type map_entry = {
    dist : int;
    prev : V.t option;
    token : heap_entry Pairing_heap.Elt.t; [@printer fun fmt _ -> Format.fprintf fmt "??"]
  }
  [@@deriving show { with_path = false }]

  (* Djikstra's *)
  let shortest_path ~(edges : V.t -> (V.t * int) OSeq.iter) (v_start : V.t) (v_end : V.t) : int * V.t list =
    let tab : map_entry M.t ref = ref M.empty in
    let cmp (a : heap_entry) (b : heap_entry) = compare (snd a) (snd b) in
    let queue = Pairing_heap.create ~cmp () in
    let set (v : V.t) (dist : int) (prev : V.t option) =
      match M.find_opt v !tab with
      | None ->
          let token = Pairing_heap.add_removable queue (v, dist) in
          tab := M.add v { dist; prev; token } !tab
      | Some e ->
          let token = Pairing_heap.update queue e.token (v, dist) in
          tab := M.add v { dist; prev; token } !tab
    in

    set v_start 0 None;
    let rec loop () =
      match Pairing_heap.pop queue with
      | None -> fail "not found"
      | Some (u, u_dist) when Stdlib.(u = v_end) ->
          let rec chase : V.t option -> V.t list = function None -> [] | Some v -> v :: chase (M.find v !tab).prev in
          (u_dist, List.rev (chase (Some v_end)))
      | Some (u, u_dist) ->
          edges u (fun (v, w) ->
              match M.find_opt v !tab with Some ve when ve.dist <= u_dist + w -> () | _ -> set v (u_dist + w) (Some u));
          loop ()
    in
    loop ()
end
