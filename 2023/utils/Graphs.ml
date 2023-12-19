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

module IntGraph = struct
  type v = int [@@deriving show]
  type heap_entry = v * int [@@deriving show]

  type map_entry = {
    dist : int;
    prev : v option;
    token : heap_entry Pairing_heap.Elt.t; [@printer fun fmt _ -> Format.fprintf fmt "??"]
  }
  [@@deriving show { with_path = false }]

  (* Djikstra's *)
  let shortest_path ~(edges : v -> (v * int) OSeq.iter) (v_start : v) (v_end : v) : int * v list =
    let tab : map_entry option array ref = ref [||] in
    let ensure_tab_size n =
      let old_size = Array.length !tab in
      if n <= old_size then ()
      else
        let new_size = max n (2 * old_size) in
        let new_tab = Array.make new_size None in
        Array.blit !tab 0 new_tab 0 old_size;
        tab := new_tab
    in

    let tab_get i : map_entry option = if i < Array.length !tab then !tab.(i) else None in
    let tab_set i e =
      ensure_tab_size (i + 1);
      !tab.(i) <- Some e
    in
    let cmp (a : heap_entry) (b : heap_entry) = compare (snd a) (snd b) in
    let queue = Pairing_heap.create ~cmp () in
    let set (v : v) (dist : int) (prev : v option) =
      match tab_get v with
      | None ->
          let token = Pairing_heap.add_removable queue (v, dist) in
          tab_set v { dist; prev; token }
      | Some e ->
          let token = Pairing_heap.update queue e.token (v, dist) in
          tab_set v { dist; prev; token }
    in

    set v_start 0 None;
    let rec loop () =
      match Pairing_heap.pop queue with
      | None -> fail "not found"
      | Some (u, u_dist) when Stdlib.(u = v_end) ->
          let rec chase : v option -> v list = function
            | None -> []
            | Some v -> v :: chase (Option.get_exn_or "" (tab_get v)).prev
          in
          (u_dist, List.rev (chase (Some v_end)))
      | Some (u, u_dist) ->
          edges u (fun (v, w) ->
              match tab_get v with Some ve when ve.dist <= u_dist + w -> () | _ -> set v (u_dist + w) (Some u));
          loop ()
    in
    loop ()
end
