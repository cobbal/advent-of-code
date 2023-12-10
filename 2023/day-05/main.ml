open Utils

module SeedMap = struct
  type range = { lo : int; hi : int }
  let pp_range (fmt : Format.formatter) (r : range) : unit =
    Format.pp_print_string fmt @@ Fmt.str "(%d, %d)" r.lo r.hi
  let range_contains r i = r.lo <= i && i < r.hi
  let range_intersect r s =
    let lo = max r.lo s.lo in
    let hi = min r.hi s.hi in
    let res = if lo < hi then Some { lo = lo; hi = hi } else None in
    print_endline @@ Fmt.str "%a int %a -> %a" pp_range r pp_range s (Format.pp_print_option pp_range) res;
    res

  type segment = { source : range; offset : int }
  let pp_segment (fmt : Format.formatter) (map : segment) : unit =
    Format.pp_print_string fmt @@
    Fmt.str "%a%s%d" pp_range map.source (if map.offset >= 0 then "+" else "") map.offset

  type t = segment list [@@deriving show]

  let rec apply (map : t) (i : int) : int = match map with
    | [] -> i
    | seg :: _ when range_contains seg.source i -> i + seg.offset
    | _ :: map -> apply map i

  let rec apply_range (map : t) (r : range) : range list = match map with
    | [] -> [r]
    | seg :: rest -> begin match range_intersect seg.source r with
        | Some intersection -> [{
            lo = intersection.lo + seg.offset;
            hi = intersection.hi + seg.offset;
          }] @ ???
        | None -> apply_range rest r
      end

  let of_para : string list list -> t =
    let of_line : int list -> segment = function
      | [map_to; from; count] -> {
          source = { lo = from; hi = from + count };
          offset = map_to - from
        }
      | _ -> fail "parse error"
    in List.map (of_line % List.map int_of_string) % List.tl
end

let solve0 (input : string list) : int =
  let tokens = List.map (List.map words) @@ paragraphs input in
  (* print_endline @@ [%show: string list list list] tokens; *)
  match tokens with
  | ["seeds:" :: seed_strings] :: map_strings ->
    let seeds = List.map int_of_string seed_strings in
    let maps = List.map SeedMap.of_para map_strings in
    let loc_of_seed seed = List.fold_left (fun i map -> SeedMap.apply map i) seed maps in
    let locations = List.map loc_of_seed seeds in
    List.fold_left min (List.hd locations) locations
  | _ -> fail "parse error"

let solve1 (input : string list) : int =
  let tokens = List.map (List.map words) @@ paragraphs input in
  (* print_endline @@ [%show: string list list list] tokens; *)
  match tokens with
  | ["seeds:" :: seed_strings] :: map_strings ->
    let maps = List.map SeedMap.of_para map_strings in
    let mk_range (lo, count) = SeedMap.{ lo = lo; hi = lo + count } in
    let seeds = List.map mk_range @@ pairs @@ List.map int_of_string seed_strings in
    print_endline @@ [%show: SeedMap.range list] seeds;
    let loc_of_seeds seeds = List.fold_left (fun r map ->
        List.concat_map (SeedMap.apply_range map) r
      ) seeds maps in
    let locations = loc_of_seeds seeds in
    print_endline @@ [%show: SeedMap.range list] @@ SeedMap.apply_range (List.hd maps) (List.hd seeds);
    print_endline @@ [%show: SeedMap.range list] locations;
    0
    (* List.fold_left min (List.hd locations) locations *)
  | _ -> fail "parse error"

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected:expected ~actual:result

let () = solve_file "input-ex0.txt" @@ None
(* let () = solve_file "input-real0.txt" @@ None *)
