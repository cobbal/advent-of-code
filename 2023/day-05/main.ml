open Utils 

module SeedMap = struct
  type segment = { source : IntRangeSet.t; offset : int }
  let pp_segment (fmt : Format.formatter) (map : segment) : unit =
    Format.pp_print_string fmt @@
    Fmt.str "%a%s%d" IntRangeSet.pp map.source (if map.offset >= 0 then "+" else "") map.offset

  type t = segment list [@@deriving show]

  let rec apply (map : t) (i : int) : int = match map with
    | [] -> i
    | seg :: _ when IntRangeSet.mem i seg.source -> i + seg.offset
    | _ :: map -> apply map i

  let rec apply_set (map : t) (r : IntRangeSet.t) : IntRangeSet.t = match map with
    | [] -> r
    | seg :: rest -> 
      let overlap = IntRangeSet.inter r seg.source in 
      let missed = IntRangeSet.diff r seg.source in
      IntRangeSet.union (IntRangeSet.offset seg.offset overlap) (apply_set rest missed)

  let of_para : string list list -> t =
    let of_line : int list -> segment = function
      | [map_to; from; count] -> {
          source = IntRangeSet.range from (from + count - 1);
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
    let mk_range (lo, count) = IntRangeSet.range lo (lo + count - 1) in
    let seeds = List.fold_left IntRangeSet.union IntRangeSet.empty
      @@ List.map mk_range @@ pairs @@ List.map int_of_string seed_strings in
(*     print_endline @@ [%show: IntRangeSet.t] seeds; *)
    let loc_of_seeds seeds = List.fold_left (fun r map ->
        SeedMap.apply_set map r
      ) seeds maps in
    let locations = loc_of_seeds seeds in
    IntRangeSet.min_elt locations
    (* List.fold_left min (List.hd locations) locations *)
  | _ -> fail "parse error"

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected:expected ~actual:result

let () = solve_file "input-ex0.txt" @@ Some (35, 46)
let () = solve_file "input-real0.txt" @@ Some (525792406, 79004094)
