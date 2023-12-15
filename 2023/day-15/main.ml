open Utils
open Containers

let hash : string -> int = List.fold_left (fun a c -> (a + Char.code c) * 17 mod 256) 0 % list_of_string

let solve0 : string list -> int = function
  | [ line ] -> sum @@ List.map hash @@ String.split_on_char ',' line
  | _ -> fail "parse error"

type boxes = (string * int ref) list array

let print_boxes : boxes -> unit =
  Array.iteri @@ fun i l ->
  if not (List.is_empty l) then
    Format.printf "Box %d: %s\n" i @@ String.concat " "
    @@ List.map (fun (s, fl) -> Fmt.str "[%s %d]" s !fl) (List.rev l)

let array_update (arr : 'a array) (idx : int) (f : 'a -> 'a) : unit = arr.(idx) <- f arr.(idx)

let do_box (bs : boxes) (cmd : string) =
  let eq = String.( = ) in
  match String.chop_suffix ~suf:"-" cmd with
  | Some label -> array_update bs (hash label) (List.remove_assoc ~eq label)
  | None -> (
      match String.split_on_char '=' cmd with
      | [ label; fl_str ] ->
          let fl = int_of_string fl_str in
          array_update bs (hash label) (fun l ->
              match List.assoc_opt ~eq label l with
              | None -> (label, ref fl) :: l
              | Some old_fl ->
                  old_fl := fl;
                  l)
      | _ -> fail ("bad command: " ^ cmd))

let score_boxes : boxes -> int =
  sum % Array.to_list % Array.mapi (fun i -> sum % List.mapi (fun j (_, fl) -> (i + 1) * (j + 1) * !fl) % List.rev)

let solve1 : string list -> int = function
  | [ line ] ->
      let bs = Array.init 256 (const []) in
      List.iter (do_box bs) @@ String.split_on_char ',' line;
      score_boxes bs
  | _ -> fail "parse error"

let solve_file (filename : string) expected =
  let input = Core.In_channel.read_lines filename in
  let result = (solve0 input, solve1 input) in
  print_string @@ Fmt.str "%s: %s" filename @@ [%show: int * int] result;
  check_results ~expected ~actual:result

let () =
  time @@ fun () ->
  solve_file "input-ex0.txt" @@ Some (1320, 145);
  solve_file "input-real0.txt" @@ Some (521341, 252782);
  ()
