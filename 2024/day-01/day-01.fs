module Day01

open Utils
open FSharpx
open FSharpx.Collections
open FSharpx.Text

let parse (input : string list) : int list * int list =
    let l =
        List.map (Strings.split ' ' >> Seq.choose FSharpOption.ParseInt >> Seq.assertPairs) input

    (List.map fst l, List.map snd l)

let solvePart0 (input : string list) : int =
    let (left, right) = parse input
    let zips = List.zip (List.sort left) (List.sort right)
    List.sum (List.map (fun (x, y) -> abs (x - y)) zips)

let solvePart1 (input : string list) : int =
    let left, right = parse input

    let occurrences =
        List.fold (fun acc k -> Map.change k (fun v -> Some (defaultArg v 0 + 1)) acc) Map.empty right

    let scores = List.map (fun x -> x * defaultArg (Map.tryFind x occurrences) 0) left
    List.sum scores

let day01: Day.Day =
    Day.day 01 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (11, 31))
    |> Day.addInput "input-real0.txt" (Some (765748, 27732508))
