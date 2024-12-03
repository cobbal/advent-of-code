module Day02

open FSharpx.Collections
open FSharpx.Text
open FSharpx

let parse (input : string list) : int list list =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (Strings.split ' ' >> List.ofArray >> List.choose FSharpOption.ParseInt)

let isSafe (level : int list) =
    let pairs = List.pairwise level
    let ascending = List.forall (uncurry (<)) pairs
    let descending = List.forall (uncurry (>)) pairs
    let maxDrift = List.fold (fun x (a, b) -> max x (abs (a - b))) 0 pairs
    (ascending || descending) && maxDrift <= 3

let solvePart0 (input : string list) : int =
    let levels = parse input
    List.filter isSafe levels |> List.length

let solvePart1 (input : string list) : int =
    let levels = parse input

    let rec removeOnes =
        function
        | [] -> []
        | x :: xs -> xs :: (List.map (List.cons x) (removeOnes xs))

    List.filter (List.exists isSafe << removeOnes) levels |> List.length

let day02 =
    Day.day 02 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (2, 4))
    |> Day.addInput "input-real0.txt" (Some (686, 717))
