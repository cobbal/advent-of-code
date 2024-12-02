open FSharpx.Collections
open FSharpx.Text
open Utils
open System
open FSharpx
open FSharpx.IO

let parse (input: string list) : int list list =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (Strings.split ' ' >> List.ofArray >> List.choose FSharpOption.ParseInt)

let isSafe (level: int list) =
    let pairs = List.pairwise level
    let ascending = List.forall (uncurry (<)) pairs
    let descending = List.forall (uncurry (>)) pairs
    let maxDrift = List.fold (fun x (a, b) -> max x (abs (a - b))) 0 pairs
    (ascending || descending) && maxDrift <= 3

let solve0 (input: string list) : int =
    let levels = parse input
    List.filter isSafe levels |> List.length

let solve1 (input: string list) : int =
    let levels = parse input
    let rec removeOnes = function
        | [] -> []
        | x :: xs -> xs :: (List.map (List.cons x) (removeOnes xs))
    List.filter (List.exists isSafe << removeOnes) levels |> List.length

let solveFile (filename: String) (expected: (int * int) option) =
    let lines = readFile filename |> List.ofSeq
    let result = (solve0 lines, solve1 lines)
    printf $"%s{filename}: %A{result}"
    checkResults expected result

solveFile "input-ex0.txt" (Some (2, 4))
solveFile "input-real0.txt" (Some (686, 717))
