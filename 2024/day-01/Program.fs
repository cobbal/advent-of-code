open FSharpx.Collections
open Utils
open System
open FSharpx
open FSharpx.Text
open FSharpx.IO

let parse (input: string list): int list * int list =
    let l = List.map (Strings.split ' ' >> Seq.choose FSharpOption.ParseInt >> Seq.assertPairs) input
    (List.map fst l, List.map snd l)

let solve0 (input: string list) : int =
    let (left, right) = parse input
    let zips = List.zip (List.sort left) (List.sort right)
    List.sum (List.map (fun (x, y) -> abs (x - y)) zips)

let solve1 (input: string list) : int =
    let left, right = parse input
    let occurrences = List.fold (fun acc k -> Map.change k (fun v -> Some (defaultArg v 0 + 1)) acc) Map.empty right
    let scores = List.map (fun x -> x * defaultArg (Map.tryFind x occurrences) 0) left
    List.sum scores

let solveFile (filename: String) (expected: (int * int) option) =
    let lines = readFile filename |> List.ofSeq
    let result = (solve0 lines, solve1 lines)
    printf $"%s{filename}: %A{result}"
    checkResults expected result

solveFile "input-ex0.txt" (Some (11, 31))
solveFile "input-real0.txt" (Some (765748, 27732508))
