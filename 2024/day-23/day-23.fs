module Day23

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (Strings.split '-' >> Seq.assertPairs)
    |> fun pairs -> MultiMap.ofSeq (pairs @ List.map swap pairs)


let cliquesOf (graph : MultiMap<string, string>) : int -> Set<string> array =
    let rec loop =
        memo <|
        function
        | 3 ->
            seq {
                for KeyValue (a, ac) in graph do
                    for b in ac do
                        for c in Set.intersect ac graph[b] do
                            yield [| a ; b ; c |] |> Set.ofArray
            }
            |> Seq.distinct
            |> Array.ofSeq
        | n when n > 3 ->
            printfn $"looking for size %d{n}"
            seq {
                for clique in loop (n - 1) do
                    for candidate in graph.Keys do
                        if
                            not (Set.contains candidate clique)
                            && Set.isEmpty (Set.difference clique graph[candidate])
                        then
                            yield Set.add candidate clique
            }
            |> Seq.distinct
            |> Array.ofSeq
            |>! (printfn "cliquesOf %d, found %d" n << Array.length)
    loop

let solvePart0 (input : string list) =
    let cliquesOf = parse input |> cliquesOf
    cliquesOf 3 |> Seq.count (Set.exists (String.startsWith "t"))

let solvePart1 (input : string list) =
    let cliquesOf = parse input |> cliquesOf
    let rec search i =
        match cliquesOf i with
        | [| |] -> failwith "not unique"
        | [| theOne |] -> String.concat "," theOne
        | _ -> search (i + 1)
    search 3

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 23 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", None
                "input-real0.txt", None
            }
