module Day05

open Utils
open FSharpx.Text
open FSharpx

let parse (input : string list) : MultiMap<int, int> * int list list =
    let part0, part1 =
        input
        |> List.filter (not << Strings.isNullOrEmpty)
        |> List.partition (Strings.contains "|")

    let pairs = List.map (Strings.split '|' >> Array.map int >> Seq.assertPairs) part0
    let prereqs = MultiMap.ofList <| List.map (fun (a, b) -> (b, a)) pairs

    let required =
        part1 |> List.map (Strings.split ',' >> Array.map int >> List.ofArray)

    (prereqs, required)

let satisfiesPrereqs (prereqs : MultiMap<int, int>) (l : int list) : bool =
    List.foldBack
        (fun page (good, seen) ->
            (good && Set.isEmpty (Set.intersect (MultiMap.get prereqs page) seen), Set.add page seen)
        )
        l
        (true, Set.empty)
    |> fst

let inCorrectOrder prereqs : int list -> bool =
    let postreqs = MultiMap.inverse prereqs
    fun l -> satisfiesPrereqs prereqs l && satisfiesPrereqs postreqs (List.rev l)

let mid l = List.item (List.length l / 2) l

let solvePart0 (input : string list) : int64 =
    let prereqs, required = parse input

    required
    |> List.filter (inCorrectOrder prereqs)
    |> List.map (int64 << mid)
    |> List.sum

let solvePart1 (input : string list) : int64 =
    let prereqs, required = parse input

    let rec sort =
        function
        | [] -> []
        | x :: xs ->
            match List.partition (flip Set.contains (MultiMap.get prereqs x)) xs with
            | [], _ -> x :: sort xs
            | before, after -> sort (before @ [ x ] @ after)

    List.filter (not << inCorrectOrder prereqs) required
    |> List.map (int64 << mid << sort)
    |> List.sum

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 05 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (143, 123)
                "input-real0.txt", Some (4185, 4480)
            }
