module Day25

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) =
    input
    |> List.groupNeighboursBy Strings.isNullOrEmpty
    |> List.choose (
        function
        | true, _ -> None
        | false, x -> Some x
    )
    |> List.map (
        Seq.transpose
        >> Seq.map (
            Seq.groupNeighboursBy id
            >> Seq.map (fun (c, s) -> (c, Seq.length s))
            >> Seq.assertPairs
        )
    )
    |> List.map Array.ofSeq
    |> List.map (fun arr ->
        let isLock = arr[0] |> fst |> fst = '#'

        Array.map (fun ((_, l0), (_, l1)) -> if isLock then l0 - 1 else l1 - 1) arr
        |> if isLock then Choice1Of2 else Choice2Of2
    )
    |> List.partitionChoices

let keyFitsLock key lock =
    Array.forall2 (fun k l -> k + l < 6) key lock

let solvePart0 (input : string list) =
    let keys, locks = parse input
    Seq.allPairs keys locks |> Seq.count (uncurry keyFitsLock)

let solvePart1 (input : string list) = "freebie"

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 25 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (3, "freebie")
                "input-real0.txt", Some (3307, "freebie")
            }
