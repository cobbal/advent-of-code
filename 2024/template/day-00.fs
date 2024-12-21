module Day00

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) : int64 list list =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (Strings.split ' ' >> List.ofArray >> List.map int64)

let solvePart0 (input : string list) =
    -1

let solvePart1 (input : string list) =
    -1

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 00 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", None
                // "input-real0.txt", None
            }
