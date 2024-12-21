module Day11

open FSharpx.Text
open Utils

let parse (input : string list) : int64 array =
    input |> List.head |> Strings.split ' ' |> Array.map int64

let digitCount =
    let rec help acc =
        function
        | 0L -> acc
        | n -> help (1 + acc) (n / 10L)

    help 0

let multiblink i =
    match digitCount i with
    | 0 -> seq { 1L }
    | d when d % 2 = 0 ->
        let powerOf10 = pown 10L (d / 2)

        seq {
            i / powerOf10
            i % powerOf10
        }
    | _ -> seq { 2024L * i }

let solve blinks input =
    let rocks = MultiSet.of1Seq input

    Seq.fold (fun rocks _ -> MultiSet.collect multiblink rocks) rocks (seq { 1..blinks })
    |> MultiSet.count

let solvePart0 (input : string list) : int64 = parse input |> solve 25

let solvePart1 (input : string list) : int64 = parse input |> solve 75

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 11 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (55312L, 65601038650482L)
                "input-real0.txt", Some (217812L, 259112729857522L)
            }
