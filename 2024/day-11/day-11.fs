module Day11

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) : int64 array =
    input
    |> List.head
    |> Strings.split ' '
    |> Array.map int64

let digitCount =
    let rec help acc =
        function
        | 0L -> acc
        | n -> help (1 + acc) (n / 10L)
    help 0

let blink i =
    match digitCount i with
    | 0 -> [| 1L |]
    | d when d % 2 = 0 ->
        let powerOf10 = pown 10L (d / 2)
        [| i / powerOf10; i % powerOf10 |]
    | _ -> [| 2024L * i |]

let solvePart0 (input : string list) : int64 =
    let rocks = parse input
    Seq.fold (fun rocks _ -> Array.collect blink rocks) rocks (seq { 1 .. 25 })
    |> Array.length
    |> int64

let solvePart1 (input : string list) : int64 =
    -1

let day11 =
    Day.day 11 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" None
    |> Day.addInput "input-real0.txt" None
