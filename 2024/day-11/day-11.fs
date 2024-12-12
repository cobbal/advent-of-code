module Day11

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) : int64 array =
    input |> List.head |> Strings.split ' ' |> Array.map int64

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
        [| i / powerOf10 ; i % powerOf10 |]
    | _ -> [| 2024L * i |]

let solvePart0 (input : string list) : int64 =
    let rocks = parse input

    Seq.fold (fun rocks _ -> Array.collect blink rocks) rocks (seq { 1..25 })
    |> Array.length
    |> int64

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

type MultiSet<'T when 'T : comparison> = Map<'T, int64>

module MultiSet =
    let of1Seq<'T when 'T : comparison> : 'T seq -> MultiSet<'T> =
        Seq.fold (fun m k -> Map.change k (fun v -> defaultArg v 0L + 1L |> Some) m) Map.empty

    let ofSeq<'T when 'T : comparison> : ('T * int64) seq -> MultiSet<'T> =
        Seq.fold (fun m (k, count) -> Map.change k (fun v -> defaultArg v 0L + count |> Some) m) Map.empty

    let collect (mapping : 'T -> 'U seq) (set : MultiSet<'T>) : MultiSet<'U> =
        seq {
            for kv in set do
                for k in mapping kv.Key do
                    yield (k, kv.Value)
        }
        |> ofSeq

    let count = Map.fold (fun acc _ count -> acc + count) 0L

let solvePart1 (input : string list) : int64 =
    let rocks = parse input |> MultiSet.of1Seq

    Seq.fold (fun rocks _ -> MultiSet.collect multiblink rocks) rocks (seq { 1..75 })
    |> MultiSet.count

let day11 =
    Day.day 11 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (55312L, 65601038650482L))
    |> Day.addInput "input-real0.txt" (Some (217812L, 259112729857522L))
