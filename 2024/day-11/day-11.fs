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
