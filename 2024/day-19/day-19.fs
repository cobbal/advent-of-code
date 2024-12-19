module Day19

open System
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> function
        | head :: tail -> (String.splitString [| ", " |] StringSplitOptions.None head, tail)
        | _ -> failwith "bad parse"

module String =
    let containsAt (n : int) (sub : string) (large : string) : bool =
        sub.Length + n <= large.Length && String.substring' n sub.Length large = sub

let countCombinations : string array -> string -> int64 =
    memo2
    <| fun tiles str ->
        let skipTable =
            Array.init
                str.Length
                (fun i ->
                    tiles
                    |> Array.map (fun tile -> if String.containsAt i tile str then tile.Length else -1)
                )

        0
        |> recMemo (fun loop ->
            function
            | i when i = str.Length -> 1L
            | i ->
                seq {
                    for j in 0 .. tiles.Length - 1 do
                        match skipTable[i][j] with
                        | skip when skip > 0 -> loop (i + skip)
                        | _ -> ()
                }
                |> Seq.sum
        )

let solvePart0 (input : string list) : int64 =
    let tiles, goals = parse input

    goals |> List.count (countCombinations tiles >> (<>) 0L)

let solvePart1 (input : string list) =
    let tiles, goals = parse input
    goals |> List.sumBy (countCombinations tiles)

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 19 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (6L, 16L)
                "input-real0.txt", Some (350L, 769668867512623L)
            }
