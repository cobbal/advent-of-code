module Day10

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) : int array array =
    input
    |> Seq.filter (not << Strings.isNullOrEmpty)
    |> Seq.map (Seq.map (fun c -> int $"{c}") >> Array.ofSeq)
    |> Array.ofSeq

let trace (m : Monoid<'T>) (nine : int * int -> 'T) (grid : int array array) : 'T seq =
    let digits d =
        Seq.collecti (fun y -> Seq.choosei (fun x g -> if g = d then Some (x, y) else None)) grid

    let nines = digits 9 |> Seq.map (fun k -> (k, nine k)) |> Map.ofSeq

    seq { 8..-1..0 }
    |> Seq.fold
        (fun up d ->
            digits d
            |> Seq.map (fun (x, y) ->
                [ (x - 1, y) ; (x + 1, y) ; (x, y - 1) ; (x, y + 1) ]
                |> Seq.collect (fun k -> Seq.ofOption (Map.tryFind k up))
                |> m.Concat
                |> tuple2 (x, y)
            )
            |> Map.ofSeq
        )
        nines
    |> Map.values

let solvePart0 (input : string list) : int64 =
    parse input
    |> trace Set.monoid Set.singleton
    |> Seq.map Set.count
    |> Seq.sum
    |> int64

let solvePart1 (input : string list) : int64 =
    parse input |> trace (Monoid.sum ()) (fun _ -> 1) |> Seq.sum |> int64

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 10 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (36, 81)
                "input-real0.txt", Some (512, 1045)
            }
