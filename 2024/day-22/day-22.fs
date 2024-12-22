module Day22

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) = input |> List.map int64 |> Array.ofList

let prune v = v % 16777216L

let evolve s =
    let s = s ^^^ (s * 64L) |> prune
    let s = (s / 32L) ^^^ s |> prune
    let s = (s * 2048L) ^^^ s |> prune
    s

let evolveArray : int64 -> int64 array =
    memo (fun s ->
        { 0..2000 }
        |> Seq.fold (fun (s, acc) _ -> (evolve s, s :: acc)) (s, [])
        |> snd
        |> Array.ofSeq
        |> Array.rev
    )

let solvePart0 (input : string list) =
    parse input |> Array.map (evolveArray >> Array.last) |> Array.sum

let encode a b c d =
    1000000 * (10 + a) + 10000 * (10 + b) + 100 * (10 + c) + (10 + d)

let diffArray =
    memo (fun seed ->
        seed
        |> evolveArray
        |> Array.pairwise
        |> Array.map (fun (a, b) -> int (b % 10L - a % 10L))
        |> Array.pairwise4
        |> Array.map (fun (a, b, c, d) -> encode a b c d)
    )

let findPrice target seed =
    diffArray seed
    |> Array.tryFindIndex ((=) target)
    |> Option.map (fun i -> (evolveArray seed)[i + 4] % 10L)
    |> flip defaultArg 0L

let solvePart1 (input : string list) =
    let arr = parse input

    seq {
        for a in -9 .. 9 do
            for b in -9 .. 9 do
                if -9 <= a + b && a + b <= 9 then
                    for c in -9 .. 9 do
                        if -9 <= a + b + c && a + b + c <= 9 then
                            for d in -9 .. 9 do
                                if -9 <= a + b + c + d && a + b + c + d <= 9 then
                                    let target = encode a b c d
                                    yield Array.sumBy (findPrice target) arr
    }
    |> Seq.max

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 22 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (37327623L, 24L)
                "input-ex1.txt", Some (37990510L, 23L)
                "input-real0.txt", Some (13764677935L, 1619L)
            }
