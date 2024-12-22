module Day22

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) = input |> List.map int |> Array.ofList

let prune v = v &&& 0xffffff

let evolve s =
    let s = (s <<< 6) ^^^ s |> prune
    let s = (s >>> 5) ^^^ s
    let s = (s <<< 11) ^^^ s
    s |> prune

let evolveArray : int -> int array =
    memo <| fun s -> { 1..2000 } |> Seq.scan (fun s _ -> evolve s) s |> Array.ofSeq

let solvePart0 (input : string list) =
    parse input |> Array.map (evolveArray >> Array.last >> int64) |> Array.sum

let encode a b c d =
    ((a * 32 + b) * 32 + c) * 32 + d

let summarizePrices seed =
    evolveArray seed
    |> Array.map (fun x -> x % 10)
    |> Array.fivewise
    |> Array.fold
        (fun counts (e0, e1, e2, e3, e4) ->
            let encoded = encode (e1 - e0) (e2 - e1) (e3 - e2) (e4 - e3)
            Map.change encoded (Option.orElse (Some e4)) counts
        )
        Map.empty

let solvePart1 (input : string list) =
    parse input
    |> Array.map summarizePrices
    |> Array.fold (Map.unionWith (konst (+))) Map.empty
    |> Map.values
    |> Seq.max

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 22 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (37327623L, 24)
                "input-ex1.txt", Some (37990510L, 23)
                "input-real0.txt", Some (13764677935L, 1619)
            }
