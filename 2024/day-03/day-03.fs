module Day03

open System.Text.RegularExpressions

let solvePart0 (input : string list) : int64 =
    let input = String.concat "\n" input
    let rx = Regex @"mul\((\d{1,3}),(\d{1,3})\)"

    rx.Matches input
    |> Seq.map (fun x -> int64 x.Groups[1].Value * int64 x.Groups[2].Value)
    |> Seq.sum

type Inst =
    | Enable
    | Disable
    | Mul of int64

let solvePart1 (input : string list) : int64 =
    let input = String.concat "\n" input
    let rx = Regex @"mul\((\d{1,3}),(\d{1,3})\)|do(n't)?\(\)"

    rx.Matches input
    |> Seq.map (fun x ->
        match x.Value with
        | "do()" -> Enable
        | "don't()" -> Disable
        | _ -> int64 x.Groups[1].Value * int64 x.Groups[2].Value |> Mul
    )
    |> Seq.fold
        (fun (enabled, total) ->
            function
            | Enable -> (true, total)
            | Disable -> (false, total)
            | Mul m -> (enabled, total + (if enabled then m else 0))
        )
        (true, 0L)
    |> snd

let day03 =
    Day.day 03 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (161, 161))
    |> Day.addInput "input-ex1.txt" (Some (161, 48))
    |> Day.addInput "input-real0.txt" (Some (174103751, 100411201))
