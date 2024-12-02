﻿module Program

open System
open Day
open FSharpx
open FSharpx.Option
open Utils
open FSharpx.IO
open FSharpx.Collections

let allDays =
    List.monoid {
        [ Day01.day01 ]
        [ Day02.day02 ]
    }

let days =
    let dayArgs = Environment.GetCommandLineArgs () |> Array.skip 1 |> List.ofArray

    let getDay : string -> Day option =
        let dayMap = List.map (fun day -> (day.dayNumber, day)) allDays |> Map.ofList
        FSharpOption.ParseInt >=> flip Map.tryFind dayMap

    if List.isEmpty dayArgs then
        allDays
    else
        let (good, bad) =
            List.map (fun x -> getDay x |> Choice.ofOption x) dayArgs
            |> List.partitionChoices

        if List.isEmpty bad then
            good
        else
            printfn $"Unknown days requested: %A{bad}"
            exit 1

flip List.map days
<| fun day ->
    let dayPath = $"day-%02d{day.dayNumber}"
    printfn $"=== %s{dayPath} ==="

    timer
    <| fun () ->
        flip List.map day.inputs
        <| fun (inputFile, expected) ->
            let lines = readFile $"%s{dayPath}/%s{inputFile}" |> List.ofSeq
            printf $"%s{inputFile}: "
            let result0 = day.solvePart0 lines
            printf $"%A{result0} "
            let result1 = day.solvePart1 lines
            printf $"%A{result1} "

            match expected with
            | Some expectation ->
                if expectation = (result0, result1) then
                    printfn " \u2705 good"
                    true
                else
                    printfn " \u274c bad"
                    false
            | None ->
                printfn ""
                true
        |> List.forall id
|> (fun successes ->
    if not (List.forall id successes) then
        (printfn ""
         printfn "\u274c Exiting due to earlier errors"
         exit 1)
)
