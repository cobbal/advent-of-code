﻿module Day00

open FSharpx.Collections
open FSharpx.Text
open FSharpx

let parse (input : string list) : int list list =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (Strings.split ' ' >> List.ofArray >> List.choose FSharpOption.ParseInt)

let solvePart0 (input : string list) : int =
    -1

let solvePart1 (input : string list) : int =
    -1

let day00 =
    Day.day 00 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" None
    // |> Day.addInput "input-real0.txt" None
