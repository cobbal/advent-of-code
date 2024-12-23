﻿module Day02

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) : int64 list list =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (Strings.split ' ' >> List.ofArray >> List.map int64)

let isSafe (level : int64 list) =
    let pairs = List.pairwise level
    let ascending = List.forall (uncurry (<)) pairs
    let descending = List.forall (uncurry (>)) pairs
    let maxDrift = List.fold (fun x (a, b) -> max x (abs (a - b))) 0L pairs
    (ascending || descending) && maxDrift <= 3L

let solvePart0 (input : string list) : int64 =
    let levels = parse input
    List.count isSafe levels

let solvePart1 (input : string list) : int64 =
    let levels = parse input

    let rec removeOnes =
        function
        | [] -> []
        | x :: xs -> xs :: (List.map (List.cons x) (removeOnes xs))

    List.count (List.exists isSafe << removeOnes) levels

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 02 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (2, 4)
                "input-real0.txt", Some (686, 717)
            }
