module Day08

open FSharpx.Text
open Utils

let parse (input : string list) : char array array =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map Array.ofSeq
    |> Array.ofList

let makeMap (arr : char array array) : MultiMap<char, int * int> =
    seq {
        for y in 0 .. arr.Length - 1 do
            for x in 0 .. arr[y].Length - 1 do
                if arr[y][x] <> '.' then
                    yield (arr[y][x], (x, y))
    } |> MultiMap.ofSeq

let solvePart0 (input : string list) : int64 =
    let grid = parse input
    let width, height = grid[0].Length, grid.Length
    let ants = makeMap grid
    let inBounds (x, y) = 0 <= x && x < width && 0 <= y && y < height
    seq {
        for ant in ants.Values do
            for a0, a1 in Seq.allPairs ant ant do
                if a0 <> a1 then
                    let a2 = (2 * fst a0 - fst a1, 2 * snd a0 - snd a1)
                    if inBounds a2 then
                        yield a2
    } |> Set.ofSeq
    |> Set.count
    |> int64

let solvePart1 (input : string list) : int64 =
    let grid = parse input
    let width, height = grid[0].Length, grid.Length
    let ants = makeMap grid
    let inBounds (x, y) = 0 <= x && x < width && 0 <= y && y < height
    seq {
        for ant in ants.Values do
            for a0, a1 in Seq.allPairs ant ant do
                if a0 <> a1 then
                    let dx, dy = (fst a0 - fst a1, snd a0 - snd a1)
                    let dx, dy = (dx / gcd dx dy, dy / gcd dx dy)
                    let rec loop mult pos =
                        seq {
                            if inBounds pos then
                                yield pos
                                yield! loop mult (fst pos + mult * dx, snd pos + mult * dy)
                        }
                    yield! loop 1 a0
                    yield! loop -1 a0
    } |> Set.ofSeq
    |> Set.count
    |> int64

let day08 =
    Day.day 08 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (14, 34))
    |> Day.addInput "input-real0.txt" (Some (228, 766))
