module Day06

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

type Dir =
    | N
    | E
    | S
    | W

let step (x, y) =
    function
    | N -> (x, y - 1)
    | E -> (x + 1, y)
    | S -> (x, y + 1)
    | W -> (x - 1, y)

let rotate =
    function
    | N -> E
    | E -> S
    | S -> W
    | W -> N

let parse (input : string list) : char array array =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map Array.ofSeq
    |> Array.ofList

let find2 (arr : 'a array array) (elem : 'a) : (int * int) seq =
    seq {
        for y in 0 .. Array.length arr - 1 do
            for x in 0 .. Array.length arr[y] - 1 do
                if arr[y][x] = elem then
                    yield (x, y)
    }

let solvePart0 (input : string list) : int =
    let grid = parse input
    let start = find2 grid '^' |> Seq.head
    let height, width = Array.length grid, Array.length grid[0]

    let rec travel (x, y) (seen : Set<int * int>) (dir : Dir) =
        let seen = Set.add (x, y) seen
        let x', y' = step (x, y) dir

        if x' < 0 || width <= x' || y' < 0 || height <= y' then
            seen
        else if grid[y'][x'] = '#' then
            travel (x, y) seen (rotate dir)
        else
            travel (x', y') seen dir

    travel start Set.empty N |> Set.count

let visited (seen : Set<int * int * Dir>) (x, y) =
    Seq.exists (fun d -> Set.contains (x, y, d) seen) [ N ; E ; S ; W ]

let solvePart1 (input : string list) : int =
    let grid = parse input
    let start = find2 grid '^' |> Seq.head
    let height, width = Array.length grid, Array.length grid[0]

    let rec travel (x, y) (seen : Set<int * int * Dir>) (deviated : (int * int) option) (dir : Dir) =

        if Set.contains (x, y, dir) seen then
            Set.ofList (Option.toList deviated)
        else

        let seen = Set.add (x, y, dir) seen
        let x', y' = step (x, y) dir

        if x' < 0 || width <= x' || y' < 0 || height <= y' then
            Set.empty
        else if grid[y'][x'] = '#' || Some (x', y') = deviated then
            travel (x, y) seen deviated (rotate dir)
        else
            Set.union
                (match deviated with
                 | None when not <| visited seen (x', y') -> travel (x, y) seen (Some (x', y')) (rotate dir)
                 | _ -> Set.empty)
                (travel (x', y') seen deviated dir)

    travel start Set.empty None N |> Set.count

let day06 =
    Day.day 06 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (41, 6))
    |> Day.addInput "input-real0.txt" (Some (4967, 1789))
