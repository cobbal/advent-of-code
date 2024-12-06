module Day06

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

type Dir =
    | N = 8
    | E = 4
    | S = 2
    | W = 1

type DirSet = int

let step (x, y) =
    function
    | Dir.N -> (x, y - 1)
    | Dir.E -> (x + 1, y)
    | Dir.S -> (x, y + 1)
    | Dir.W -> (x - 1, y)
    | _ -> System.ArgumentOutOfRangeException () |> raise

let rotate =
    function
    | Dir.N -> Dir.E
    | Dir.E -> Dir.S
    | Dir.S -> Dir.W
    | Dir.W -> Dir.N
    | _ -> System.ArgumentOutOfRangeException () |> raise

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

type Step =
    | Looped
    | Escaped of Map<int * int, DirSet>
    | Rotate of Map<int * int, DirSet> * Dir
    | Step of Map<int * int, DirSet> * (int * int)

type Grid(grid : char array array, width : int, height : int, start : int * int) =
    member this.grid = grid
    member this.width = width
    member this.height = height
    member this.start = start
    new(grid) = Grid (grid, Array.length grid[0], Array.length grid, find2 grid '^' |> Seq.head)

    member this.travel1 seen (x, y) dir =
        if Map.findOrDefault (x, y) 0 seen &&& int dir <> 0 then
            Looped
        else

        let seen = Map.change (x, y) (fun v -> Some (defaultArg v 0 ||| int dir)) seen
        let x', y' = step (x, y) dir

        if x' < 0 || width <= x' || y' < 0 || height <= y' then
            Escaped seen
        else if grid[y'][x'] = '#' then
            Rotate (seen, rotate dir)
        else
            Step (seen, (x', y'))

    member this.travel seen xy dir : Set<int * int> option =
        match this.travel1 seen xy dir with
        | Looped -> None
        | Escaped seen' -> Some (Set.ofSeq seen'.Keys)
        | Rotate (seen', dir') -> this.travel seen' xy dir'
        | Step (seen', xy') -> this.travel seen' xy' dir

    member this.withObstacle (x, y) (f : unit -> 'a option) : 'a option =
        if this.grid[y][x] = '.' then
            this.grid[y][x] <- '#'
            let res = f ()
            this.grid[y][x] <- '.'
            res
        else
            assert (this.grid[y][x] = '^')
            None

let solvePart0 (input : string list) : int =
    let grid = parse input |> Grid
    grid.travel Map.empty grid.start Dir.N |> Option.get |> Set.count

let solvePart1 (input : string list) : int =
    let grid = parse input |> Grid

    let rec travel (seen : Map<int * int, DirSet>) (acc : Set<int * int>) xy (dir : Dir) =
        match grid.travel1 seen xy dir with
        | Looped -> failwith "unexpected"
        | Escaped _ -> acc
        | Rotate (seen', dir') -> travel seen' acc xy dir'
        | Step (seen', (x', y')) ->
            let shouldBlock = Map.findOrDefault (x', y') 0 seen = 0
            let mutable acc' = acc
            if shouldBlock then
                assert (grid.grid[y'][x'] = '.')
                grid.grid[y'][x'] <- '#'
                if Option.isNone (grid.travel seen xy dir) then
                    acc' <- Set.add (x', y') acc
                grid.grid[y'][x'] <- '.'
            travel seen' acc' (x', y') dir

    travel Map.empty Set.empty grid.start Dir.N |> Set.count

let day06 =
    Day.day 06 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (41, 6))
    |> Day.addInput "input-real0.txt" (Some (4967, 1789))
