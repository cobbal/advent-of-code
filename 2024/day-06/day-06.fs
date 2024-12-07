module Day06

open System
open FSharpx.Collections
open FSharpx.Text
open Utils

type Dir =
    | N = 8
    | E = 4
    | S = 2
    | W = 1
    | Escaped = 0

type DirSet = int

[<Struct>]
type XY = { x : int ; y : int }

let moveInDirection : XY -> Dir -> XY =
    fun pos ->
        function
        | Dir.N -> { x = pos.x ; y = pos.y - 1 }
        | Dir.E -> { x = pos.x + 1 ; y = pos.y }
        | Dir.S -> { x = pos.x ; y = pos.y + 1 }
        | Dir.W -> { x = pos.x - 1 ; y = pos.y }
        | _ -> ArgumentOutOfRangeException () |> raise

let rotate =
    function
    | Dir.N -> Dir.E
    | Dir.E -> Dir.S
    | Dir.S -> Dir.W
    | Dir.W -> Dir.N
    | _ -> ArgumentOutOfRangeException () |> raise

let parse (input : string list) : char array array =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map Array.ofSeq
    |> Array.ofList

[<Struct>]
type Step = { pos : XY ; dir : Dir }

module Step =
    let Escaped =
        {
            pos = { x = 0 ; y = 0 }
            dir = Dir.Escaped
        }

    let isEscaped step = step.dir = Dir.Escaped

type Grid(grid : char array, width : int, height : int, start : XY) =
    member this.grid = grid
    member this.width = width
    member this.height = height
    member this.start = start

    member this.Item
        with get pos = this.grid[pos.y * width + pos.x]
        and set pos value = this.grid[pos.y * width + pos.x] <- value

    new(grid : char array array)
        =
        let width = Array.length grid[0]
        let height = Array.length grid

        let start =
            seq {
                for y in 0 .. height - 1 do
                    for x in 0 .. width - 1 do
                        if grid[y][x] = '^' then
                            yield { x = x ; y = y }
            }

        Grid (Array.concat grid, width, height, Seq.head start)

    member this.travel1 pos dir =
        let pos' = moveInDirection pos dir

        if pos'.x < 0 || width <= pos'.x || pos'.y < 0 || height <= pos'.y then
            Step.Escaped
        else if this[pos'] = '#' then
            { pos = pos ; dir = rotate dir }
        else
            { pos = pos' ; dir = dir }

    member this.trackedTravel1 (seen : Map<XY, DirSet>) pos dir =
        let seen = Map.change pos (fun v -> Some (defaultArg v 0 ||| int dir)) seen
        struct (seen, this.travel1 pos dir)

    member this.travel seen (pos : XY) dir : Set<XY> option =
        match this.trackedTravel1 seen pos dir with
        | seen', step when Step.isEscaped step -> Some (Set.ofSeq seen'.Keys)
        | seen', step -> this.travel seen' step.pos step.dir

    member this.loops : XY -> Dir -> bool =
        let stepOnce s =
            if Step.isEscaped s then s else this.travel1 s.pos s.dir

        let rec loop tortoise hare =
            let tortoise' = stepOnce tortoise
            let hare' = stepOnce (stepOnce hare)

            if Step.isEscaped hare' then false
            else if tortoise' = hare' then true
            else loop tortoise' hare'

        fun pos dir ->
            let start = { pos = pos ; dir = dir }
            loop start start

    member this.withObstacle pos =
        let newGrid = Grid (Array.copy this.grid, this.width, this.height, this.start)
        newGrid[pos] <- '#'
        newGrid

let solvePart0 (input : string list) : int64 =
    let grid = parse input |> Grid
    grid.travel Map.empty grid.start Dir.N |> Option.get |> Set.count |> int64

let solvePart1 (input : string list) : int64 =
    let grid = parse input |> Grid

    let rec travel (seen : Map<XY, DirSet>) (acc : Set<XY>) pos (dir : Dir) =
        match grid.trackedTravel1 seen pos dir with
        | _, step when Step.isEscaped step -> acc
        | seen', step ->
            let shouldBlock = pos <> step.pos && Map.findOrDefault step.pos 0 seen = 0

            let acc' =
                if shouldBlock && (grid.withObstacle step.pos).loops pos dir then
                    Set.add step.pos acc
                else
                    acc

            travel seen' acc' step.pos step.dir

    travel Map.empty Set.empty grid.start Dir.N |> Set.count |> int64

let day06 =
    Day.day 06 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (41, 6))
    |> Day.addInput "input-real0.txt" (Some (4967, 1789))
