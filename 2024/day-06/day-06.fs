module Day06

open System
open FSharpx.Collections
open FSharpx.Text
open Utils
open Grid

type Dir =
    | N = 8
    | E = 4
    | S = 2
    | W = 1
    | Escaped = 0

type DirSet = int

let moveInDirection : XY -> Dir -> XY =
    fun pos ->
        function
        | Dir.N -> pos.N
        | Dir.E -> pos.E
        | Dir.S -> pos.S
        | Dir.W -> pos.W
        | _ -> ArgumentOutOfRangeException () |> raise

let rotate =
    function
    | Dir.N -> Dir.E
    | Dir.E -> Dir.S
    | Dir.S -> Dir.W
    | Dir.W -> Dir.N
    | _ -> ArgumentOutOfRangeException () |> raise

[<Struct>]
type Step = { pos : XY ; dir : Dir }

module Step =
    let Escaped = { pos = XY (0, 0) ; dir = Dir.Escaped }

    let isEscaped step = step.dir = Dir.Escaped

let travel1 (grid : Grid) pos dir =
    let pos' = moveInDirection pos dir

    if not (grid.ContainsIndex pos') then
        Step.Escaped
    else if grid[pos'] = '#' then
        { pos = pos ; dir = rotate dir }
    else
        { pos = pos' ; dir = dir }

let trackedTravel1 (grid : Grid) (seen : Map<XY, DirSet>) pos dir =
    let seen = Map.change pos (fun v -> Some (defaultArg v 0 ||| int dir)) seen
    struct (seen, travel1 grid pos dir)

let rec travel grid seen (pos : XY) dir : Set<XY> option =
    match trackedTravel1 grid seen pos dir with
    | seen', step when Step.isEscaped step -> Some (Set.ofSeq seen'.Keys)
    | seen', step -> travel grid seen' step.pos step.dir

let loops grid : XY -> Dir -> bool =
    let stepOnce s =
        if Step.isEscaped s then s else travel1 grid s.pos s.dir

    let rec loop tortoise hare =
        let tortoise' = stepOnce tortoise
        let hare' = stepOnce (stepOnce hare)

        if Step.isEscaped hare' then false
        else if tortoise' = hare' then true
        else loop tortoise' hare'

    fun pos dir ->
        let start = { pos = pos ; dir = dir }
        loop start start

let withObstacle (grid : Grid) pos =
    let newGrid = grid.Copy ()
    newGrid[pos] <- '#'
    newGrid

let solvePart0 (input : string list) : int64 =
    let grid = Grid input
    let start = Seq.head (grid.FindIndicesOf '^')
    travel grid Map.empty start Dir.N |> Option.get |> Set.count |> int64

let solvePart1 (input : string list) : int64 =
    let grid = Grid input
    let start = Seq.head (grid.FindIndicesOf '^')

    let rec travel (seen : Map<XY, DirSet>) (acc : Set<XY>) pos (dir : Dir) =
        match trackedTravel1 grid seen pos dir with
        | _, step when Step.isEscaped step -> acc
        | seen', step ->
            let shouldBlock = pos <> step.pos && Map.findOrDefault step.pos 0 seen = 0

            let acc' =
                if shouldBlock && loops (withObstacle grid step.pos) pos dir then
                    Set.add step.pos acc
                else
                    acc

            travel seen' acc' step.pos step.dir

    travel Map.empty Set.empty start Dir.N |> Set.count |> int64

let day06 =
    Day.day 06 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (41, 6))
    |> Day.addInput "input-real0.txt" (Some (4967, 1789))
