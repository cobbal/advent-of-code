module Day06

open Utils
open Grid

let rotate =
    function
    | N -> E
    | E -> S
    | S -> W
    | W -> N

[<Struct>]
type Step = { pos : XY ; dir : Dir option }

let travel1 (grid : Grid) (pos : XY) dir =
    let pos' = pos[dir]

    if not (grid.ContainsIndex pos') then
        { pos = pos' ; dir = None }
    else if grid[pos'] = '#' then
        { pos = pos ; dir = Some (rotate dir) }
    else
        { pos = pos' ; dir = Some dir }

let trackedTravel1 (grid : Grid) (seen : Set<XY>) pos dir =
    struct (Set.add pos seen, travel1 grid pos dir)

let rec travel grid seen (pos : XY) dir : Set<XY> option =
    match trackedTravel1 grid seen pos dir with
    | seen', { dir = None } -> Some seen'
    | seen', { pos = pos' ; dir = Some dir' } -> travel grid seen' pos' dir'

let loops grid : XY -> Dir -> bool =
    let stepOnce =
        function
        | { dir = None } as s -> s
        | { dir = Some dir ; pos = pos } -> travel1 grid pos dir

    let rec loop tortoise hare =
        let tortoise' = stepOnce tortoise
        let hare' = stepOnce (stepOnce hare)

        if hare'.dir = None then false
        else if tortoise' = hare' then true
        else loop tortoise' hare'

    fun pos dir ->
        let start = { pos = pos ; dir = Some dir }
        loop start start

let withObstacle (grid : Grid) pos =
    let newGrid = grid.Copy ()
    newGrid[pos] <- '#'
    newGrid

let solvePart0 (input : string list) : int64 =
    let grid = Grid input
    let start = Seq.head (grid.FindIndicesOf '^')
    travel grid Set.empty start Dir.N |> Option.get |> Set.count |> int64

let solvePart1 (input : string list) : int64 =
    let grid = Grid input
    let start = Seq.head (grid.FindIndicesOf '^')

    let rec travel (seen : Set<XY>) (acc : Set<XY>) pos (dir : Dir) =
        match trackedTravel1 grid seen pos dir with
        | _, { dir = None } -> acc
        | seen', ({ dir = Some stepDir } as step) ->
            let shouldBlock = pos <> step.pos && not (Set.contains step.pos seen)

            let acc' =
                if shouldBlock && loops (withObstacle grid step.pos) pos stepDir then
                    Set.add step.pos acc
                else
                    acc

            travel seen' acc' step.pos stepDir

    travel Set.empty Set.empty start Dir.N |> Set.count |> int64

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 06 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (41, 6)
                "input-real0.txt", Some (4967, 1789)
            }
