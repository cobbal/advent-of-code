module Day15

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils
open Grid

type dir =
    | N
    | E
    | S
    | W

module Dir =
    let ofChar =
        function
        | '^' -> N
        | '>' -> E
        | 'v' -> S
        | '<' -> W
        | c -> failwith $"bad direction {c}"

    let move (xy : XY) =
        function
        | N -> XY (xy.X, xy.Y - 1)
        | E -> XY (xy.X + 1, xy.Y)
        | S -> XY (xy.X, xy.Y + 1)
        | W -> XY (xy.X - 1, xy.Y)

let parse (input : string list) (gridder : char -> string) =
    let grid, moves = List.split Strings.isNullOrEmpty input
    (Grid.Grid (List.map (String.collect gridder) grid), String.concat "" moves |> Seq.map Dir.ofChar |> List.ofSeq)

let solvePart0 (input : string list) : int64 =
    let grid, moves = parse input (fun c -> System.String [| c |])
    let start = grid.FindIndicesOf '@' |> Seq.head
    grid[start] <- '.'

    let push pos dir =
        let firstBox = Dir.move pos dir

        let rec loop pos =
            if grid[pos] = 'O' then loop (Dir.move pos dir) else pos

        let lastBox = loop firstBox

        if grid[lastBox] = '.' then
            grid.Swap (firstBox, lastBox)
            firstBox
        else
            pos

    let endPos = List.fold push start moves
    // grid.Print (Map.ofList [ (endPos, '@') ])

    grid.FindIndicesOf 'O' |> Seq.sumBy (fun xy -> 100 * xy.Y + xy.X) |> int64

let solvePart1 (input : string list) : int64 =
    let grid, moves =
        parse input
        <| function
            | '#' -> "##"
            | '@' -> "@."
            | 'O' -> "[]"
            | '.' -> ".."
            | c -> failwith $"parseError: {c}"

    let start = grid.FindIndicesOf '@' |> Seq.head
    grid[start] <- '.'

    let push pos dir =
        // grid.Print (Map.ofList [ (pos, '@') ])
        let firstBox = Dir.move pos dir

        let rec moveSet (acc : Set<XY>) pos =
            match grid[pos], dir with
            | '.', _ -> Some acc
            | '#', _ -> None
            | '[', N | '[', S ->
                Option.maybe {
                    let! acc' = moveSet (Set.add pos acc) (Dir.move pos dir)
                    let right = Dir.move pos E
                    return! moveSet (Set.add right acc') (Dir.move right dir)
                }
            | ']', N | ']', S ->
                Option.maybe {
                    let! acc' = moveSet (Set.add pos acc) (Dir.move pos dir)
                    let left = Dir.move pos W
                    return! moveSet (Set.add left acc') (Dir.move left dir)
                }
            | '[', _ | ']', _ ->
                moveSet (Set.add pos acc) (Dir.move pos dir)
            | c, _ -> failwith $"unknown grid cell {c}"
        match moveSet Set.empty firstBox with
        | Some boxes ->
            let compareFn: dir -> XY -> int =
                function
                | N -> _.Y
                | E -> fun xy -> -xy.X
                | S -> fun xy -> -xy.Y
                | W -> _.X
            for xy in Seq.sortBy (compareFn dir) boxes do
                grid.Swap (xy, Dir.move xy dir)
            firstBox
        | None -> pos

    let endPos = List.fold push start moves
    // grid.Print (Map.ofList [ (endPos, '@') ])

    grid.FindIndicesOf '[' |> Seq.sumBy (fun xy -> 100 * xy.Y + xy.X) |> int64

let day15 =
    Day.day 15 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" None
    |> Day.addInput "input-ex1.txt" None
    |> Day.addInput "input-ex2.txt" None
    |> Day.addInput "input-real0.txt" None
