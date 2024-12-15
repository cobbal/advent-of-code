module Day15

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils
open Grid

let parse (input : string list) (gridder : char -> string) =
    let grid, moves = List.split Strings.isNullOrEmpty input
    (Grid.Grid (List.map (String.collect gridder) grid), String.concat "" moves |> Seq.map Dir.ofChar |> List.ofSeq)

let solvePart0 (input : string list) : int64 =
    let grid, moves = parse input (fun c -> System.String [| c |])
    let start = grid.FindIndicesOf '@' |> Seq.head
    let boxes = grid.FindIndicesOf 'O' |> Set.ofSeq

    let grid =
        Grid.Grid (grid.Grid |> Array.map (fun b -> if b = byte '#' then b else byte '.'), grid.Width, grid.Height)

    let push (boxes, pos : XY) dir =
        let firstBox = pos[dir]

        let rec movedBoxes acc pos =
            if grid[pos] = '#' then
                None
            else if Set.contains pos boxes then
                movedBoxes (Set.add pos acc) pos[dir]
            else
                Some acc

        match movedBoxes Set.empty firstBox with
        | Some moved ->
            let boxes' =
                Set.difference boxes moved |> Set.union (Set.map (fun (xy: XY) -> xy[dir]) moved)

            (boxes', firstBox)
        | None -> (boxes, pos)

    let boxes, endPos = List.fold push (boxes, start) moves
    // grid.Print (Map.ofList ((endPos, '@') :: List.ofSeq (Seq.map (flip tuple2 'O') boxes)))
    boxes |> Seq.sumBy (fun xy -> 100 * xy.Y + xy.X) |> int64

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
    let boxes = grid.FindIndicesOf '[' |> Set.ofSeq

    let grid =
        Grid.Grid (grid.Grid |> Array.map (fun b -> if b = byte '#' then b else byte '.'), grid.Width, grid.Height)

    let print chr (boxes, pos) =
        grid.Print (
            Map.ofList (
                (pos, chr)
                :: List.ofSeq (Seq.collect (fun b -> [ (b, '[') ; (b.E, ']') ]) boxes)
            )
        )

    let push (boxes, pos : XY) dir =
        // print (Dir.toChar dir) (boxes, pos)
        let firstBox = pos[dir]

        let rec movedBoxes (acc : Set<XY>) pos =
            let getBox (pos : XY) =
                if Set.contains pos boxes then
                    Some pos
                else
                    let pos' = pos.W
                    if Set.contains pos' boxes then Some pos' else None

            if grid[pos] = '#' then
                None
            else

            match getBox pos with
            | None -> Some acc
            | Some box ->
                match dir with
                | N
                | S -> [ box ; box.E ]
                | E -> [ box.E ]
                | W -> [ box ]
                |> List.fold
                    (fun acc pos -> Option.bind (fun acc -> movedBoxes acc pos[dir]) acc)
                    (Some (Set.add box acc))

        match movedBoxes Set.empty firstBox with
        | Some moved ->
            let boxes' =
                Set.difference boxes moved |> Set.union (Set.map (fun (xy: XY) -> xy[dir]) moved)

            (boxes', firstBox)
        | None -> (boxes, pos)

    List.fold push (boxes, start) moves
    // |>! print '@'
    |> fst
    |> Seq.sumBy (fun xy -> 100 * xy.Y + xy.X)
    |> int64

let day15 =
    Day.day 15 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (10092, 9021))
    |> Day.addInput "input-ex1.txt" (Some (2028, 1751))
    |> Day.addInput "input-ex2.txt" (Some (908, 618))
    |> Day.addInput "input-real0.txt" (Some (1563092, 1582688))
