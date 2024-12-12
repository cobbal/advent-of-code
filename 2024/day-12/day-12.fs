module Day12

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) : char array array =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map Array.ofSeq
    |> Array.ofList

let neighbors (x, y) =
    [ (x - 1, y) ; (x + 1, y) ; (x, y - 1) ; (x, y + 1) ]

let regions (grid : char array array) =
    let width, height = grid[0].Length, grid.Length

    let rec fill target (visited : Set<int * int>) (region : (int * int) list) (x, y) =
        if
            x < 0
            || width <= x
            || y < 0
            || height <= y
            || grid[y][x] <> target
            || Set.contains (x, y) visited
        then
            (visited, region)
        else

        List.fold (uncurry (fill target)) (Set.add (x, y) visited, (x, y) :: region) (neighbors (x, y))

    Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })
    |> Seq.fold
        (fun (visited, regions) (x, y) ->
            let target = grid[y][x]
            let visited, region = fill target visited [] (x, y)

            (visited,
             (if region <> [] then
                  (target, region) :: regions
              else
                  regions))
        )
        (Set.empty, [])
    |> snd

let perimiter (grid : char array array) region =
    let width, height = grid[0].Length, grid.Length
    let x, y = List.head region
    let target = grid[y][x]

    let inRegion (x, y) =
        0 <= x && x < width && 0 <= y && y < height && grid[y][x] = target

    let notInRegion = not << inRegion
    Seq.map (neighbors >> List.count notInRegion) region |> Seq.sum

let discountPerimiter (grid : char array array) region =
    let width, height = grid[0].Length, grid.Length
    let x, y = List.head region
    let target = grid[y][x]

    let inRegion (x, y) =
        0 <= x && x < width && 0 <= y && y < height && grid[y][x] = target

    let notInRegion = not << inRegion

    let count (x, y) =
        let hasLeftEdge = notInRegion (x - 1, y)
        let dontCountLeft = notInRegion (x - 1, y - 1) && inRegion (x, y - 1)

        let hasRightEdge = notInRegion (x + 1, y)
        let dontCountRight = notInRegion (x + 1, y - 1) && inRegion (x, y - 1)

        let hasTopEdge = notInRegion (x, y - 1)
        let dontCountTop = notInRegion (x - 1, y - 1) && inRegion (x - 1, y)

        let hasBottomEdge = notInRegion (x, y + 1)
        let dontCountBottom = notInRegion (x - 1, y + 1) && inRegion (x - 1, y)

        let account has dontCount = if has && not dontCount then 1 else 0

        account hasLeftEdge dontCountLeft
        + account hasRightEdge dontCountRight
        + account hasTopEdge dontCountTop
        + account hasBottomEdge dontCountBottom

    Seq.map count region |> Seq.sum

let solvePart0 (input : string list) : int64 =
    let grid = parse input

    regions grid
    |> List.map (fun (_, region) -> int64 (List.length region) * perimiter grid region)
    |> List.sum

let solvePart1 (input : string list) : int64 =
    let grid = parse input

    regions grid
    |> List.map (fun (_, region) -> int64 (List.length region) * int64 (discountPerimiter grid region))
    |> List.sum

let day12 =
    Day.day 12 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (1930, 1206))
    |> Day.addInput "input-real0.txt" (Some (1494342, 893676))
