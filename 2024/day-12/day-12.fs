module Day12

open FSharpx
open Utils
open Grid

let neighbors (pos : XY) = [ pos.N ; pos.E ; pos.S ; pos.W ]

let regions (grid : Grid) =
    let rec fill target (visited : Set<XY>) (region : XY list) pos =
        if grid[pos] <> target || Set.contains pos visited then
            (visited, region)
        else
            List.fold (uncurry (fill target)) (Set.add pos visited, pos :: region) (neighbors pos)

    grid.Indices ()
    |> Seq.fold
        (fun (visited, regions) pos ->
            let target = grid[pos]
            let visited, region = fill target visited [] pos

            (visited,
             (if region <> [] then
                  (target, region) :: regions
              else
                  regions))
        )
        (Set.empty, [])
    |> snd

let perimiter (grid : Grid) region =
    let start = List.head region
    let target = grid[start]

    let inRegion pos = grid[pos] = target

    let notInRegion = not << inRegion
    Seq.map (neighbors >> List.count notInRegion) region |> Seq.sum

let discountPerimiter (grid : Grid) region =
    let start = List.head region
    let target = grid[start]

    let inRegion pos = grid[pos] = target

    let notInRegion = not << inRegion

    let count (pos : XY) =
        [|
            notInRegion pos.E && (inRegion pos.N.E || notInRegion pos.N)
            notInRegion pos.W && (inRegion pos.N.W || notInRegion pos.N)
            notInRegion pos.N && (inRegion pos.N.W || notInRegion pos.W)
            notInRegion pos.S && (inRegion pos.S.W || notInRegion pos.W)
        |]
        |> Seq.count id

    Seq.map count region |> Seq.sum

let solvePart0 (input : string list) : int64 =
    let grid = Grid input

    regions grid
    |> List.map (fun (_, region) -> int64 (List.length region) * perimiter grid region)
    |> List.sum

let solvePart1 (input : string list) : int64 =
    let grid = Grid input

    regions grid
    |> List.map (fun (_, region) -> int64 (List.length region) * int64 (discountPerimiter grid region))
    |> List.sum

let day12 =
    Day.day 12 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (1930, 1206))
    |> Day.addInput "input-real0.txt" (Some (1494342, 893676))
