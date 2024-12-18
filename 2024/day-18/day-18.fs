module Day18

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils
open Grid

let parse (input : string list) =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (Strings.int64sIn >> Seq.map int >> Seq.assertPairs >> XY)

let solve (grid : Grid) =
    let rec loop dists (needsUpdate : Deque<int * XY>) =
        match needsUpdate.TryUncons with
        | None -> dists
        | Some ((dist, pos), needsUpdate) ->
            match Map.tryFind pos dists with
            | Some storedDist when storedDist <= dist -> loop dists needsUpdate
            | _ ->
                loop
                    (Map.add pos dist dists)
                    (seq {
                        for next in [ pos.N ; pos.E ; pos.S ; pos.W ] do
                            if grid[next] = '.' then
                                yield (1 + dist, next)
                     }
                     |> Seq.fold (flip Deque.conj) needsUpdate)

    loop Map.empty (Deque.singleton (0, XY (grid.Width - 1, grid.Height - 1)))
    |> Map.tryFind (XY (0, 0))

let solvePart0 (input : string list) : int64 =
    let drops = parse input
    let w = (List.map (fun (xy : XY) -> xy.X) drops |> List.max) + 1
    let h = (List.map (fun (xy : XY) -> xy.Y) drops |> List.max) + 1
    let grid = Grid.Grid (Array.create (w * h) (byte '.'), w, h)

    let bytes = if w > 50 then 1024 else 12

    for xy in List.take bytes drops do
        grid[xy] <- '#'

    solve grid |> _.Value |> int64

// returns the index of the first true, or max if not found
let binBoolSearch (monotoneFn : int -> bool) : int * int -> int =
    let rec loop (min, max) =
        if min < max then
            let mid = (min + max) / 2

            if monotoneFn mid then
                loop (min, mid)
            else
                loop (mid + 1, max)
        else
            max

    loop

let solvePart1 (input : string list) : int64 =
    let drops = parse input
    let w = (List.map (fun (xy : XY) -> xy.X) drops |> List.max) + 1
    let h = (List.map (fun (xy : XY) -> xy.Y) drops |> List.max) + 1

    let blocked bytes =
        let grid = Grid.Grid (Array.create (w * h) (byte '.'), w, h)

        for xy in List.take bytes drops do
            grid[xy] <- '#'

        (solve grid).IsNone

    binBoolSearch blocked (0, List.length drops)
    |> fun i -> List.item (i - 1) drops
    |> fun pos -> pos.X * 100 + pos.Y
    |> int64

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 18 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (22, 601)
                "input-real0.txt", Some (232, 4464)
            }
