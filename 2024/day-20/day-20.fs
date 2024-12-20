module Day20

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils
open Grid

let parse (input : string list) =
    let grid = Grid input
    let start = grid.FindIndicesOf 'S' |> Seq.head
    let stop = grid.FindIndicesOf 'E' |> Seq.head
    grid[start] <- '.'
    grid[stop] <- '.'
    (grid, stop)

let traceDists =
    memo2 // memoization saves a whole 30 ms... worth it? idk
    <| fun (grid : Grid) stop ->
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

        loop Map.empty (Deque.singleton (0, stop))

let solvePart0 (input : string list) =
    let grid, stop = parse input
    let dists = traceDists grid stop

    seq {
        for s, sDist in Map.toSeq dists do
            for e in [ s.N.N ; s.N.E ; s.E.E ; s.S.E ] do
                if grid[e] = '.' && abs (sDist - Map.find e dists) - 2 >= 100 then
                    yield ()
    }
    |> Seq.length

let solvePart1 (input : string list) =
    let grid, stop = parse input
    let dists = traceDists grid stop |> Map.toSeq |> Array.ofSeq

    let rec loop acc i j =
        if i >= dists.Length then
            acc
        else if j >= dists.Length then
            loop acc (i + 1) (i + 2)
        else
            let s, sDist = dists[i]
            let e, eDist = dists[j]
            let dist = abs (s.X - e.X) + abs (s.Y - e.Y)
            let saved = abs (sDist - eDist) - dist
            let acc = if dist <= 20 && saved >= 100 then acc + 1 else acc
            loop acc i (j + 1)

    loop 0 0 1

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 20 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (0, 0)
                "input-real0.txt", Some (1323, 983905)
            }
