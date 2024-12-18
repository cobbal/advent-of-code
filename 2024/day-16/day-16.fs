module Day16

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils
open Grid

let parse (input : string list) : Grid =
    input |> List.filter (not << Strings.isNullOrEmpty) |> Grid

type PriorityQueue<'K, 'V> when 'K : comparison and 'V : comparison =
    {
        Priorities : Heap<'V * 'K>
        Lookup : Map<'K, 'V>
        Removed : Set<'V * 'K>
    }

module PriorityQueue =
    let ofSeq<'K, 'V when 'K : comparison and 'V : comparison> (s : ('V * 'K) seq) =
        {
            Priorities = s |> Heap.ofSeq false
            Lookup = s |> Seq.map swap |> Map.ofSeq
            Removed = Set.empty
        }

    let insertIfLower<'K, 'V when 'K : comparison and 'V : comparison> (k : 'K) (v : 'V) (q : PriorityQueue<'K, 'V>) =
        match Map.tryFind k q.Lookup with
        | Some v0 when v0 <= v -> q
        | Some v0 ->
            {
                Priorities = Heap.insert (v, k) q.Priorities
                Lookup = Map.add k v q.Lookup
                Removed = Set.add (v0, k) q.Removed
            }
        | None ->
            { q with
                Priorities = Heap.insert (v, k) q.Priorities
                Lookup = Map.add k v q.Lookup
            }

    let rec uncons q =
        let vk, prio = Heap.uncons q.Priorities

        if Set.contains vk q.Removed then
            uncons
                { q with
                    Priorities = prio
                    Removed = Set.remove vk q.Removed
                }
        else
            (vk, { q with Priorities = prio })


let solve =
    memo
    <| fun (grid : Grid) ->
        let start = (grid.FindIndicesOf 'S' |> Seq.head, E)
        let stop = grid.FindIndicesOf 'E' |> Seq.head

        let rec loop (heap : PriorityQueue<XY * Dir, int>) =
            let (dist, (pos, dir)), heap = PriorityQueue.uncons heap

            if (pos, dir) = start then
                (dist, heap.Lookup)
            else

            let guardInsert (dist, (pos, dir)) heap =
                if grid[pos] = '#' then
                    heap
                else
                    PriorityQueue.insertIfLower (pos, dir) dist heap

            loop (
                heap
                |> guardInsert (1 + dist, (pos[Dir.rev dir], dir))
                |> guardInsert (1000 + dist, (pos, Dir.clockwise dir))
                |> guardInsert (1000 + dist, (pos, Dir.counterClockwise dir))
            )

        loop (PriorityQueue.ofSeq [ (0, (stop, N)) ; (0, (stop, E)) ; (0, (stop, S)) ; (0, (stop, E)) ])

let solvePart0 (input : string list) : int64 = parse input |> solve |> fst |> int64

let solvePart1 (input : string list) : int64 =
    let grid = parse input
    let dist, map = solve grid

    let rec findSeats (pos, dir) dist =
        Set.monoid {
            if Map.tryFind (pos, dir) map = Some dist then
                yield Set.singleton pos
                yield findSeats (pos[dir], dir) (dist - 1)
                yield findSeats (pos, Dir.clockwise dir) (dist - 1000)
                yield findSeats (pos, Dir.counterClockwise dir) (dist - 1000)
        }

    let start = (grid.FindIndicesOf 'S' |> Seq.head, E)
    findSeats start dist |> Set.count |> int64

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 16 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (7036, 45)
                "input-ex1.txt", Some (11048, 64)
                "input-real0.txt", Some (95444, 513)
            }
