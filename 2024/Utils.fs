module Utils

open FSharpx.Collections

type 'T ``[]`` with
    member xs.GetOp (n : int) : 'T option =
        if 0 <= n && n < xs.Length then Some (xs[n]) else None

module Seq =
    let assertPairs (xs : 'T seq) : 'T * 'T =
        let arr = Seq.toArray xs in

        if arr.Length = 2 then
            (arr[0], arr[1])
        else
            failwith $"bad length, expected 2 got %d{arr.Length}"

    let count (predicate : 'a -> bool) : 'a seq -> int =
        Seq.fold (fun acc x -> acc + if predicate x then 1 else 0) 0

module List =
    let count (predicate : 'a -> bool) : 'a list -> int =
        List.fold (fun acc x -> acc + if predicate x then 1 else 0) 0

type MultiMap<'K, 'V> when 'K : comparison and 'V : comparison = Map<'K, Set<'V>>

module MultiMap =
    let ofSeq (s : ('K * 'V) seq) : MultiMap<'K, 'V> =
        Seq.fold
            (fun map (key, value) ->
                Map.change key (fun set -> defaultArg set Set.empty |> (Set.add value) |> Some) map
            )
            Map.empty
            s

    let ofList (l : ('K * 'V) list) : MultiMap<'K, 'V> = ofSeq l

    let toSeq (m : MultiMap<'K, 'V>) : ('K * 'V) seq =
        m |> Seq.collect (fun entry -> Seq.map (fun v -> (entry.Key, v)) entry.Value)

    let toList (m : MultiMap<'K, 'V>) : ('K * 'V) list = toSeq m |> List.ofSeq

    let get (map : MultiMap<'K, 'V>) (key : 'K) : Set<'V> = Map.findOrDefault key Set.empty map

    let inverse (m : MultiMap<'K, 'V>) : MultiMap<'V, 'K> =
        m |> toSeq |> Seq.map (fun (k, v) -> (v, k)) |> ofSeq


let timer (f : unit -> 'a) : 'a =
    let watch = System.Diagnostics.Stopwatch.StartNew ()
    let result = f ()
    watch.Stop ()
    printfn $"Time taken: %d{watch.ElapsedMilliseconds}ms"
    result
