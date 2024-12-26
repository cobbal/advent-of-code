namespace Utils

open FSharpx
open FSharpx.Collections
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.LanguagePrimitives

module Array =
    let tryGet (n : int) (xs : 'T array) : 'T option =
        if 0 <= n && n < xs.Length then Some (xs[n]) else None

    let fourwise (a : 'T array) =
        Array.init (max 0 (a.Length - 3)) (fun i -> a[i], a[i + 1], a[i + 2], a[i + 3])

    let fivewise (a : 'T array) =
        Array.init (max 0 (a.Length - 4)) (fun i -> a[i], a[i + 1], a[i + 2], a[i + 3], a[i + 4])

    let tryTake n (xs : 'T array) =
        Array.take (min n xs.Length) xs

module Seq =
    let assertPairs (xs : 'T seq) : 'T * 'T =
        let arr = Seq.toArray xs in

        if arr.Length = 2 then
            (arr[0], arr[1])
        else
            failwith $"bad length, expected 2 got %d{arr.Length}"

    let assertOne (xs : 'T seq) : 'T =
        let arr = Seq.toArray xs in

        if arr.Length = 1 then
            arr[0]
        else
            failwith $"bad length, expected 1 got %d{arr.Length}"


    let count (predicate : 'a -> bool) : 'a seq -> int64 =
        Seq.fold (fun acc x -> acc + if predicate x then 1L else 0L) 0L

    let choosei (f : int -> 'a -> 'b option) : 'a seq -> 'b seq = Seq.indexed >> Seq.choose (uncurry f)

    let collecti (f : int -> 'a -> 'b seq) : 'a seq -> 'b seq = Seq.indexed >> Seq.collect (uncurry f)

    let ofOption =
        function
        | Some x -> seq { yield x }
        | None -> seq { }

    let tryMin (s : 'a seq) : 'a option =
        if Seq.isEmpty s then None else Some (Seq.min s)

module List =
    let count (predicate : 'a -> bool) : 'a list -> int64 =
        List.fold (fun acc x -> acc + if predicate x then 1L else 0L) 0L

    let uncons =
        function
        | [] -> failwith "uncons of nil"
        | x :: xs -> (x, xs)

    let rec revAppend l1 l2 =
        match l1 with
        | [] -> l2
        | x :: xs -> revAppend xs (x :: l2)

module Map =
    let singleton k v = Map.add k v Map.empty
    let invert<'K, 'V when 'K: comparison and 'V: comparison> (m : Map<'K, 'V>): Map<'V, 'K> =
        let result = m |> Map.toSeq |> Seq.map swap |> Map.ofSeq
        assert (Map.count m = Map.count result)
        result

module Option =
    let unzip =
        function
        | None -> (None, None)
        | Some (a, b) -> (Some a, Some b)

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

type MultiSet<'T when 'T : comparison> = Map<'T, int64>

module MultiSet =
    let of1Seq<'T when 'T : comparison> : 'T seq -> MultiSet<'T> =
        Seq.fold (fun m k -> Map.change k (fun v -> defaultArg v 0L + 1L |> Some) m) Map.empty

    let ofSeq<'T when 'T : comparison> : ('T * int64) seq -> MultiSet<'T> =
        Seq.fold (fun m (k, count) -> Map.change k (fun v -> defaultArg v 0L + count |> Some) m) Map.empty

    let collect (mapping : 'T -> 'U seq) (set : MultiSet<'T>) : MultiSet<'U> =
        seq {
            for kv in set do
                for k in mapping kv.Key do
                    yield (k, kv.Value)
        }
        |> ofSeq

    let bind<'T, 'U when 'T : comparison and 'U : comparison>
        (mapping : 'T -> MultiSet<'U>)
        (set : MultiSet<'T>)
        : MultiSet<'U>
        =
        seq {
            for KeyValue (t, outerCount) in set do
                for KeyValue (u, innerCount) in mapping t do
                    yield (u, outerCount * innerCount)
        }
        |> ofSeq

    let count (set : MultiSet<'a>) =
        Map.fold (fun acc _ count -> acc + count) 0L set

module Strings =
    let int64sIn : string -> int64 seq =
        let rx = Regex @"-?\d+"
        rx.Matches >> Seq.map (_.Value >> int64)

[<AutoOpen>]
module Values =
    let timerf (reporter : int64 -> unit) (f : unit -> 'a) : 'a =
        let watch = System.Diagnostics.Stopwatch.StartNew ()
        let result = f ()
        watch.Stop ()
        reporter watch.ElapsedMilliseconds
        result

    let timer (name : string) : (unit -> 'a) -> 'a = timerf (printfn "%s: %dms" name)

    // https://stackoverflow.com/a/35848799/73681
    /// Euclidean remainder, the proper modulo operation
    let inline (%!) a b = (a % b + b) % b

    // Adapted from https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Pseudocode
    let inline extended_gcd (a : ^T) (b : ^T) =
        let zero : ^T = GenericZero
        let one : ^T = GenericOne

        let rec loop (old_r, r) (old_s, s) =
            if r = zero then
                (old_s, (if b = zero then zero else (old_r - old_s * a) / b), old_r)
            else
                // TODO: div instead of / here?
                let quotient = old_r / r
                loop (r, old_r - quotient * r) (s, old_s - quotient * s)

        loop (a, b) (one, zero)

    let inline chineseRemainderTheorem (s : (^T * ^T) seq) : ^T =
        let remainders, moduli = Array.unzip (Array.ofSeq s)
        let totalModulus = Array.fold (*) GenericOne moduli

        let bezouts =
            Array.map (fun n -> let m, _, _ = extended_gcd (totalModulus / n) n in m) moduli

        Array.map3 (fun a m n -> a * m * (totalModulus / n)) remainders bezouts moduli
        |> Array.sum
        |> flip (%!) totalModulus

    let gcd x y =
        let rec gcd' a =
            function
            | 0 -> a
            | b -> gcd' b (a %! b)

        gcd' (abs x) (abs y)

    let recMemo (f : ('K -> 'v) -> 'K -> 'v) : 'K -> 'v =
        let mutable table = Map.empty

        let rec memoized k =
            match Map.tryFind k table with
            | Some v -> v
            | None -> f memoized k |>! (fun v -> table <- Map.add k v table)

        memoized

    let memo (f : 'K -> 'v) : 'K -> 'v = recMemo (konst f)

    let memo2 (f : 'K0 -> 'K1 -> 'v) : 'K0 -> 'K1 -> 'v = curry (memo (uncurry f))

    let rec repeatedly n (f : 'a -> 'a) (x : 'a) =
        match n with
        | 0 -> x
        | _ -> repeatedly (n - 1) f (f x)
