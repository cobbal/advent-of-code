namespace Utils

open FSharpx
open FSharpx.Collections
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.LanguagePrimitives

module Array =
    let tryGet (n : int) (xs : 'T array) : 'T option =
        if 0 <= n && n < xs.Length then Some (xs[n]) else None

module Seq =
    let assertPairs (xs : 'T seq) : 'T * 'T =
        let arr = Seq.toArray xs in

        if arr.Length = 2 then
            (arr[0], arr[1])
        else
            failwith $"bad length, expected 2 got %d{arr.Length}"

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

    let memo (f : 'K -> 'v) : 'K -> 'v =
        let mutable table = Map.empty
        fun k ->
            match Map.tryFind k table with
            | Some v -> v
            | None -> f k |>! (fun v -> table <- Map.add k v table)
