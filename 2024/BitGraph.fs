module BitGraph

open System.Collections
open System.Numerics
open Utils
open FSharpx
open FSharpx.Collections

[<Struct>]
type BitArray(count : int, limbs : uint64 array) =
    member this.Length = count
    member this.Limbs = limbs

    new(count) = BitArray (count, Array.zeroCreate ((count + 63) / 64))

    member this.Item
        with get i = limbs[i / 64] &&& (1uL <<< i % 64) <> 0uL
        and set i newValue =
            let mask = 1uL <<< i % 64

            limbs[i / 64] <-
                if newValue then
                    limbs[i / 64] ||| mask
                else
                    limbs[i / 64] &&& ~~~mask

    member this.Not () =
        Array.map (~~~) this.Limbs |> curry BitArray count

    member this.LimbBinOp (op : uint64 -> uint64 -> uint64) (other : BitArray) =
        Array.map2 op this.Limbs other.Limbs |> curry BitArray count

    member this.Or other = this.LimbBinOp (|||) other
    member this.And other = this.LimbBinOp (&&&) other

    member this.Difference other =
        this.LimbBinOp (fun x y -> x &&& ~~~y) other

    member this.HasAnySet () = Array.exists ((<>) 0uL) this.Limbs

    member this.Set i newValue =
        let mutable result = BitArray (count, Array.copy limbs)
        result[i] <- newValue
        result

[<Struct>]
type BitSet(storage : BitArray) =
    member private this.Storage = storage

    override this.ToString () =
        let storage = this.Storage

        Array.init storage.Length (fun i -> if storage[i] then '#' else '.')
        |> System.String

    static member ofSeq (universeSize : int) (sequence : int seq) : BitSet =
        let mutable storage = BitArray universeSize

        for element in sequence do
            storage[element] <- true

        BitSet storage

    static member toSeq (bitset : BitSet) =
        seq {
            for limbIndex in 0 .. bitset.Storage.Limbs.Length - 1 do
                let mutable workingLimb = bitset.Storage.Limbs[limbIndex]

                while workingLimb <> 0uL do
                    let bit = BitOperations.TrailingZeroCount workingLimb
                    workingLimb <- workingLimb &&& ~~~(1uL <<< bit)
                    yield 64 * limbIndex + bit
        }

    static member contains element (bitset : BitSet) = bitset.Storage[element]

    static member difference (set0 : BitSet) (set1 : BitSet) =
        set0.Storage.Difference set1.Storage |> BitSet

    static member differenceIsEmpty (set0 : BitSet) (set1 : BitSet) =
        Seq.forall2 (fun x y -> x &&& ~~~ y = 0uL) set0.Storage.Limbs set1.Storage.Limbs

    static member intersect (set0 : BitSet) (set1 : BitSet) = set0.Storage.And set1.Storage |> BitSet

    static member add element (bitset : BitSet) =
        bitset.Storage.Set element true |> BitSet

    static member isEmpty (bitset : BitSet) = not (bitset.Storage.HasAnySet ())

    interface Generic.IEnumerable<int> with
        member this.GetEnumerator () : Generic.IEnumerator<int> = (BitSet.toSeq this).GetEnumerator ()
        member this.GetEnumerator () : IEnumerator = (BitSet.toSeq this).GetEnumerator ()

type BitGraph<'T when 'T : comparison> =
    {
        Labels : 'T array
        LabelMap : Map<'T, int>
        Edges : BitSet array
        AscendingEdges : BitSet array
    }

module BitGraph =
    let ofSeq (pairs : ('T * 'T) seq) =
        let pairs = Seq.cache pairs

        let labels =
            Seq.collect (fun (u, v) -> [ u ; v ]) pairs
            |> Seq.distinct
            |> Seq.sort
            |> Array.ofSeq

        let labelMap = Array.indexed labels |> Array.map swap |> Map.ofArray

        let multimap =
            pairs
            |> Seq.map (fun (x, y) -> Map.find x labelMap, Map.find y labelMap)
            |> MultiMap.ofSeq

        let edges =
            Array.init labels.Length (fun i -> BitSet.ofSeq labels.Length (Map.findOrDefault i Set.empty multimap))

        let ascendingEdges =
            Array.init
                labels.Length
                (fun i ->
                    BitSet.ofSeq labels.Length (Map.findOrDefault i Set.empty multimap |> Seq.filter (fun x -> i < x))
                )

        {
            Labels = labels
            LabelMap = labelMap
            Edges = edges
            AscendingEdges = ascendingEdges
        }

    let unlabeledNodes (g : BitGraph<'T>) = { 0 .. g.Labels.Length - 1 }
