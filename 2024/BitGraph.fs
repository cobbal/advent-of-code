module BitGraph

open System.Collections
open Utils
open FSharpx
open FSharpx.Collections

type BitSet(storage : BitArray) =
    member private this.Storage = storage
    override this.ToString () =
        Array.init storage.Length (fun i -> if storage[i] then '#' else '.')
        |> System.String

    static member ofSeq (universeSize : int) (sequence : int seq) : BitSet =
        let storage = BitArray universeSize

        for element in sequence do
            storage[element] <- true

        BitSet storage

    static member toSeq (bitset : BitSet) =
        seq {
            // TODO: faster
            for i in 0 .. bitset.Storage.Length - 1 do
                if bitset.Storage[i] then
                    yield i
        }

    static member contains element (bitset : BitSet) = bitset.Storage[element]

    static member difference (set0 : BitSet) (set1 : BitSet) =
        assert (set0.Storage.Length = set1.Storage.Length)
        ((BitArray set1.Storage).Not ()).And set0.Storage |> BitSet

    static member intersect (set0 : BitSet) (set1 : BitSet) =
        (BitArray set0.Storage).And set1.Storage |> BitSet

    static member add element (bitset : BitSet) =
        let result = BitArray bitset.Storage
        result[element] <- true
        BitSet result

    static member isEmpty (bitset : BitSet) =
        not (bitset.Storage.HasAnySet ())

    interface Generic.IEnumerable<int> with
        member this.GetEnumerator () : Generic.IEnumerator<int> = (BitSet.toSeq this).GetEnumerator ()
        member this.GetEnumerator () : IEnumerator = (BitSet.toSeq this).GetEnumerator ()

type BitGraph<'T when 'T : comparison> =
    {
        Labels : 'T array
        LabelMap : Map<'T, int>
        Edges : BitSet array
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
        let multimap = MultiMap.ofSeq pairs

        {
            Labels = labels
            LabelMap = labelMap
            Edges =
                Array.init
                    labels.Length
                    (fun i ->
                        BitSet.ofSeq
                            labels.Length
                            (Map.findOrDefault labels[i] Set.empty multimap
                             |> Set.map (flip Map.find labelMap))
                    )
        }

    let unlabeledNodes (g : BitGraph<'T>) = { 0 .. g.Labels.Length - 1 }
