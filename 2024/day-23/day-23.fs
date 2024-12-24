module Day23

open FSharpx.Text
open FSharpx
open Utils
open BitGraph

let parse (input : string list) =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (Strings.split '-' >> Seq.assertPairs)
    // |> List.map (fun (x, y) -> if x < y then (x, y) else (y, x))
    |> (fun pairs -> pairs @ List.map swap pairs)
    |> BitGraph.ofSeq

let cliquesOf3 (graph : BitGraph<string>) =
    seq {
        for a in BitGraph.unlabeledNodes graph do
            for b in graph.AscendingEdges[a] do
                for c in BitSet.intersect graph.AscendingEdges[a] graph.AscendingEdges[b] do
                    yield [| a ; b ; c |] |> BitSet.ofSeq graph.Labels.Length
    }
    |> Array.ofSeq

let biggerCliques (graph : BitGraph<string>) (smallerCliques : BitSet array) : BitSet array =
    seq {
        for clique in smallerCliques do
            for candidate in Seq.max clique + 1 .. graph.Labels.Length - 1 do
                if
                    BitSet.isEmpty (BitSet.difference clique graph.Edges[candidate])
                then
                    yield BitSet.add candidate clique
    }
    |> Array.ofSeq

let solvePart0 (input : string list) =
    let graph = parse input

    graph
    |> cliquesOf3
    |> Seq.count (Seq.exists (fun i -> String.startsWith "t" graph.Labels[i]))

let solvePart1 (input : string list) =
    let graph = parse input

    let rec search i cliques =
        // printfn $"%d{i}-cliques: %d{Array.length cliques}"
        match cliques with
        | [||] -> failwith "not unique"
        | [| theOne |] -> theOne |> Seq.map (fun i -> graph.Labels[i]) |> String.concat ","
        | _ -> search (i + 1) (biggerCliques graph cliques)

    search 3 (cliquesOf3 graph)

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 23 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (7, "co,de,ka,ta")
                "input-ex1.txt", Some (14, "co,de,ja,ka,ta,za")
                "input-real0.txt", Some (1215, "bm,by,dv,ep,ia,ja,jb,ks,lv,ol,oy,uz,yt")
            }
