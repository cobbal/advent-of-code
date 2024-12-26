module Day24

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

type Op =
    | And
    | Or
    | Xor

type Gate<'T, 'B> =
    | Const of string * 'B
    | Gate of Op * 'T * 'T

type Tree = | Tree of Gate<Tree, unit>

module Tree =
    let konst s = Tree (Const (s, ()))
    let gate op in0 in1 = Tree (Gate (op, in0, in1))

let parse (input : string list) =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (String.filter ((<>) ':') >> Strings.toWords >> Array.ofSeq)
    |> List.map (
        function
        | [| wire ; "0" |] -> (wire, Const (wire, false))
        | [| wire ; "1" |] -> (wire, Const (wire, true))
        | [| in0 ; "AND" ; in1 ; "->" ; out |] -> (out, Gate (And, in0, in1))
        | [| in0 ; "OR" ; in1 ; "->" ; out |] -> (out, Gate (Or, in0, in1))
        | [| in0 ; "XOR" ; in1 ; "->" ; out |] -> (out, Gate (Xor, in0, in1))
        | words -> failwith $"bad parse: %A{words}"
    )
    |> Map.ofSeq

let eval circuit =
    recMemo
    <| fun recur target ->
        match Map.find target circuit with
        | Const (_, b) -> b
        | Gate (And, w0, w1) -> recur w0 && recur w1
        | Gate (Or, w0, w1) -> recur w0 || recur w1
        | Gate (Xor, w0, w1) -> recur w0 <> recur w1

let solvePart0 (input : string list) =
    let circuit = parse input

    Map.keySet circuit
    |> Set.filter (String.startsWith "z")
    |> Seq.sortDescending
    |> Seq.map (eval circuit)
    |> Seq.fold (fun x y -> 2L * x + if y then 1L else 0L) 0L

let solvePart1 (input : string list) =
    if List.length input < 50 then
        "N/A"
    else

    let circuit = parse input

    let vars prefix =
        Map.keys circuit
        |> Seq.filter (String.startsWith prefix)
        |> Array.ofSeq
        |> Array.sort
        |> List.ofSeq

    let xs, ys, zs = (vars "x", vars "y", vars "z")

    let xor0s =
        Seq.map2
            (fun x y ->
                circuit
                |> Map.toSeq
                |> Seq.choose (
                    function
                    | out, Gate (Xor, in0, in1) when in0 = x && in1 = y || in0 = y && in1 = x -> Some out
                    | _ -> None
                )
                |> Seq.assertOne
            )
            xs
            ys
        |> Array.ofSeq

    let xor1s =
        Seq.map2
            (fun xy z ->
                circuit
                |> Map.toSeq
                |> Seq.choose (
                    function
                    | out, Gate (Xor, in0, in1) ->
                        let otherGate () =
                            match Map.find in0 circuit, Map.find in1 circuit with
                            | Gate (Or, _, _), _ -> in1
                            | _, Gate (Or, _, _) -> in0
                            | _ -> failwith "unexpected gate"

                        match out = z, in0 = xy || in1 = xy with
                        | true, true -> Some ([], out)
                        | false, true -> Some ([ (out, z) ], out)
                        | true, false -> Some ([ (xy, otherGate ()) ], out)
                        | false, false -> None
                    | _ -> None
                )
            )
            (Array.tail xor0s)
            (zs |> Array.ofSeq |> Array.tail)
        |> Seq.map Seq.assertOne

    xor1s
    |> Seq.map fst
    |> Seq.concat
    |> Seq.collect (fun (x, y) -> [ x ; y ])
    |> Seq.sort
    |> String.concat ","

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 24 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (4, "N/A")
                "input-ex1.txt", Some (2024, "N/A")
                "input-real0.txt", Some (56620966442854L, "chv,jpj,kgj,rts,vvw,z07,z12,z26")
            }
