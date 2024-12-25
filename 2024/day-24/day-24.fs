module Day24

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open FSharpx.Text.Regex.Interpreted
open Utils

[<RequireQualifiedAccessAttribute>]
type Op =
    | And
    | Or
    | Xor

type Gate =
    | Const of bool
    | And of string * string
    | Or of string * string
    | Xor of string * string

let gateAnd wire0 wire1 = And (min wire0 wire1, max wire0 wire1)
let gateOr wire0 wire1 = Or (min wire0 wire1, max wire0 wire1)
let gateXor wire0 wire1 = Xor (min wire0 wire1, max wire0 wire1)

let op =
    function
    | Const _ -> failwith "gate has no op"
    | And _ -> Op.And
    | Or _ -> Op.Or
    | Xor _ -> Op.Xor

let in0 =
    function
    | Const _ -> failwith "gate has no inputs"
    | And (wire, _) -> wire
    | Or (wire, _) -> wire
    | Xor (wire, _) -> wire

let in1 =
    function
    | Const _ -> failwith "gate has no inputs"
    | And (_, wire) -> wire
    | Or (_, wire) -> wire
    | Xor (_, wire) -> wire

let parse (input : string list) =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (String.filter ((<>) ':') >> Strings.toWords >> Array.ofSeq)
    |> List.map (
        function
        | [| wire ; "0" |] -> (wire, Const false)
        | [| wire ; "1" |] -> (wire, Const true)
        | [| in0 ; "AND" ; in1 ; "->" ; out |] -> (out, gateAnd in0 in1)
        | [| in0 ; "OR" ; in1 ; "->" ; out |] -> (out, gateOr in0 in1)
        | [| in0 ; "XOR" ; in1 ; "->" ; out |] -> (out, gateXor in0 in1)
        | words -> failwith $"bad parse: %A{words}"
    )
    |> Map.ofSeq

let eval circuit =
    recMemo
    <| fun recur target ->
        match Map.find target circuit with
        | Const b -> b
        | And (w0, w1) -> recur w0 && recur w1
        | Or (w0, w1) -> recur w0 || recur w1
        | Xor (w0, w1) -> recur w0 <> recur w1

let solvePart0 (input : string list) =
    let circuit = parse input

    Map.keySet circuit
    |> Set.filter (String.startsWith "z")
    |> Seq.sortDescending
    |> Seq.map (eval circuit)
    |> Seq.fold (fun x y -> 2L * x + if y then 1L else 0L) 0L


let generateAdder (xs : string list) (ys : string list) (zs : string list) : Map<string, Gate> =
    let mutable counter = 0
    let gensym () =
        counter <- counter + 1
        $"i%d{counter - 1}"
    let rec loop acc carry xs ys zs =
        match xs, ys, zs with
        | [], [], [ zout ] -> (zout, carry) :: acc |> Map.ofList
        | x :: xs, y :: ys, z :: zs ->
            let c, i, j, k = gensym (), gensym (), gensym (), gensym ()

            let acc =
                (c, carry)
                :: (i, gateXor x y)
                :: (z, gateXor i c)
                :: (j, gateAnd i c)
                :: (k, gateAnd x y)
                :: acc

            loop acc (gateOr j k) xs ys zs
        | _ -> failwith "uneven circuit"

    let (x, xs), (y, ys), (z, zs) = List.uncons xs, List.uncons ys, List.uncons zs
    loop [ (z, gateXor x y) ] (gateAnd x y) xs ys zs

let oddnessOf circuit refCircuit xs ys zs without =
    let circuit =
        Option.fold
            (fun circuit key ->
                let gate = Map.find key circuit
                Map.remove key circuit |> Map.add $"{key}_disconnected" gate
            )
            circuit
            without

    let mutable refsForActs = xs @ ys @ zs |> Seq.map (fun x -> x, x) |> Map.ofSeq
    let mutable discrepancies = [||]

    let mutable queue =
        Map.toSeq circuit
        |> Seq.filter (
            function
            | _, Const _ -> false
            | _ -> true
        )
        |> Deque.ofSeq

    let refMatches (out, gate) =
        let ref0 = Map.tryFind (in0 gate) refsForActs
        let ref1 = Map.tryFind (in1 gate) refsForActs
        let refOut = Map.tryFind out refsForActs

        let optMatches =
            curry (
                function
                | None, _ -> true
                | Some x, y -> x = y
            )

        refCircuit
        |> Map.toSeq
        |> Seq.filter (
            function
            | o, g ->
                op g = op gate
                && optMatches ref0 (in0 g)
                && optMatches ref1 (in1 g)
                && optMatches refOut o
        )
        |> List.ofSeq

    let mutable stepsWithoutProgress = 0

    while not (Deque.isEmpty queue) && stepsWithoutProgress <= Deque.length queue do
        let (out, gate), queue' = Deque.uncons queue
        queue <- queue'

        match refMatches (out, gate) with
        | [ (refOut, refGate) ] ->
            seq {
                refOut, out
                in0 refGate, in0 gate
                in1 refGate, in1 gate
            }
            |> Seq.iter (fun (ref, act) -> refsForActs <- Map.add act ref refsForActs)

            stepsWithoutProgress <- 0
        | [] ->
            discrepancies <- Array.append discrepancies [| (out, gate) |]
            stepsWithoutProgress <- 0
        | _ ->
            queue <- Deque.conj (out, gate) queue
            stepsWithoutProgress <- stepsWithoutProgress + 1

    discrepancies.Length + Deque.length queue

let solvePart1 (input : string list) =
    let circuit = parse input

    let vars prefix =
        Map.keys circuit
        |> Seq.filter (String.startsWith prefix)
        |> Array.ofSeq
        |> Array.sort
        |> List.ofSeq

    let xs, ys, zs = (vars "x", vars "y", vars "z")
    let refCircuit = generateAdder xs ys zs

    let reduceOddness circuit =
        let others =
            Map.keySet circuit
            |> Set.filter (fun k -> not (String.startsWith "x" k || String.startsWith "y" k || String.startsWith "z" k))

        Seq.minBy (oddnessOf circuit refCircuit xs ys zs << Some) others
        |>! (fun odd ->
            printfn
                "%A: %d -> %d"
                odd
                (oddnessOf circuit refCircuit xs ys zs None)
                (oddnessOf circuit refCircuit xs ys zs (Some odd))
        )
        |> flip Map.remove circuit

    printfn ""

    circuit
    |> reduceOddness
    |> reduceOddness
    |> reduceOddness
    |> reduceOddness
    |> reduceOddness
    |> reduceOddness
    |> reduceOddness
    |> reduceOddness
    |> ignore

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 24 solvePart0 solvePart1
            <| seq {
                // "input-ex0.txt", None
                // "input-ex1.txt", None
                // "input-ex2.txt", None
                "input-real0.txt", None
            }
