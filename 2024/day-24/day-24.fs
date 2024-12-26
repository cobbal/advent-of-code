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

type Tree =
    | Input of string
    | Gate of Op * Tree * Tree

let opStr = function Op.And -> "&" | Op.Or -> "|" | Op.Xor -> "^"

let rec treeStr t =
    match t with
    | Input s -> s
    | Gate (op, t0, t1) -> $"({treeStr t0} {opStr op} {treeStr t1})"

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

    let startingGates =
        seq {
            for x in xs do
                yield (x, Const false)

            for y in ys do
                yield (y, Const false)
        }
        |> List.ofSeq

    let (x, xs), (y, ys), (z, zs) = List.uncons xs, List.uncons ys, List.uncons zs
    loop ((z, gateXor x y) :: startingGates) (gateAnd x y) xs ys zs

module Map =
    let swapKeys k0 k1 map =
        map |> Map.add k0 (Map.find k0 map) |> Map.add k0 (Map.find k1 map)

let rec treeCount =
    function
    | Input _ -> 1
    | Gate (_, t0, t1) -> 1 + treeCount t0 + treeCount t1

let canon : Tree -> Tree =
    recMemo
    <| fun recur t ->
        match t with
        | Input _ -> t
        | Gate (op, t0, t1) ->
            let t0, t1 = recur t0, recur t1
            Gate (op, min t0 t1, max t0 t1)

let rectify circuit : string -> Tree =
    recMemo
    <| fun recur wire ->
        match Map.tryFind wire circuit with
        | None
        | Some (Const _) -> Input wire
        | Some gate -> Gate (op gate, recur (in0 gate), recur (in1 gate))
        |> canon

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

    let refTreeMap =
        Seq.map (fun w -> (w, rectify refCircuit w)) (Map.keys refCircuit) |> Map.ofSeq

    let refTreeCoMap = Map.invert refTreeMap

    printfn ""

    let findSwap circuit =
        let treeMap =
            Seq.map (fun w -> (w, rectify circuit w)) (Map.keys circuit) |> Map.ofSeq

        let treeCoMap = Map.invert treeMap

        let wrongOut =
            Map.values refTreeMap
            |> Seq.sortBy (fun t -> treeCount t, t)
            |> Seq.filter (not << flip Map.containsKey treeCoMap)
            |> Seq.cache
            |>! (Seq.map treeCount >> printfn "%A")
            |> Seq.head
            |> flip Map.find refTreeCoMap

        let missedCircuit = Map.find wrongOut refCircuit
        let o = op missedCircuit
        let tree0 = in0 missedCircuit |> flip Map.find refTreeMap
        let tree1 = in1 missedCircuit |> flip Map.find refTreeMap
        let i0 = Map.find tree0 treeCoMap
        let i1 = Map.find tree1 treeCoMap

        circuit
        |> Seq.choose (
            function
            | KeyValue(_, Const _) -> None
            | KeyValue(_, g) when op g <> o -> None
            | KeyValue(k, g) when i0 = in0 g -> Some (k, i0, i1, in1 g)
            | KeyValue(k, g) when i1 = in0 g -> Some (k, i1, i0, in1 g)
            | KeyValue(k, g) when i0 = in1 g -> Some (k, i0, i1, in0 g)
            | KeyValue(k, g) when i1 = in1 g -> Some (k, i1, i0, in0 g)
            | _ -> None
        )
        |> Array.ofSeq
        |> function
            | [| (k, good, x, y) |] ->
                let describeWire = treeStr << canon << flip Map.find treeMap
                printfn "%s good: %s <-> %s" good x y
                printfn "%A" ^ treeStr ^ canon (Map.find x treeMap)
                printfn "%A" ^ treeStr ^ canon (Map.find y treeMap)
                printfn "%s" ^ treeStr ^ canon ^ Gate (o, Map.find good treeMap, Map.find x treeMap)
                printfn "%s" ^ treeStr ^ Map.find wrongOut refTreeMap
                printfn "%s" ^ treeStr ^ canon ^ rectify (Map.swapKeys x y circuit) k
                (x, y)
            | _ -> failwith "not one"

    circuit
    |> (fun c -> uncurry Map.swapKeys (findSwap c) c)
    |> (fun c -> uncurry Map.swapKeys (findSwap c) c)
    |> (fun c -> uncurry Map.swapKeys (findSwap c) c)
    |> (fun c -> uncurry Map.swapKeys (findSwap c) c)
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
