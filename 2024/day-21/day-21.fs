module Day21

open FSharpx.Collections
open Utils
open Grid

let kps = [| Grid [ "789" ; "456" ; "123" ; " 0A" ] ; Grid [ " ^A" ; "<v>" ] |]

let moveToAndPress kpi (pos : XY) (dest : XY) =
    let kp = kps[kpi]
    let h = Array.replicate (abs (dest.X - pos.X)) (if pos.X < dest.X then '>' else '<')
    let v = Array.replicate (abs (dest.Y - pos.Y)) (if pos.Y < dest.Y then 'v' else '^')

    if kp[XY (dest.X, pos.Y)] = ' ' then Array.append v h
    else if kp[XY (pos.X, dest.Y)] = ' ' then Array.append h v
    else if pos.X < dest.X then Array.append v h
    else Array.append h v

let activate kpi c0 c1 =
    let start = kps[kpi].FindIndicesOf c0 |> Seq.head
    let dest = kps[kpi].FindIndicesOf c1 |> Seq.head

    Array.append (moveToAndPress kpi start dest) [| 'A' |]

let pairsOf (start : char) (str : char seq) : (char * char) seq = str |> Seq.cons start |> Seq.pairwise

let activateAll kpi : MultiSet<char * char> -> MultiSet<char * char> =
    MultiSet.collect (fun (start, stop) -> activate kpi start stop |> pairsOf 'A')

let chain intermediates : MultiSet<char * char> -> MultiSet<char * char> =
    activateAll 0 >> repeatedly intermediates (activateAll 1)

let score intermediates (output : string) =
    pairsOf 'A' output
    |> MultiSet.of1Seq
    |> chain intermediates
    |> MultiSet.count
    |> (*) (int64 (String.filter ((<>) 'A') output))

let solvePart0 (input : string list) = List.sumBy (score 2) input
let solvePart1 (input : string list) = List.sumBy (score 25) input

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 21 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (126384L, 154115708116294L)
                "input-real0.txt", Some (164960L, 205620604017764L)
            }
