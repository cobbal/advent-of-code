module Day01

open Utils
open FSharpx
open FSharpx.Text

let parse (input : string list) : int64 list * int64 list =
    let l =
        List.map (Strings.split ' ' >> Seq.choose FSharpOption.ParseInt64 >> Seq.assertPairs) input

    (List.map fst l, List.map snd l)

let solvePart0 (input : string list) : int64 =
    let left, right = parse input
    let zips = List.zip (List.sort left) (List.sort right)
    List.sum (List.map (fun (x, y) -> abs (x - y)) zips)

let solvePart1 (input : string list) : int64 =
    let left, right = parse input

    let occurrences =
        List.fold (fun acc k -> Map.change k (fun v -> Some (defaultArg v 0L + 1L)) acc) Map.empty right

    let scores = List.map (fun x -> x * defaultArg (Map.tryFind x occurrences) 0L) left
    List.sum scores

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 01 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (11, 31)
                "input-real0.txt", Some (765748, 27732508)
            }
