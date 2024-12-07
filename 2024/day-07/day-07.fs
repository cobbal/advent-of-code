module Day07

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) : (int64 * int64 list) list =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (String.filter (not << (=) ':'))
    |> List.map (Strings.split ' ' >> List.ofArray >> List.choose FSharpOption.ParseInt64)
    |> List.map (fun l -> (List.head l, List.tail l |> List.rev))

type Op =
    | Times
    | Plus

let rec checkGood =
    function
    | _, [] -> None
    | result, [ x ] -> if result = x then Some [] else None
    | result, x :: xs ->
        Option.orElse
            (if x <> 0L && result % x = 0L then
                 checkGood (result / x, xs) |> Option.map (List.cons Times)
             else
                 None)
            (checkGood (result - x, xs) |> Option.map (List.cons Plus))

let dumb (result, list) =
    let rec help =
        function
        | [] -> failwith "bad"
        | [ x ] -> Set.singleton x
        | x :: xs ->
            let sub = help xs
            Set.union (Set.map ((+) x) sub) (Set.map ((*) x) sub)

    Set.contains result (help list)

let rec isGood =
    function
    | _, [] -> false
    | result, [ x ] -> result = x
    | result, x :: xs ->
        (if x <> 0L && result % x = 0L then
             isGood (result / x, xs)
         else
             false)
        || isGood (result - x, xs)

let unsuffix: int64 * int64 -> int64 option =
    function
        | whole, _ when whole < 0 -> None
        | whole, suffix when whole = suffix -> Some 0L
        | whole, suffix ->
        let ws = $"%d{whole}_"
        let ss = $"%d{suffix}_"

        if ws.Contains ss then
            let x = FSharpOption.ParseInt64 (ws.Substring (0, ws.Length - ss.Length))
            if x.IsNone then printfn $"{ws} {ss}"
            Some x.Value
        else
            None

let rec isGood1 =
    function
    | _, [] -> false
    | result, [ x ] -> result = x
    | result, x :: xs ->
        (if x <> 0L && result % x = 0L then
             isGood1 (result / x, xs)
         else
             false)
        || (
            match unsuffix (result, x) with
            | Some z -> isGood1 (z, xs)
            | None -> false
        )
        || isGood1 (result - x, xs)


let solvePart0 (input : string list) : int =
    parse input
    |> List.filter dumb
    // |> List.map (fun (y, z) ->
    //     let ops = List.rev (checkGood (y, z)).Value
    //     let x, xs = List.uncons  (List.rev z)
    //     let delta = function Times -> (*) | Plus -> (+)
    //     let opStr = function Times -> "*" | Plus -> "+"
    //     let check = List.fold2 (flip delta) x ops xs
    //
    //     printf $"%A{y} = "
    //     List.map2 (fun op y -> $"{opStr op} %A{y}") ops xs
    //     |> List.cons (string x)
    //     |> Strings.joinWords
    //     |> printfn "%s"
    //     assert (check = y)
    //     (y, z)
    // )
    |> List.map fst
    |> List.sum
    |> (fun x ->
        printfn $"%A{x}"
        int x
    )

let solvePart1 (input : string list) : int =
    parse input
    |> List.filter isGood1
    |> List.map fst
    |> List.sum
    |> (fun x ->
        printfn $"%A{x}"
        int x
    )


let day07 =
    Day.day 07 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" None
    |> Day.addInput "input-real0.txt" None
