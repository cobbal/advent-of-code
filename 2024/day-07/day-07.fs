module Day07

open FSharpx
open FSharpx.Text
open Utils

let parse (input : string list) : (int64 * int64 list) list =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (String.filter (not << (=) ':'))
    |> List.map (Strings.split ' ' >> List.ofArray >> List.map int64)
    |> List.map (fun l -> (List.head l, List.tail l |> List.rev))

let unsuffix whole =
    function
    | _ when whole < 0L -> None
    | 0L -> if whole % 10L = 0L then Some (whole / 10L) else None
    | suffix ->
        let rec loop whole =
            function
            | 0L -> Some whole
            | suffix when suffix % 10L = whole % 10L -> loop (whole / 10L) (suffix / 10L)
            | _ -> None

        loop whole suffix

let solvePart0 (input : string list) : int64 =
    let rec isGood result =
        function
        | [] -> false
        | [ x ] -> result = x
        | x :: xs -> (result % x = 0L && isGood (result / x) xs) || isGood (result - x) xs

    parse input |> List.filter (uncurry isGood) |> List.map fst |> List.sum

let solvePart1 (input : string list) : int64 =
    let rec isGood result =
        function
        | [] -> false
        | [ x ] -> result = x
        | x :: xs ->
            (result % x = 0L && isGood (result / x) xs)
            || (unsuffix result x |> Option.map (flip isGood xs) |> flip defaultArg false)
            || isGood (result - x) xs

    parse input |> List.filter (uncurry isGood) |> List.map fst |> List.sum

let day07 =
    Day.day 07 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (3749, 11387))
    |> Day.addInput "input-real0.txt" (Some (28730327770375L, 424977609625985L))
