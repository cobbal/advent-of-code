module Day04

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) : char list list =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (List.ofArray << String.toCharArray)

let rec countString : char list -> int64 =
    function
    | [] -> 0
    | 'X' :: 'M' :: 'A' :: 'S' :: xs -> 1L + countString xs
    | _ :: xs -> countString xs

let rec map2default (f : 'a -> 'a -> 'a) : 'a list -> 'a list -> 'a list =
    curry
    <| function
        | [], [] -> []
        | xs, [] -> xs
        | [], ys -> ys
        | (x :: xs), (y :: ys) -> f x y :: map2default f xs ys

let rec diagonalize : 'a list list -> 'a list list =
    function
    | [] -> []
    | [] :: _ -> []
    | (x :: xs) :: xss -> [ x ] :: map2default List.append (List.map List.singleton xs) (diagonalize xss)

let solvePart0 (input : string list) : int64 =
    let grid = parse input

    let dirLines =
        grid // 🡒
        @ List.map List.rev grid // 🡐
        @ List.transpose grid // 🡓
        @ List.transpose (List.rev grid) // 🡑
        @ diagonalize grid // 🡗
        @ List.map List.rev (diagonalize grid) // 🡕
        @ diagonalize (List.rev grid) // 🡔
        @ List.map List.rev (diagonalize (List.rev grid)) // 🡖

    List.map countString dirLines |> List.sum

let isXMas (arr : char array array) (x : int, y : int) : bool =
    let isMS a b =
        (a, b) = ('M', 'S') || (a, b) = ('S', 'M')

    let c0, d0 = arr[y][x], arr[y][x + 2]
    let mid = arr[y + 1][x + 1]
    let d1, c1 = arr[y + 2][x], arr[y + 2][x + 2]
    mid = 'A' && isMS c0 c1 && isMS d0 d1

let solvePart1 (input : string list) : int64 =
    let grid = parse input |> List.map Array.ofList |> Array.ofList
    let height, width = Array.length grid, Array.length grid[0]

    Seq.allPairs { 0 .. width - 3 } { 0 .. height - 3 } |> Seq.count (isXMas grid)

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 04 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (18, 9)
                "input-real0.txt", Some (2654, 1990)
            }
