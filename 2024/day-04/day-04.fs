module Day04

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils

let parse (input : string list) : char list list =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (List.ofArray << String.toCharArray)

let rec countString : char list -> int =
    function
    | [] -> 0
    | 'X' :: 'M' :: 'A' :: 'S' :: xs -> 1 + countString xs
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

let solvePart0 (input : string list) : int =
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

let rec heads2d (l : 'a list list) : 'a list list list =
    let rec horizontalHeads (l : 'a list list) : 'a list list list =
        if List.forall List.isEmpty l then
            []
        else
            l :: horizontalHeads (List.map List.tail l)

    match l with
    | [] -> []
    | _ :: xss as l -> horizontalHeads l @ heads2d xss

let isXMas : char list list -> bool =
    let isMS a b =
        (a, b) = ('M', 'S') || (a, b) = ('S', 'M')

    function
    | (x0 :: _ :: y0 :: _) ::
      (_ :: 'A' :: _ :: _) ::
      (y1 :: _ :: x1 :: _) ::
      _ -> isMS x0 x1 && isMS y0 y1
    | _ -> false

let solvePart1 (input : string list) : int =
    parse input |> heads2d |> List.count isXMas

let day04 =
    Day.day 04 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (18, 9))
    |> Day.addInput "input-real0.txt" (Some (2654, 1990))
