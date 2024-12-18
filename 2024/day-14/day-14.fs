module Day14

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open Utils
open Grid

let parse (input : string list) : (XY * XY) list =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (Strings.int64sIn >> Array.ofSeq >> Array.map int)
    |> List.map (
        function
        | [| px ; py ; vx ; vy |] -> (XY (px, py), XY (vx, vy))
        | arr -> failwith $"parse error %A{arr}"
    )

let display width height (robots : XY seq) =
    let grid = Array.init height (fun _ -> Array.create width '.')

    for p in robots do
        grid[p.Y][p.X] <- 'X'

    printfn ""
    Array.iter<char array> (fun row -> printfn "%s" <| System.String row) grid

let move width height n (p : XY, v : XY) =
    XY ((p.X + n * v.X) %! width, (p.Y + n * v.Y) %! height)

let quad width height (xy : XY) =
    match (compare xy.X (width / 2), compare xy.Y (height / 2)) with
    | -1, -1 -> Some 0
    | -1, 1 -> Some 1
    | 1, -1 -> Some 2
    | 1, 1 -> Some 3
    | _ -> None

let solvePart0 (input : string list) : int64 =
    let (w, h) = if List.length input > 30 then (101, 103) else (11, 7)

    parse input
    |> List.map (move w h 100)
    // |> tee (display w h)
    |> List.choose (quad w h)
    |> List.groupBy id
    |> List.map (snd >> List.length)
    |> List.fold (*) 1
    |> int64

let moves width height (arr : struct (XY * XY) array) =
    for i in 0 .. arr.Length - 1 do
        let struct (p, v) = arr[i]
        let p' = XY ((p.X + v.X) %! width, (p.Y + v.Y) %! height)
        arr[i] <- struct (p', v)

type StrongestDatum =
    {
        Index : int
        RowWeight : int
        ColWeight : int
    }

let solvePart1 (input : string list) : int64 =
    let (w, h) = if List.length input > 30 then (101, 103) else (11, 7)
    let arr = parse input |> Array.ofList

    let strongestRowsAndColumns =
        seq {
            for n in 1 .. (max w h) do
                let result = Array.map (move w h n) arr

                let find (fn : XY -> int) =
                    Array.groupBy fn result |> Array.map (snd >> Array.length) |> Array.max

                {
                    Index = n
                    RowWeight = find _.X
                    ColWeight = find _.Y
                }
        }
        |> Array.ofSeq

    let remX = Array.maxBy _.RowWeight strongestRowsAndColumns |> _.Index |> flip (%!) w
    let remY = Array.maxBy _.ColWeight strongestRowsAndColumns |> _.Index |> flip (%!) h

    chineseRemainderTheorem [ (remX, w) ; (remY, h) ]
    // |> tee (fun n -> Array.map (move w h n) arr |> display w h)
    |> int64

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 14 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (12, 31)
                "input-real0.txt", Some (222208000, 7623)
            }
