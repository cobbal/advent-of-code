module Day13

open Utils

type XY = { X : int64 ; Y : int64 }
let xy x y = { X = x ; Y = y }
type Machine = { A : XY ; B : XY ; P : XY }

let parse input =
    let rec chunk : XY list -> Machine list =
        function
        | [] -> []
        | a :: b :: p :: rest -> { A = a ; B = b ; P = p } :: chunk rest
        | line :: _ -> failwith $"bad parse: %A{line}"

    let parseXY =
        function
        | [| x ; y |] -> xy x y
        | arr -> failwith $"bad parse: %A{arr} is not a point"

    input
    |> List.map (Strings.int64sIn >> Array.ofSeq)
    |> List.filter (not << Array.isEmpty)
    |> List.map parseXY
    |> chunk

let solve (m : Machine) =
    // px = na * ax + nb * bx
    // py = na * ay + nb * by

    // na = (px - nb * bx) / ax
    // py = (px - nb * bx) * ay / ax + nb * by
    //    = px * ay / ax - nb * bx * ay / ax + nb * by
    //    = px * ay / ax - nb * (bx * ay / ax - by)
    // nb = (- py + px * ay / ax) / (bx * ay / ax - by)
    //    = (- py * ax + px * ay) / (bx * ay - by * ax)

    let nb = (-m.P.Y * m.A.X + m.P.X * m.A.Y) / (m.B.X * m.A.Y - m.B.Y * m.A.X)
    let na = (m.P.X - nb * m.B.X) / m.A.X

    if m.P.X = na * m.A.X + nb * m.B.X && m.P.Y = na * m.A.Y + nb * m.B.Y then
        3L * na + nb
    else
        0L

let solvePart0 (input : string list) : int64 =
    parse input |> List.map solve |> List.sum |> int64

let solvePart1 (input : string list) : int64 =
    let upgrade m =
        { m with
            P = xy (m.P.X + 10000000000000L) (m.P.Y + 10000000000000L)
        }

    parse input |> List.map (upgrade >> solve) |> List.sum |> int64

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 13 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (480L, 875318608908L)
                "input-real0.txt", Some (33209L, 83102355665474L)
            }
