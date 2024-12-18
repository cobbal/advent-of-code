module Day09

open FSharpx.Collections
open FSharpx
open Utils

let parse (input : string list) : int list =
    input |> List.head |> List.ofSeq |> List.map (fun c -> int $"{c}")

let solvePart0 (input : string list) : int64 =
    let lens = parse input @ [ 0 ]
    let totalLen = List.sum lens

    let rec loadBuffer acc id =
        function
        | x :: y :: xs -> loadBuffer (Array.create y -1 :: Array.create x id :: acc) (id + 1) xs
        | _ -> List.rev acc |> Array.concat

    let buffer = loadBuffer [] 0 lens

    let rec compact acc fwd rev =
        if rev < fwd then
            acc
        else if buffer[fwd] <> -1 then
            compact (acc + int64 (fwd * buffer[fwd])) (fwd + 1) rev
        else if buffer[rev] = -1 then
            compact acc fwd (rev - 1)
        else
            compact (acc + int64 (fwd * buffer[rev])) (fwd + 1) (rev - 1)

    compact 0L 0 (totalLen - 1)

[<Struct>]
type File = { Id : int ; Pos : int ; Len : int }

let solvePart1 (input : string list) : int64 =
    let lens = parse input @ [ 0 ]

    let rec loadDisk (i : int) (pos : int) (acc : File list) : int list -> File array =
        function
        | x :: y :: xs -> loadDisk (i + 1) (pos + x + y) ({ Id = i ; Pos = pos ; Len = x } :: acc) xs
        | _ -> List.rev acc |> Array.ofList

    let rec defrag1 (i : int) (pos : int) (file : File) (disk : File array) : File array =
        let nextFile = disk[int i]

        if file.Pos <= pos then
            disk
        else if int file.Len <= int (nextFile.Pos - pos) then
            Array.insertAt (int i) { file with Pos = pos } disk
        else
            defrag1 (i + 1) (nextFile.Pos + nextFile.Len) file disk

    let rec dedup seen =
        function
        | file :: rest when not (Map.containsKey file.Id seen) -> dedup (Map.add file.Id file seen) rest
        | _ :: rest -> dedup seen rest
        | [] -> seen

    let disk = loadDisk 0 0 [] lens

    let defragged =
        Array.foldBack (defrag1 0 0) disk disk |> List.ofArray |> dedup Map.empty

    defragged
    |> Map.values
    |> Seq.fold (fun acc file -> acc + int64 file.Id * int64 file.Len * int64 (2 * file.Pos + file.Len - 1) / 2L) 0L

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 09 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (1928, 2858)
                "input-real0.txt", Some (6432869891895L, 6467290479134L)
            }
