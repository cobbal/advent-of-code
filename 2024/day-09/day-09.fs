module Day09

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open FSharpx.Text.Regex
open Utils

let parse (input : string list) : int list =
    input |> List.head |> List.ofSeq |> List.map (fun c -> int $"{c}")

let rec checksum (buffer : int array) pos acc =
    if pos < buffer.Length then
        checksum buffer (pos + 1) (acc + int64 pos * max 0L (int64 buffer[pos]))
    else
        acc

let solvePart0 (input : string list) : int64 =
    let lens = parse input @ [ 0 ]
    let totalLen = List.sum lens
    let buffer = Array.create totalLen -1

    let rec loadBuffer pos id =
        function
        | x :: y :: xs ->
            Array.fill buffer pos x id
            loadBuffer (pos + x + y) (id + 1) xs
        | _ -> ()

    let rec compact fwd rev =
        if fwd < rev then
            if buffer[fwd] <> -1 then
                compact (fwd + 1) rev
            else if buffer[rev] = -1 then
                compact fwd (rev - 1)
            else
                buffer[fwd] <- buffer[rev]
                buffer[rev] <- -1
                compact (fwd + 1) (rev - 1)

    loadBuffer 0 0 lens
    compact 0 (totalLen - 1)
    checksum buffer 0 0L

[<Struct>]
type File = { Id : int ; Pos : int ; Len : int }

let solvePart1 (input : string list) : int64 =
    let lens = parse input @ [ 0 ]
    let totalLen = List.sum lens
    let buffer = Array.create totalLen -1

    let rec loadDisk (i : int) (pos : int) (acc : File list) : int list -> File array =
        function
        | x :: y :: xs -> loadDisk (i + 1) (pos + x + y) ({ Id = i ; Pos = pos ; Len = x } :: acc) xs
        | _ -> List.rev acc |> Array.ofList

    let disk = loadDisk 0 0 [] lens

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
        | file :: rest when not (Map.containsKey file.Id seen) ->
            dedup (Map.add file.Id file seen) rest
        | _ :: rest -> dedup seen rest
        | [] -> seen

    let defragged =
        Array.foldBack (defrag1 0 0) disk disk |> List.ofArray |> dedup Map.empty

    defragged
    |> Map.values
    |> Seq.iter (fun file ->
        for i in file.Pos .. file.Pos + int file.Len - 1 do
            buffer[int i] <- int file.Id
    )

    checksum buffer 0 0L

let day09 =
    Day.day 09 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (1928, 2858))
    |> Day.addInput "input-real0.txt" (Some (6432869891895L, 6467290479134L))
