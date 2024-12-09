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

type File = { Id : int ; Pos : int ; Len : int }

let solvePart1 (input : string list) : int64 =
    let lens = parse input @ [ 0 ]
    let totalLen = List.sum lens
    let buffer = Array.create totalLen -1

    let rec loadDisk (i : int) (pos : int) (acc : File list) : int list -> File list =
        function
        | x :: y :: xs ->
            loadDisk
                (i + 1)
                (pos + x + y)
                ({ Id = -1 ; Pos = pos + x ; Len = y } :: { Id = i ; Pos = pos ; Len = x } :: acc)
                xs
        | _ -> List.rev acc

    let nonEmpty f = f.Len > 0

    let disk = loadDisk 0 0 [] lens |> List.filter nonEmpty
    let files = disk |> List.filter (fun f -> f.Id <> -1) |> List.rev

    let rec defrag1 acc disk file =
        match disk with
        | block :: _ when file.Pos <= block.Pos -> List.revAppend acc disk
        | space :: disk when space.Id = -1 && file.Len <= space.Len ->
            if file.Len = space.Len then
                List.revAppend acc ({ file with Pos = space.Pos } :: disk)
            else
               List.revAppend acc (
                   { file with Pos = space.Pos }
                   :: { Id = -1; Pos = space.Pos + file.Len; Len = space.Len - file.Len }
                   :: disk
               )
        | block :: disk -> defrag1 (block :: acc) disk file
        | [] -> failwith "unreachable?"

    let rec dedup seen = function
        | file :: rest when file.Id <> -1 && not (Map.containsKey file.Id seen) -> dedup (Map.add file.Id file seen) rest
        | _ :: rest -> dedup seen rest
        | [] -> seen

    let defragged = List.fold (defrag1 []) disk files |> dedup Map.empty

    defragged |> Map.values
    |> Seq.iter (fun file ->
        for i in file.Pos .. file.Pos + file.Len - 1 do
            buffer[i] <- file.Id
    )

    checksum buffer 0 0L

let day09 =
    Day.day 09 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" (Some (1928, 2858))
    |> Day.addInput "input-real0.txt" (Some (6432869891895L, 6467290479134L))
