module Day09

open FSharpx.Collections
open FSharpx.Text
open FSharpx
open FSharpx.Text.Regex
open Utils

let parse (input : string list) : int list =
    input |> List.head |> List.ofSeq |> List.map (fun c -> int $"{c}")

let bufStr (buf : int array) : string =
    Array.map
        (function
        | -1 -> '.'
        | i -> '0' + char i)
        buf
    |> System.String

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

[<ReferenceEquality>]
type File =
    {
        id : int
        mutable pos : int
        mutable length : int
        mutable next : Option<File>
        mutable prev : Option<File>
    }

    static member Create id pos length after =
        let file =
            {
                id = id
                pos = pos
                length = length
                next = None
                prev = after
            }

        file.next <-
            Option.bind
                (fun prev ->
                    let ret = prev.next
                    prev.next <- Some file
                    ret
                )
                after

        file

    member file.Head () =
        match file.prev with
        | None -> file
        | Some prev -> prev.Head ()

type Disk =
    {
        mutable head : File
        mutable tail : File
        mutable frees : Map<int, File>
    }

    static member Create (tail : File) =
        let rec scan (f : File) (frees : Map<int, File>) : File * Map<int, File> =
            let mutable frees = frees

            if f.id = -1 then
                frees <- frees |> Map.filter (fun k _ -> k > f.length) |> Map.add f.length f

            match f.prev with
            | None -> (f, frees)
            | Some prev -> scan prev frees

        let head, frees = scan tail Map.empty

        {
            head = head
            tail = tail
            frees = frees
        }

    member this.MoveInto (space : File) (file : File) =
        let newSpace = File.Create -1 file.pos file.length file.prev

        match file.next with
        | Some next -> next.prev <- Some newSpace
        | None -> this.tail <- newSpace

        space.prev
        |> Option.iter (fun prev ->
            prev.next <- Some file
            file.prev <- Some prev
        )

        newSpace.next <- file.next
        file.pos <- space.pos

        if space.length > file.length then
            file.next <- Some space
            space.prev <- Some file
            space.pos <- space.pos + file.length
            space.length <- space.length - file.length
        else
            file.next <- space.next
            space.next |> Option.iter (fun next -> next.prev <- Some file)

    member this.Defrag (file : File) =
        if file.id = -1 then
            Option.iter this.Defrag file.prev
        else
            let next = file.prev

            this.FindSpace file.length
            |> Option.iter (fun free ->
                if free.pos < file.pos then
                    this.MoveInto free file
            )

            this.VerifyInvariants ()
            Option.iter this.Defrag next

    member this.FindSpace len =
        let rec search f =
            match f.next with
            | _ when f.id = -1 && f.length >= len -> Some f
            | Some next -> search next
            | None -> None

        search this.head

    member this.VerifyInvariants () =
        if true then () else
            let rec verify pos f =
                assert (f.pos = pos)

                match f.next with
                | Some next ->
                    assert (next <> f)
                    assert (next.prev = Some f)
                    verify (f.pos + f.length) next
                | None -> assert (f = this.tail)

            verify 0 this.head

    member this.Iter (fn : File -> unit) =
        this.VerifyInvariants ()

        let rec loop f =
            fn f

            match f.next with
            | Some next -> loop next
            | None -> ()

        loop this.head

let solvePart1 (input : string list) : int64 =
    let lens = parse input @ [ 0 ]
    let totalLen = List.sum lens
    let buffer = Array.create totalLen -1

    let rec loadDisk i pos tail =
        function
        | x :: y :: xs ->
            let mutable tail = tail

            if x > 0 then
                tail <- Some (File.Create i pos x tail)

            if y > 0 then
                tail <- Some (File.Create -1 (pos + x) y tail)

            loadDisk (i + 1) (pos + x + y) tail xs
        | _ -> tail

    let disk = (loadDisk 0 0 None lens).Value |> Disk.Create
    disk.VerifyInvariants ()
    disk.Defrag disk.tail

    disk.Iter (fun file ->
        for i in file.pos .. file.pos + file.length - 1 do
            buffer[i] <- file.id
    )

    checksum buffer 0 0L

let day09 =
    Day.day 09 solvePart0 solvePart1
    |> Day.addInput "input-ex0.txt" None
    |> Day.addInput "input-real0.txt" None
