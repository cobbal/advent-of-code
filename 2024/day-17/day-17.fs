module Day17

open FSharpx.Text
open FSharpx
open Utils

type File =
    {
        PC : int
        A : int64
        B : int64
        C : int64
    }

let parse (input : string list) =
    input
    |> List.filter (not << Strings.isNullOrEmpty)
    |> List.map (List.ofSeq << Seq.map int << Strings.int64sIn)
    |> function
        | [ [ a ] ; [ b ] ; [ c ] ; program ] -> ({ PC = 0 ; A = a ; B = b ; C = c }, Array.ofList program)
        | _ -> failwith "bad parse"

let interp (file, program : int array) =
    let rec loop output file =
        if file.PC < 0 || program.Length < file.PC + 2 then
            List.rev output
        else
            let combo () =
                match program[file.PC + 1] with
                | c when 0 <= c && c < 4 -> c
                | 4 -> int file.A
                | 5 -> int file.B
                | 6 -> int file.C
                | _ -> failwith "bad op"

            let literal () = program[file.PC + 1]
            let pc = file.PC
            let file = { file with PC = file.PC + 2 }

            match program[pc] with
            | 0 (* adv *) -> loop output { file with A = file.A >>> combo () }
            | 1 (* bxl *) -> loop output { file with B = file.B ^^^ literal () }
            | 2 (* bst *) -> loop output { file with B = int64 (combo () %! 8) }
            | 3 (* jnz *) ->
                if file.A = 0 then file else { file with PC = literal () }
                |> loop output
            | 4 (* bxc *) -> loop output { file with B = file.B ^^^ file.C }
            | 5 (* out *) -> loop (combo () %! 8 :: output) file
            | 6 (* bdv *) -> loop output { file with B = file.A >>> combo () }
            | 7 (* cdv *) -> loop output { file with C = file.A >>> combo () }
            | _ -> failwith "bad opcode"

    loop [] file

let solvePart0 (input : string list) : int64 =
    parse input
    |> interp
    // |>! (List.map string >> String.concat "," >> printfn "%s")
    |> List.fold (fun a b -> 10L * a + int64 b) 0L

let solvePart1 (input : string list) : int64 =
    let startFile, program = parse input

    let rec solveNibble i (aNibs : int array) =
        if i >= aNibs.Length then // Success
            Array.fold (fun a b -> 8L * a + int64 b) 0L aNibs
        else if i < 0 then
            -1
        else if aNibs[i] >= 8 then
            if i <= 0 then // Failure
                -1
            else
                seq {
                    Array.take (i - 1) aNibs
                    [| aNibs[i - 1] + 1 |]
                    Array.create (aNibs.Length - i) 0
                }
                |> Array.concat
                |> solveNibble (i - 1)
        else
            let a = Array.fold (fun a b -> 8L * a + int64 b) 0L aNibs
            let out = interp ({ startFile with A = a }, program) |> Array.ofList

            if
                (out.Length = program.Length
                 && out[program.Length - i - 1] = program[program.Length - i - 1])
            then
                solveNibble (i + 1) aNibs
            else
                Array.updateAt i (aNibs[i] + 1) aNibs |> solveNibble i

    solveNibble 0 (Array.map (konst 0) program)

type ThisDay() =
    interface IDay with
        member this.day () =
            Day.create 17 solvePart0 solvePart1
            <| seq {
                "input-ex0.txt", Some (4635635210L, -1L)
                "input-real0.txt", Some (150520135L, 236581108670061L)
            }
