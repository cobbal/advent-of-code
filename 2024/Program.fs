module Program

open System
open System.Threading
open FSharpx
open FSharpx.Option
open Utils
open FSharpx.IO
open FSharpx.Collections

let allDays =
    typeof<IDay>.Assembly.GetTypes ()
    |> Seq.filter (fun x -> not x.IsInterface && typeof<IDay>.IsAssignableFrom x)
    |> Seq.map (fun x -> (Activator.CreateInstance x :?> IDay).day ())
    |> Array.ofSeq
    |> Array.sortBy _.dayNumber

let days =
    let dayArgs = Environment.GetCommandLineArgs () |> Array.skip 1

    let getDay : string -> Day option =
        let dayMap =
            allDays
            |> Array.map (fun day -> (day.dayNumber, day))
            |> Map.ofArray
            |> Map.add -1 (Array.last allDays)

        FSharpOption.ParseInt >=> flip Map.tryFind dayMap

    if Array.isEmpty dayArgs then
        allDays
    else
        let good, bad =
            Array.map (fun x -> getDay x |> Choice.ofOption x) dayArgs
            |> Array.partitionChoices

        if Array.isEmpty bad then
            good
        else
            printfn $"Unknown days requested: %A{bad}"
            exit 1

let main () =
    let all () =
        flip Array.map days
        <| fun day ->
            let dayPath = $"day-%02d{day.dayNumber}"
            printfn $"=== %s{dayPath} ==="

            timerf (fun ms ->
                printfn $"time: %d{ms}ms"
                printfn ""
            )
            <| fun () ->
                flip Seq.map day.inputs
                <| fun (inputFile, expected) ->
                    let lines = readFile $"%s{dayPath}/%s{inputFile}" |> List.ofSeq
                    printf $"%s{inputFile}: "
                    let result0 = day.solvePart0 lines
                    printf $"%d{result0} "
                    let result1 = day.solvePart1 lines
                    printf $"%d{result1} "

                    match expected with
                    | Some expectation ->
                        if expectation = (result0, result1) then
                            printfn " \u2705 good"
                            true
                        else
                            printfn " \u274c bad"
                            false
                    | None ->
                        printfn ""
                        true
                |> List.ofSeq
                |> List.forall id
        |> (fun successes ->
            if not (Array.forall id successes) then
                (printfn ""
                 printfn "\u274c Exiting due to earlier errors"
                 exit 1)
        )

    if Array.length days > 1 then
        timer "Total time" all
    else
        all ()

if false then
    let workerThread = Thread (main, Int32.MaxValue / 32 * 32)
    workerThread.Start ()
    workerThread.Join ()
else
    main ()
