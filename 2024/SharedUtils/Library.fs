module Utils

type 'T ``[]`` with
    member xs.GetOp(n: int) : 'T option =
        if 0 <= n && n < xs.Length then Some(xs[n]) else None

module Seq =
    let assertPairs (xs: 'T seq) : 'T * 'T =
        let arr = Seq.toArray xs in

        if arr.Length = 2 then
            (arr[0], arr[1])
        else
            failwith "bad length, expected 2 got %d{arr.Length}"

let checkResults expected actual =
    match expected with
    | Some expectation ->
        if expectation = actual then
            printfn " \u2705 good"
        else
            (printfn " \u274c bad"
             failwith "Wrong answer")
    | None -> printfn ""
