module Utils

type 'T ``[]`` with
    member xs.GetOp (n : int) : 'T option =
        if 0 <= n && n < xs.Length then Some (xs[n]) else None

module Seq =
    let assertPairs (xs : 'T seq) : 'T * 'T =
        let arr = Seq.toArray xs in

        if arr.Length = 2 then
            (arr[0], arr[1])
        else
            failwith $"bad length, expected 2 got %d{arr.Length}"

module List =
    let count (predicate : 'a -> bool) : 'a list -> int =
        let rec help acc =
            function
            | [] -> acc
            | x :: xs -> help (if predicate x then acc + 1 else acc) xs

        help 0

let timer (f : unit -> 'a) : 'a =
    let watch = System.Diagnostics.Stopwatch.StartNew ()
    let result = f ()
    watch.Stop ()
    printfn $"Time taken: %d{watch.ElapsedMilliseconds}ms"
    result
