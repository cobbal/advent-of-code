namespace Utils

type Day =
    {
        dayNumber : int
        solvePart0 : string list -> obj
        solvePart1 : string list -> obj
        inputs : (string * (obj * obj) option) seq
    }

module Day =
    let create<'T0, 'T1>
        number
        (solvePart0 : string list -> 'T0)
        (solvePart1 : string list -> 'T1)
        (inputs : (string * ('T0 * 'T1) option) seq)
        : Day
        =
        {
            dayNumber = number
            solvePart0 = fun i -> solvePart0 i :> obj
            solvePart1 = fun i -> solvePart1 i :> obj
            inputs =
                inputs
                |> Seq.map (fun (input, expected) ->
                    (input, Option.map (fun (e0, e1) -> (e0 :> obj, e1 :> obj)) expected)
                )
        }

type IDay =
    abstract member day : unit -> Day
