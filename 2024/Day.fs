module Day

type Day =
    {
        dayNumber : int
        solvePart0 : string list -> int64
        solvePart1 : string list -> int64
        inputs : (string * (int64 * int64) option) list
    }

let day number solvePart0 solvePart1 =
    {
        dayNumber = number
        solvePart0 = solvePart0
        solvePart1 = solvePart1
        inputs = []
    }

let addInput file expected day = { day with inputs = day.inputs @ [(file, expected)] }
