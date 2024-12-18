namespace Utils

type Day =
    {
        dayNumber : int
        solvePart0 : string list -> int64
        solvePart1 : string list -> int64
        inputs : (string * (int64 * int64) option) seq
    }

module Day =
    let create number solvePart0 solvePart1 inputs =
        {
            dayNumber = number
            solvePart0 = solvePart0
            solvePart1 = solvePart1
            inputs = inputs
        }

type IDay =
    abstract member day : unit -> Day
