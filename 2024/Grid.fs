module Grid

open System.Diagnostics

type Dir =
    | N
    | E
    | S
    | W

[<Struct>]
type XY =
    val X : int
    val Y : int
    new(x, y) = { X = x ; Y = y }

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member this.N = XY (this.X, this.Y - 1)

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member this.E = XY (this.X + 1, this.Y)

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member this.S = XY (this.X, this.Y + 1)

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member this.W = XY (this.X - 1, this.Y)

    member this.Item
        with get dir =
            match dir with
            | N -> this.N
            | E -> this.E
            | S -> this.S
            | W -> this.W

    override this.ToString () = $"%d{this.X},%d{this.Y}"

module Dir =
    let ofChar =
        function
        | '^' -> N
        | '>' -> E
        | 'v' -> S
        | '<' -> W
        | c -> failwith $"bad direction {c}"

    let toChar =
        function
        | N -> '^'
        | E -> '>'
        | S -> 'v'
        | W -> '<'

    let clockwise =
        function
        | N -> E
        | E -> S
        | S -> W
        | W -> N

    let counterClockwise =
        function
        | N -> W
        | E -> N
        | S -> E
        | W -> S

    let rev =
        function
        | N -> S
        | E -> W
        | S -> N
        | W -> E

type Grid(grid : byte array, width : int, height : int) =
    member this.Grid = grid
    member this.Width = width
    member this.Height = height

    member this.ContainsIndex (pos : XY) =
        0 <= pos.X && pos.X < this.Width && 0 <= pos.Y && pos.Y < this.Height

    member this.Item
        with get pos : char =
            if this.ContainsIndex pos then
                char this.Grid[pos.Y * width + pos.X]
            else
                char 0
        and set pos (value : char) =
            assert this.ContainsIndex pos
            this.Grid[pos.Y * width + pos.X] <- byte value

    new(input : string seq)
        =
        let grid =
            Seq.map (fun (s : string) -> System.Text.Encoding.UTF8.GetBytes s) input
            |> Array.ofSeq

        let width = Array.length grid[0]
        let height = Array.length grid

        Grid (Array.concat grid, width, height)

    member this.Copy () = Grid (Array.copy grid, width, height)

    member this.Indices () =
        seq {
            for y in 0 .. height - 1 do
                for x in 0 .. width - 1 do
                    yield XY (x, y)
        }

    member this.FindIndicesOf (target : char) =
        seq {
            for pos in this.Indices () do
                if this[pos] = target then
                    yield pos
        }

    member this.Print (overrides : Map<XY, char>) =
        printfn ""

        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                let xy = XY (x, y)
                printf $"%c{defaultArg (Map.tryFind xy overrides) this[xy]}"

            printfn ""

    member this.Swap (pos0, pos1) =
        let temp = this[pos0]
        this[pos0] <- this[pos1]
        this[pos1] <- temp

    member private this.AsTuple () = (this.Grid, this.Width, this.Height)

    override this.Equals other =
        match other with
        | :? Grid as other -> this.AsTuple () = other.AsTuple ()
        | _ -> false

    override this.GetHashCode () = (this.AsTuple ()).GetHashCode ()

    interface System.IComparable<Grid> with
        member this.CompareTo other =
            compare (this.AsTuple ()) (other.AsTuple ())

    interface System.IComparable with
        member this.CompareTo (other) =
            compare (this.AsTuple ()) ((other :?> Grid).AsTuple ())
