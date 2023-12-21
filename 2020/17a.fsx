type CubeState = Inactive | Active
type ReactorState = ReactorState of CubeState [,,]
type Step = ReactorState -> ReactorState

let parseInitialState (input: string list): ReactorState =
    let state = Array3D.init input.[0].Length input.Length 1
                    (fun i j k -> match input.[i].[j] with | '#' -> Active | _ -> Inactive)
    ReactorState state

let sumActiveNeighbors (ReactorState state) i j k =
    let mini = max (i-1) 0
    let maxi = min (i+1) (Array3D.length1 state - 1)
    let minj = max (j-1) 0
    let maxj = min (j+1) (Array3D.length2 state  - 1)
    let mink = max (k-1) 0
    let maxk = min (k+1) (Array3D.length3 state - 1)

    let neighborStates = [
        for i' in [mini..maxi] do
            for j' in [minj..maxj] do
                for k' in [mink..maxk] do
                    if (i' <> i || j' <> j || k' <> k) then
                        yield state.[i',j',k'] ]

    neighborStates |> List.filter (fun nb -> nb = Active) |> List.length

let updateCube (ReactorState state) i j k: CubeState =
    let nbcount = sumActiveNeighbors (ReactorState state) i j k

    match state.[i,j,k] with
    | Inactive when nbcount = 3 -> Active
    | Active when (nbcount = 2 || nbcount = 3) -> Active
    | _ -> Inactive

let step: Step =
    fun (ReactorState state) ->
        let l1 = Array3D.length1 state
        let l2 = Array3D.length2 state
        let l3 = Array3D.length3 state

        // grow array in every step
        let enlargedState = Array3D.init (l1 + 2) (l2 + 2) (l3 + 2) (fun i j k ->
                            if i <= l1 && j <= l2 && k <= l3 && i > 0 && j > 0 && k > 0
                            then state.[i-1, j-1, k-1]
                            else Inactive)

        let newState = Array3D.init (l1 + 2) (l2 + 2) (l3 + 2) (updateCube (ReactorState enlargedState))

        ReactorState newState

let sumActiveCubes (ReactorState state) =
    seq {
        for i in 0 .. (Array3D.length1 state - 1) do
            for j in 0 .. (Array3D.length2 state - 1) do
                for k in 0 .. (Array3D.length3 state - 1) do
                    if state.[i,j,k] = Active then yield 1
    } |> Seq.fold (+) 0

let example = """.#.
..#
###"""

let data = """######.#
##.###.#
#.###.##
..#..###
##.#.#.#
##...##.
#.#.##.#
.###.###"""

let input = data.Split("\n") |> List.ofSeq
let initialState = parseInitialState input

let finalState = [1..6] |> List.fold (fun state _ -> step state) initialState

printfn "%d" (sumActiveCubes finalState)
