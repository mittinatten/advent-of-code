type CubeState = Inactive | Active
type ReactorState = ReactorState of CubeState [,,,]
type Step = ReactorState -> ReactorState

let parseInitialState (input: string list): ReactorState =
    let state = Array4D.init input.[0].Length input.Length 1 1
                    (fun i j k l -> match input.[i].[j] with | '#' -> Active | _ -> Inactive)

    ReactorState state

let sumActiveNeighbors (ReactorState state) i j k l =
    let mini = max (i-1) 0
    let maxi = min (i+1) (Array4D.length1 state - 1)
    let minj = max (j-1) 0
    let maxj = min (j+1) (Array4D.length2 state  - 1)
    let mink = max (k-1) 0
    let maxk = min (k+1) (Array4D.length3 state - 1)
    let minl = max (l-1) 0
    let maxl = min (l+1) (Array4D.length3 state - 1)

    let neighborStates = [
        for i' in [mini..maxi] do
            for j' in [minj..maxj] do
                for k' in [mink..maxk] do
                    for l' in [minl..maxl] do
                        if (i' <> i || j' <> j || k' <> k || l' <> l) then
                            yield state.[i',j',k',l'] ]

    neighborStates |> List.filter (fun nb -> nb = Active) |> List.length

let updateCube (ReactorState state) i j k l: CubeState =
    let nbcount = sumActiveNeighbors (ReactorState state) i j k l

    match state.[i,j,k,l] with
    | Inactive when nbcount = 3 -> Active
    | Active when (nbcount = 2 || nbcount = 3) -> Active
    | _ -> Inactive

let step: Step =
    fun (ReactorState state) ->
        let l1 = Array4D.length1 state
        let l2 = Array4D.length2 state
        let l3 = Array4D.length3 state
        let l4 = Array4D.length3 state

        // grow array in every step
        let enlargedState = Array4D.init (l1 + 2) (l2 + 2) (l3 + 2) (l4 + 2) (fun i j k l ->
            if i <= l1 && j <= l2 && k <= l3 && l <= l4 && i > 0 && j > 0 && k > 0 && l > 0
            then state.[i-1, j-1, k-1, l-1]
            else Inactive)

        let newState = Array4D.init (l1 + 2) (l2 + 2) (l3 + 2) (l4 + 2) (updateCube (ReactorState enlargedState))

        ReactorState newState

let sumActiveCubes (ReactorState state) =
    seq {
        for i in 0 .. (Array4D.length1 state - 1) do
            for j in 0 .. (Array4D.length2 state - 1) do
                for k in 0 .. (Array4D.length3 state - 1) do
                    for l in 0 .. (Array4D.length4 state - 1) do
                        if state.[i,j,k,l] = Active then yield 1
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
