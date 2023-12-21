type Seat =
    | Empty
    | Taken
    | Floor

type Seats = Seats of Seat [,]
type SeatLocation = int * int
type NeighborCounter = Seats -> SeatLocation -> int

let parseRow (row: string): Seat [] =
    row
    |> Seq.map (fun (seat: char) ->
        match seat with
        | 'L' -> Empty
        | _ -> Floor)
    |> Array.ofSeq

let readSeats (input: string seq): Seats =
    let rowArray = input |> Seq.map parseRow |> Array.ofSeq
    Array2D.init rowArray.Length rowArray.[0].Length (fun i j -> rowArray.[i].[j])
    |> Seats

let directions =
    [ (-1, -1)
      (-1, 0)
      (-1, 1)
      (0, -1)
      (0, 1)
      (1, -1)
      (1, 0)
      (1, 1) ]

let countTakenNeighbors: NeighborCounter =
    fun (Seats seats) (i, j) ->
        let getNeighborSeats (i, j) =
            let maxi = Array2D.length1 seats
            let maxj = Array2D.length2 seats
            directions
            |> List.map (fun (di, dj) -> (i + di, j + dj))
            |> List.filter (fun (ni, nj) -> (ni >= 0 && ni < maxi && nj >= 0 && nj < maxj))

        getNeighborSeats (i, j)
        |> List.fold (fun count (ni, nj) ->
            match seats.[ni, nj] with
            | Taken -> count + 1
            | _ -> count) 0


let countTakenNeighborsInSight: NeighborCounter =
    fun (Seats seats) (i, j) ->
        let maxi = Array2D.length1 seats
        let maxj = Array2D.length2 seats

        let extendDirecion d =
            match d with
            | d when d > 0 -> d + 1
            | d when d < 0 -> d - 1
            | _ -> 0

        let rec searchDirection (i, j) (di, dj) =
            let ni = i + di
            let nj = j + dj

            if (ni >= maxi || ni < 0 || nj >= maxj || nj < 0)
            then (di, dj)
            elif seats.[ni, nj] <> Floor
            then (di, dj)
            else searchDirection (i, j) (extendDirecion di, extendDirecion dj)

        let getNeighborSeats (i, j) =
            directions
            |> List.map (searchDirection (i, j))
            |> List.map (fun (di, dj) -> (i + di, j + dj))
            |> List.filter (fun (ni, nj) -> (ni >= 0 && ni < maxi && nj >= 0 && nj < maxj))

        getNeighborSeats (i, j)
        |> List.fold (fun count (ni, nj) ->
            match seats.[ni, nj] with
            | Taken -> count + 1
            | _ -> count) 0

let updateEmptySeat (counter: NeighborCounter) (Seats seats) (i, j) =
    let takenNeighbours = counter (Seats seats) (i, j)
    if (takenNeighbours > 0) then Empty else Taken

let updateTakenSeat (counter: NeighborCounter) crowdingIndex (Seats seats) (i, j) =
    let takenNeighbours = counter (Seats seats) (i, j)
    if (takenNeighbours > crowdingIndex) then Empty else Taken

let updateSeats (counter: NeighborCounter) (crowdingIndex: int) (Seats seats): Seats =
    seats
    |> Array2D.mapi (fun i j seat ->
        match seat with
        | Floor -> Floor
        | Empty -> updateEmptySeat counter (Seats seats) (i, j)
        | Taken -> updateTakenSeat counter crowdingIndex (Seats seats) (i, j))
    |> Seats

let countTakenSeats (Seats seats) =
    seq {
        for i in 0 .. Array2D.length1 seats - 1 do
            for j in 0 .. Array2D.length2 seats - 1 do
                yield seats.[i, j]
    }
    |> Seq.map (fun seat ->
        match seat with
        | Taken -> 1
        | _ -> 0)
    |> Seq.fold (+) 0

let stabilizeSeating nbCounter crowdingIndex seats =
    let initialCount = countTakenSeats seats

    let rec step count seats =
        let newSeats =
            updateSeats nbCounter crowdingIndex seats

        let newCount = countTakenSeats newSeats
        if (newCount = count) then newCount else step newCount newSeats

    step initialCount seats

let input =
    System.IO.File.ReadLines("./11-input.txt")

let seats = readSeats input

printfn "11a: %d" (seats |> stabilizeSeating countTakenNeighbors 3)

printfn
    "11b: %d"
    (seats
     |> stabilizeSeating countTakenNeighborsInSight 4)
