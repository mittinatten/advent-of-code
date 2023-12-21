type Bit =
    | Zero = 0
    | One = 1
type Seat = { Row: int; Column: int; SeatId: int }
type BoardingPass = string

let toBits (pass: BoardingPass) =
    pass
    |> Seq.toList
    |> List.map (fun letter ->
        match letter with
        | 'F' -> Bit.Zero
        | 'B' -> Bit.One
        | 'L' -> Bit.Zero
        | 'R' -> Bit.One
        | c -> failwith ("Unknown character '" + c.ToString() + "'"))

let toInteger (bits: Bit list): int =
    bits
    |> List.rev
    |> List.map (int)
    |> List.mapi (fun i bit -> bit <<< i)
    |> List.fold (|||) 0

let parseBoardingPass (pass: BoardingPass): Seat =
    let getCode = toBits >> toInteger
    { Row = getCode pass.[0..6]
      Column = getCode pass.[7..9]
      SeatId = getCode pass }

let findHighestSeatId (seats: Seat list) =
    seats
    |> List.fold (fun max seat -> if (seat.SeatId > max) then seat.SeatId else max) 0

let passes =
    System.IO.File.ReadLines("./5-input.txt")
    |> Seq.toList
    |> List.map parseBoardingPass

let highestSeatId = passes |> findHighestSeatId

let freeSeat =
    passes
    |> List.map (fun seat -> seat.SeatId)
    |> List.sort
    |> List.windowed 2
    |> List.find (fun seatRange -> seatRange.[1] - seatRange.[0] > 1)
    |> fun seatRange -> seatRange.[0] + 1

printfn "5a: %d" highestSeatId
printfn "5b: %d" freeSeat
