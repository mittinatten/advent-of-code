type Bit =
    | Zero
    | One

type Position = Position of int

type Mask = Mask of (Position * Bit) list

type Value = Value of int64

let readMask (input: string) =
    input
    |> Seq.mapi (fun pos ch -> (pos, ch))
    |> Seq.fold (fun mask (pos, ch) ->
        match ch with
        | 'X' -> mask
        | '1' -> (Position pos, One) :: mask
        | '0' -> (Position pos, Zero) :: mask
        | _ -> failwith "invalid mask") []
    |> List.ofSeq
    |> List.rev
    |> Mask

let readValue (input: string) = input |> int64

let applyMask (Mask mask) (Value value) =
    mask
    |> List.fold (fun output ((Position pos), bit) ->
        match bit with
        | One -> output ||| (1L <<< pos)
        | Zero -> output &&& (~~~(1L <<< pos))) value

let mask =
    Mask [ (Position 1, One)
           (Position 2, Zero) ]

printfn "%A" (applyMask mask (Value 2L))
printfn "%A" (applyMask mask (Value 5L))
