type Degrees = int
type Distance = int
type Orientation = Orientation of Degrees
type MoveDirection =
    | North of Distance
    | East of Distance
    | South of Distance
    | West of Distance
type Turn =
    | Left of Degrees
    | Right of Degrees
type Action =
    | Direction of MoveDirection
    | Turn of Turn
    | Forward of Distance
type Latitude = int
type Longitude = int
type Position = Position of Latitude * Longitude
type BoatState = BoatState of Orientation * Position

let parseAction (input: string) =
    let amount = int input.[1..]
    match input.[0] with
    | 'F' -> (Forward amount)
    | 'R' -> (Turn (Right amount))
    | 'L' -> (Turn (Left amount))
    | 'N' -> (Direction (North amount))
    | 'E' -> (Direction (East amount))
    | 'S' -> (Direction (South amount))
    | 'W' -> (Direction (West amount))
    | _ -> failwith <| sprintf "Unknown input %s" input

let moveBoat (action: Action) (BoatState (Orientation orientation, Position (lat, lon))) =
    let move (direction: MoveDirection) =
        match direction with
        | North amount -> (lat + amount, lon)
        | East amount -> (lat, lon + amount)
        | South amount -> (lat - amount, lon)
        | West amount -> (lat, lon - amount)
        |> fun (lat, lon) -> Position (lat, lon)

    let moveForward amount =
        match orientation with
        | 0 -> move (North amount)
        | 90 -> move (East amount)
        | 180 -> move (South amount)
        | 270 -> move (West amount)
        | _ -> failwith "unknown orientation"

    let rotate (turn): int =
        match (turn) with
        | Right amount -> (orientation + amount) % 360
        | Left amount -> ((orientation + 360) - amount) % 360

    match action with
    | Turn turn -> BoatState (Orientation (rotate turn), Position (lat, lon))
    | Forward amount -> BoatState (Orientation orientation, moveForward amount)
    | Direction direction -> BoatState (Orientation orientation, move direction)


let input = System.IO.File.ReadLines("./12-input.txt")
let actions = input |> Seq.map parseAction

let finalPosition = actions |> Seq.fold (fun state action -> moveBoat action state) (BoatState (Orientation 90, Position (0, 0)))
let finalDistance = finalPosition |> fun (BoatState (orientation, Position (lat, lon))) -> (abs lat) + (abs lon)

printfn "%A" finalDistance
