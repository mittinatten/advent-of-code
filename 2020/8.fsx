type Operation =
    | Nop
    | Acc
    | Jmp

type Argument = int
type Instruction = Operation * Argument
type Position = int
type Value = int
type FailValue = int
type ProgramResult = Result<Value, FailValue>
type State = Position * Value

type Program = Instruction array

let parseInstruction (input: string): Instruction =
    let parts = input.Split(" ")

    let op =
        match parts.[0] with
        | "nop" -> Nop
        | "acc" -> Acc
        | "jmp" -> Jmp
        | unk -> failwith ("Unknown operation '" + unk + "'")

    let arg =
        match parts.[1].Chars(0) with
        | '+' -> int parts.[1].[1..]
        | '-' -> -1 * int parts.[1].[1..]
        | _ -> failwith "Illegal argument"

    (op, arg)

let step (program: Program) ((pos, value): State): State =
    let (op, arg) = program.[pos]
    match op with
    | Nop -> (pos + 1, value)
    | Acc -> (pos + 1, value + arg)
    | Jmp -> (pos + arg, value)

let run (program: Program): ProgramResult =
    let rec checkNextStep (state: State) (visitedStates: Set<Position>): ProgramResult =
        let (nextPos, nextVal) = step program state
        if nextPos >= program.Length
        then Ok nextVal
        elif visitedStates.Contains(nextPos)
        then Error nextVal
        else checkNextStep (nextPos, nextVal) (visitedStates.Add(nextPos))

    checkNextStep (0, 0) (Set [])

let changeOpUntilProgramWorks (program: Program): Value =
    let checkSwapOp (pos: Position): ProgramResult =
        let (op, arg) = program.[pos]

        let newInstruction =
            match op with
            | Nop -> (Jmp, arg)
            | Jmp -> (Nop, arg)
            | Acc -> (Acc, arg)

        run (Array.append (Array.append program.[0..(pos - 1)] [| newInstruction |]) program.[pos + 1..])

    let positionsOfJmpAndNop =
        program
        |> Seq.mapi (fun i ins -> (i, ins) )
        |> Seq.filter (fun (i, ins) ->
            match ins with
            | (Nop, _) -> true
            | (Jmp, _) -> true
            | _ -> false)
        |> Seq.map fst

    let result =
        positionsOfJmpAndNop
        |> Seq.tryPick (fun i ->
            match checkSwapOp i with
            | Ok value -> Some value
            | Error _ -> None)

    match result with
    | Some value -> value
    | None -> failwith "Could not find correct operator swap"

let input =
    System.IO.File.ReadLines("./8-input.txt")

let program =
    input |> Seq.map parseInstruction |> Seq.toArray

match run program with
| Error value -> printfn "8a: %d" value
| Ok _ -> failwith "8a: failed to find loop"

printfn "8b: %d" (changeOpUntilProgramWorks program)

