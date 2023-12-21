type XMASMessage = XMASMessage of int64 list

let parseMessage (input: string list) = input |> List.map int64 |> XMASMessage

let validateMessage preambleSize (XMASMessage message) =
    let windowValid (wdw: int64 list) =
        let indexedPreamble =
            wdw.[..(wdw.Length - 2)]
            |> List.mapi (fun i p -> (i, p))

        let value = List.last wdw
        indexedPreamble
        |> List.exists (fun (i, pi) ->
            indexedPreamble.[(i + 1)..]
            |> List.exists (fun (_, pj) -> pi + pj = value))

    message
    |> List.windowed (preambleSize + 1)
    |> List.map (fun wdw -> (List.last wdw, windowValid wdw))
    |> List.tryPick (fun (value, valid) ->
        match valid with
        | false -> Some value
        | _ -> None)
    |> fun result ->
        match result with
        | Some value -> Error value
        | None -> Ok "Valid"

let findEncryptionWeakness (invalidNumber) (XMASMessage originalMessage) =
    let rec scan sum (elements: int64 list) (message: int64 list) =
        if sum = invalidNumber then Some elements
        elif sum > invalidNumber then None
        elif message.Length = 0 then None
        else scan (sum + message.Head) (message.Head :: elements) message.Tail

    let rec findContiguousElements (subMessage: int64 list) =
        match scan 0L [] subMessage with
        | Some elements -> elements
        | None -> findContiguousElements subMessage.Tail

    let elements = findContiguousElements originalMessage
    List.min elements + List.max elements

let input =
    System.IO.File.ReadLines("./9-input.txt")


let message = input |> List.ofSeq |> parseMessage

let stopWatch = System.Diagnostics.Stopwatch()
stopWatch.Start()
let invalidNumber =
    message
    |> validateMessage 25
    |> fun v ->
        match v with
        | Error v -> v
        | Ok _ -> failwith "could not find invalid number"

printfn "9a: %A" invalidNumber
printfn "9b: %d" (findEncryptionWeakness invalidNumber message)

stopWatch.Stop()
printf "Stopwatch %A" stopWatch.Elapsed
