let example = """1000434
17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,983,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,397,x,x,x,x,x,37,x,x,x,x,x,x,13
"""

let parseNote (input: string) =
    let parts = input.Split("\n");
    let arrivalTime = int parts.[0];
    let schedule = parts.[1].Split(",") |> Seq.filter (fun bus -> bus <> "x") |> Seq.map int
    (arrivalTime, schedule)

let note = parseNote example
let arrivalTime = fst note
let schedule = snd note
let minWaitTime = schedule |> Seq.map (fun bus -> (bus, bus - arrivalTime % bus)) |> Seq.minBy snd

printfn "%A" minWaitTime
printfn "13a: %d" (fst minWaitTime * snd minWaitTime)
