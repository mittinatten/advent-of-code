let getNextNumber (currentNumber, seenNumbers: Map<int, int>) turn =
    if (seenNumbers.ContainsKey currentNumber) then
        let newNumber = turn - seenNumbers.[currentNumber]
        (newNumber, seenNumbers.Change(currentNumber, (fun n -> Some turn)))
    else
        (0, seenNumbers.Add(currentNumber, turn))

let seed startNumbers =
    startNumbers
    |> List.mapi (fun i n -> (n, (i + 1)))
    |> Map

let getNumber input targetRound =
    let n = input.Length
    [ n .. (targetRound - 1) ]
    |> Seq.fold getNextNumber (List.last input, seed input.[0..(n - 2)])
    |> fst

let input = [ 13; 16; 0; 12; 15; 1 ]

printfn "15a: %d" (getNumber input 2020)
printfn "15b: %d" (getNumber input 30000000)
