open System.Diagnostics

let inputLines = System.IO.File.ReadLines("./1-input.txt")
let filteredExpenses = inputLines |> Seq.map int |> Seq.toList

let get2Product (termList: int list) targetSum: Option<int>  =
    let checkIfTargetTerm fixedTerm termToCheck =
        match termToCheck with
        | value when (value + fixedTerm = targetSum) -> Some value
        | _ -> None

    let rec inner term terms =
        match terms with
        | [] -> None
        | _ ->
            let otherTerm = List.tryPick (checkIfTargetTerm term) terms
            match otherTerm with
                | Some value -> Some (term * value)
                | None -> inner terms.Head terms.Tail

    inner termList.Head termList.Tail

let get3Product (termList: int list) targetSum: Option<int>  =
    let rec inner term terms =
        match terms with
        | [] -> None
        | _ ->
            let twoProduct = get2Product terms (targetSum - term)
            match twoProduct with
                | Some value -> Some (term * value)
                | None -> inner terms.Head terms.Tail

    inner termList.Head termList.Tail

let stopWatch = Stopwatch()
stopWatch.Start()

printfn "%A" (get2Product filteredExpenses 2020)
printfn "%A" (get3Product filteredExpenses 2020)

stopWatch.Stop()
printf "Stopwatch %A" stopWatch.Elapsed
