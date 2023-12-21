type Schedule = int64 option seq

let parseSchedule (input: string): Schedule =
    input.Split(",")
    |> Seq.map (fun entry ->
        match entry with
        | "x" -> None
        | bus -> Some(int64 bus))

let example = "1789,37,47,1889"
//"17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,983,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,397,x,x,x,x,x,37,x,x,x,x,x,x,13"

// [b] = [ 7, 13, 59, 31, 19 ]
// [d] = [ 0,  1,  4,  6,  7 ]
// t_n = b_0 * n
// Find t_n such that (t_n + d_i) % b_i = 0 forall i
let findTimeBrute (schedule: Schedule) =
    let bd =
        schedule
        |> Seq.mapi (fun i entry ->
            match entry with
            | Some v -> Some(v, int64 i)
            | None -> None)
        |> Seq.fold (fun acc pair ->
            match pair with
            | Some p -> p :: acc
            | None -> acc) []
        |> List.sortBy fst
        |> List.rev

    let (b0, d0) = bd.Head

    let rec check tn =
        if (tn % 100000000L = 0L) then printfn "%A" tn

        let isMatch =
            bd
            |> Seq.forall (fun (bi, di) -> (tn + di) % bi = 0L)

        if isMatch then tn else check (tn + b0)

    check (-d0 + (100000000000000L / b0) * b0)

let answer =
    example |> parseSchedule |> findTimeBrute

printfn "%A" (answer)
