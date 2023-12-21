let example = """28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"""

let input = // example.Split("\n")
    System.IO.File.ReadLines("./10-input.txt")

let adapters = input |> Seq.map int |> Seq.sort

let highestAdapter = Seq.last adapters

let allAdapters =
    Seq.append (Seq.append [ 0 ] adapters) [ highestAdapter + 3 ]
    |> List.ofSeq

let distribution =
    allAdapters
    |> List.pairwise
    |> List.fold (fun acc (p1, p2) ->
        let d = p2 - p1
        acc
        |> List.mapi (fun i v -> if i + 1 = d then v + 1 else v)) [ 0; 0; 0 ]

let getGaps adapters =
    adapters
    |> List.pairwise
    |> List.map (fun (p1, p2) -> p2 - p1)

let countCombinations gaps =
    gaps
    |> List.pairwise
    |> List.fold (fun compressed p ->
        match p with
        | 1, 1 -> (compressed.Head + 1) :: compressed.Tail
        | 3, 1 -> 1 :: compressed
        | _ -> compressed) [ 1 ]
    |> List.map (fun onesStreak ->
        match onesStreak with
        | 1 -> 1L
        | 2 -> 2L
        | 3 -> 4L
        | 4 -> 7L
        | 5 -> 13L
        | _ -> failwith "don't know how to handle that many 1s")
    |> List.fold (*) 1L

let combinations =
    allAdapters |> getGaps |> countCombinations

printfn "10a: %d" (distribution.[0] * distribution.[2])
printfn "10b: %d" combinations
