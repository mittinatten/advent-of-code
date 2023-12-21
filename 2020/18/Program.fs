open System
open Expecto

type Token =
    | Plus
    | Times
    | Number of int64
    | StartParen
    | EndParen

let getNextToken (prev: Token option) (c: char): Token option =
    match c with
    | '(' -> Some StartParen
    | ')' -> Some EndParen
    | '+' -> Some Plus
    | '*' -> Some Times
    | d when d >= '0' && d <= '9' ->
        let digit = int64 (string d)
        match prev with
        | Some (Number n) ->
            Some(Number(digit + n * 10L))
        | _ -> Some(Number(int64 digit))
    | ' ' -> None
    | c -> failwith ("unrecognized character: " + (string c))

let tokenize (input: string): Token List =
    let rec tokenize (tokens: Token list) (input: char list) =
        if input.Length = 0 then
            tokens
        else
            let prev =
                if tokens.Length = 0 then None else Some tokens.Head

            let nextToken = getNextToken prev input.Head
            if nextToken = None
            then tokenize tokens input.Tail
            else match prev with
                    | (Some (Number n)) ->
                        match nextToken with // turns out we don't actually need to handle mulit digit number, but here it is
                        | (Some (Number n))-> tokenize (nextToken.Value :: tokens.Tail) input.Tail
                        | _ -> tokenize (nextToken.Value :: tokens) input.Tail
                    | _ -> tokenize (nextToken.Value :: tokens) input.Tail


    input |> List.ofSeq |> tokenize [] |> List.rev

let evaluate (input: string): int64 =
    let rec evaluate result (prevOp: Token option) (tokens: Token list): (int64 * Token List) =
        match tokens with
        | [] -> (result, [])
        | t :: ts ->
            match t with
            | Number n ->
                match prevOp with
                | None -> evaluate n None ts
                | Some Plus -> evaluate (n + result) None ts
                | Some Times -> evaluate (n * result) None ts
                | _ -> failwith "syntax error"
            | Plus -> evaluate result (Some Plus) ts
            | Times -> evaluate result (Some Times) ts
            | EndParen -> (result, ts)
            | StartParen ->
                match prevOp with
                | None ->
                    evaluate 0L None ts
                    |> fun (subResult, remaining) -> evaluate subResult None remaining
                | Some Plus ->
                    evaluate 0L None ts
                    |> fun (subResult, remaining) -> evaluate (result + subResult) None remaining
                | Some Times ->
                    evaluate 0L None ts
                    |> fun (subResult, remaining) -> evaluate (result * subResult) None remaining

                | _ -> failwith "syntax error"


    let tokens = tokenize input
    evaluate 0L None tokens |> fst

let simpleTests =
    testList "Simple" [
        testCase "no paranthesis" <| fun () ->
            Expect.equal (evaluate "2 + 3 * 4") 20L ""
        testCase "several digit number" <| fun () ->
            Expect.equal (evaluate "101 + 32 + 1") 134L ""
        testCase "simple paranthesis" <| fun () ->
            Expect.equal (evaluate "2 + 3 + (4 * 2)") 13L ""
        testCase "neighboring parantheses" <| fun () ->
            Expect.equal (evaluate "(2 + 3) * (4 + 2)") 30L ""
        testCase "nested neighboring parantheses" <| fun () ->
            Expect.equal (evaluate "((2 + 3) * (4 + 2) + 1)") 31L ""
    ]

let exampleTests =
    testList "Examples" [
        testCase "first" <| fun () ->
            Expect.equal (evaluate "2 * 3 + (4 * 5)") 26L "Should evaluate correctly"
        testCase "second" <| fun () ->
            Expect.equal (evaluate "5 + (8 * 3 + 9 + 3 * 4 * 3)") 437L "Should evaluate correctly"
        testCase "third" <| fun () ->
            Expect.equal (evaluate "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") 12240L "Should evaluate correctly"
        testCase "fourth" <| fun () ->
            Expect.equal (evaluate "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") 13632L   "Should evaluate correctly"
    ]

[<EntryPoint>]
let main argv =
    let test1 = runTestsWithCLIArgs [] argv simpleTests
    let test2 = runTestsWithCLIArgs [] argv exampleTests

    if test1 <> 0 || test2 <> 0 then 1
    else
        let input = System.IO.File.ReadLines("input.txt")
        let result1 = input |> Seq.sumBy evaluate
        printfn "18a: %d" result1
        0
