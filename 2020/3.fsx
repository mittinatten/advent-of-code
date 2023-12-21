type SlopeItem =
    | Tree
    | NoTree

type Slope = SlopeItem[][]
type Stride = int * int

let parseSlope (input: string list) =
    let parseSlopeLine (line: string) =
        Seq.toList line
        |> Seq.map (fun c ->
            match c with
            | '#' -> Tree
            | _ -> NoTree)
        |> Seq.toArray

    Seq.map parseSlopeLine input |> Seq.toArray

let traverseSlope (slope: Slope) (stride: Stride): int =
    let (right, down) = stride

    let rec stepForward count x y =
        let newY = y + down
        if (newY >= slope.Length) then
            count
        else
            let newX = (x + right) % slope.[newY].Length
            match slope.[newY].[newX] with
            | Tree -> stepForward (count + 1) newX newY
            | NoTree -> stepForward count newX newY

    stepForward 0 0 0

let treeCountProduct (slope: Slope) (strides: Stride list) =
    List.map (traverseSlope slope) strides
    |> List.fold (*) 1

let input =
    System.IO.File.ReadLines("./3-input.txt")
    |> Seq.toList

let slope = parseSlope input

printfn "3a: %d" (treeCountProduct slope [ (3, 1) ])
printfn "3b: %d" (treeCountProduct slope [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ])
