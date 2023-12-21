open System
open Expecto

type Edge = Edge of string
type TileNumber = TileNumber of int64
type Tile = TileNumber * Edge list
type EdgeMap = Map<Edge, Set<TileNumber>>

let reverseEdge (Edge edge): Edge =
    edge |> List.ofSeq |> List.rev |> List.fold (fun rev c -> rev + (string c)) "" |> Edge

let rotateTile (tile: Tile): Tile =
    let rotatedEdges = match snd tile with [t;r;b;l] -> [reverseEdge l;t;r;b] | _ -> failwith "wrong number edges"
    (fst tile, rotatedEdges)

let parseTile (unfiltered: string list): Tile =
    let input = unfiltered |> List.filter (fun line -> line <> "")
    let tile = input.Head.Split("Tile ").[1] |> fun t -> t.Split(":").[0] |> int64
    let contents = input.Tail
    let edgeTop = contents |> List.head
    let edgeBottom = List.last contents
    let edgeLeft = contents |> List.fold (fun edge line -> edge + (line |> Seq.head |> string)) ""
    let edgeRight = contents |> List.fold (fun edge line -> edge + (line |> Seq.last |> string)) ""

    Tile (TileNumber tile, [Edge edgeTop; Edge edgeRight; reverseEdge (Edge edgeBottom); reverseEdge (Edge edgeLeft)])

let parseImage (input: string) =
    input.Split("\n\n") |> Seq.map (fun s -> s.Split("\n") |> List.ofSeq |> parseTile)

let genEdgeMap (tiles: Tile seq): EdgeMap =
    tiles
    |> Seq.fold (fun map (number, edges) ->
        edges
        |> List.fold (fun map edge ->
            let revEdge = reverseEdge edge
            if Map.containsKey edge map then
                let prev = Map.find edge map
                Map.add edge (prev.Add number) map
            else if Map.containsKey revEdge map then
                let prev = Map.find revEdge map
                Map.add revEdge (prev.Add number) map
            else
                Map.add edge (Set [number]) map
            ) map)
        (Map<Edge, Set<TileNumber>> [])

let getEdgeCounts (edgeMap: EdgeMap) (tiles: Tile seq) =
    tiles
    |> Seq.map (fun tile ->
        let counts =
            tile
            |> snd
            |> List.map (fun edge ->
                let revEdge = reverseEdge edge
                if Map.containsKey edge edgeMap then
                    Map.find edge edgeMap |> Set.count
                else
                    Map.find revEdge edgeMap |> Set.count)
        (fst tile, counts))

let findCornerTiles tileMatchCounts: TileNumber seq =
    tileMatchCounts
    |> Seq.filter (fun (TileNumber n, count) ->
        match count with
        | [1;1;_;_] -> true
        | [_;1;1;_] -> true
        | [_;_;1;1] -> true
        | [1;_;_;1] -> true
        | _ -> false
    )
    |> Seq.map fst

let getAnswer1 (input: string) =
    let tiles = input |> parseImage
    let edgeMap = genEdgeMap tiles
    let tileMatchCounts = getEdgeCounts edgeMap tiles
    let cornerTiles = findCornerTiles tileMatchCounts
    cornerTiles |> Seq.fold (fun p (TileNumber i) -> i * p) 1L

let parserTest =
    let input = [
        "Tile 123:"
        "#.#"
        "###"
        ".##" ]
    testCase "parseTile" <| fun () ->
        let tile = parseTile input
        let edges = tile |> snd
        Expect.equal (tile |> fst) (TileNumber 123L) "Should extract tile number"
        Expect.equal (edges.[0]) (Edge "#.#") "Should extract top edge"
        Expect.equal (edges.[1]) (Edge "###") "Should extract right edge"
        Expect.equal (edges.[2]) (Edge "##.") "Should extract bottom edge"
        Expect.equal (edges.[3]) (Edge ".##") "Should extract left edge"

let part1Test =
    let example = System.IO.File.ReadAllText("./example.txt")
    testCase "Check example data for part 1" <| fun () ->
        Expect.equal (getAnswer1 example) 20899048083289L "Should get correct answer"

[<EntryPoint>]
let main argv =
    runTestsWithCLIArgs [] argv parserTest
    runTestsWithCLIArgs [] argv part1Test
    let input = System.IO.File.ReadAllText("./input.txt")
    printf "Answer 20a: %d" (getAnswer1 input)
    0
