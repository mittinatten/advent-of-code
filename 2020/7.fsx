open System.Text.RegularExpressions

type Bag = string
type BagItself = unit
type Rule = Bag * ((int * Bag) list)
type BagRules = Rule list

let countContainingBags (rules: BagRules) (bag: Bag): int =
    let getDirectContainers bag =
        rules
        |> List.map (fun (outer, inner) -> (outer, inner |> List.map snd))
        |> List.filter (fun (_, inner) -> List.contains bag inner)
        |> List.map fst
        |> Set.ofList

    let rec findContainers prevContainers =
        let containers =
            prevContainers
            |> Set.toList
            |> List.map getDirectContainers
            |> List.fold (+) prevContainers

        if (containers.Count > prevContainers.Count)
        then findContainers containers
        else containers

    (findContainers (getDirectContainers bag)).Count

let rec countContainedBags (rules: BagRules) (bag: Bag): int =
    let containedBags =
        rules
        |> List.find (fun (outer, _) -> outer = bag)
        |> snd

    let numberContainedBags = containedBags |> List.map fst |> List.fold (+) 0

    List.fold (fun sum (count, container) ->
            if count > 0
            then sum + count * countContainedBags rules container
            else sum) numberContainedBags containedBags

let parseRule (rule: string) =
    let containerAndBags = rule.Split(" bags contain ")
    let container = containerAndBags.[0]

    let bags =
        containerAndBags.[1].Split(", ")
        |> List.ofArray
        |> List.map (fun s -> Regex.Match(s, "^(\d+|no) (.*)$"))
        |> List.map (fun matches ->
            match (matches.Groups.[1].Value, matches.Groups.[2].Value) with
            | ("no", color) -> (0, string color)
            | (count, color) -> (int count, string color))
        |> List.map (fun (count, color) -> (count, Regex.Replace(color, " bags?\.?$", "")))

    Rule(container, bags)

let input = System.IO.File.ReadLines("./7-input.txt") |> Seq.toList
let rules = input |> List.map parseRule

printfn "7a: %d" (countContainingBags rules "shiny gold")

printfn "7b: %d" (countContainedBags rules "shiny gold")
