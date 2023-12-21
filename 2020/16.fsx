type Description = string
type Range = Range of int * int
type Rule = Rule of Description * Range list
type Ticket = Ticket of int list
type RuleMapping = (int * Rule) list

let parseRule (input: string): Rule =
    let parts = input.Split(": ")
    let description = parts.[0]
    let part2 = parts.[1].Split(" or ")

    let ranges =
        part2
        |> Seq.map (fun r -> r.Split("-"))
        |> Seq.map (fun r -> Range(int r.[0], int r.[1]))
        |> List.ofSeq

    Rule(description, ranges)

let parseTicket (input: string): Ticket =
    input.Split(",")
    |> Seq.map int
    |> List.ofSeq
    |> Ticket

let parseInput (input: string): (Rule list * Ticket * Ticket list) =
    let sections =
        input.Split("\n\n")
        |> Seq.map (fun s -> s.Split("\n"))
        |> List.ofSeq

    let rules =
        sections.[0] |> Seq.map parseRule |> List.ofSeq

    let ticket = sections.[1].[1] |> parseTicket

    let otherTickets =
        sections.[2]
        |> List.ofSeq
        |> List.tail
        |> List.map parseTicket

    (rules, ticket, otherTickets)

let valueInRange (value: int) (Range (min, max)): bool = value >= min && value <= max

let findTicketRuleViolation (rules: Rule list) (Ticket ticket): int option =
    let findViolation (entry: int) (Rule (_, ranges)): bool =
        ranges |> List.exists (valueInRange entry)

    let checkAnyRule (entry: int): int option =
        rules
        |> List.exists (findViolation entry)
        |> fun passes -> if passes then None else Some entry

    ticket
    |> List.fold (fun acc entry ->
        match checkAnyRule entry with
        | None -> acc
        | Some v -> Some v) None

let mapRulesToColumns (rules: Rule list) (tickets: Ticket list): RuleMapping =
    let nColumns = rules.Length // assume the same number of rules as ticket fields
    let columns = [ 0 .. (nColumns - 1) ]

    let findColumnsForRule (Rule (_, ranges)): int list =
        columns
        |> List.filter (fun i ->
            tickets
            |> List.forall (fun (Ticket ticket) -> ranges |> List.exists (valueInRange ticket.[i])))

    let ruleColumns =
        rules
        |> List.map (fun rule -> (rule, findColumnsForRule rule))
        |> List.sortBy (fun (rule, columns) -> columns.Length)

    let rec mapRules (availableColumns: int list) (matchedRules: RuleMapping) (unmatchedRules: (Rule * int list) list) =
        match unmatchedRules with
        | [] -> matchedRules
        | _ ->
            let ruleColumns = snd (unmatchedRules.Head)

            let column =
                ruleColumns
                |> List.find (fun c -> List.contains c availableColumns)

            let rule = fst (unmatchedRules.Head)

            let remainingColumns =
                availableColumns
                |> List.filter (fun c -> c <> column)

            mapRules remainingColumns ((column, rule) :: matchedRules) unmatchedRules.Tail

    mapRules columns [] ruleColumns

let getTicketColumn (Ticket ticket) column = ticket.[column]

let input =
    System.IO.File.ReadAllText("./16-input.txt")

let (rules, myTicket, otherTickets) = parseInput input

let result1 =
    otherTickets
    |> List.map (findTicketRuleViolation rules)
    |> List.filter (fun v -> v.IsSome)
    |> List.map (fun v -> v.Value)
    |> List.fold (fun sum v -> sum + v) 0

let validTickets =
    otherTickets
    |> List.filter (fun ticket ->
        match (findTicketRuleViolation rules ticket) with
        | None -> true
        | Some _ -> false)

let result2 =
    mapRulesToColumns rules validTickets
    |> List.filter (fun (_, (Rule (description, _))) -> description.StartsWith("departure"))
    |> List.map (fun (column, _) -> getTicketColumn myTicket column)
    |> List.map int64
    |> List.fold (*) 1L


printfn "16a: %A" result1
printfn "16b: %A" result2
