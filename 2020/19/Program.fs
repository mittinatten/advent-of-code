open System

type RuleReference = RuleReference of int list
type Rule =
    | Character of char
    | RuleRef of RuleReference list
type RuleSet = RuleSet of Map<int, Rule>
type Message = Message of string

type ValidateMessage = RuleSet -> Message -> bool

let validateMessage: ValidateMessage =
    fun rules message ->
        true

let buildValidStrings (RuleSet rules): string list =
    let rec expand (rule: Rule) =
        match rule with
        | Character c -> [Character c]
        | RuleRef refs ->
            refs
            |> List.map (fun (RuleReference ref) ->
                ref
                |> List.map (fun i -> expand (Map.find i rules)))

    expand (Map.find 0 rules)

[<EntryPoint>]
let main argv =
    0 // return an integer exit code
