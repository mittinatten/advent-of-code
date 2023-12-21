type Password = string
type PasswordPolicy = int * int * char
type PasswordWithPolicy = Password * PasswordPolicy

let parsePasswordLine (line: string) =
    let parts = line.Split ": "
    let password = parts.[1]
    let policyParts = parts.[0].Split(" ")
    let rangeParts = policyParts.[0].Split("-")

    let policy =
        (int rangeParts.[0], int rangeParts.[1], policyParts.[1].[0])

    (password, policy)

let isPasswordValid1 (wPolicy: PasswordWithPolicy) =
    let (pwd, policy) = wPolicy
    let pwdChars = Seq.toList pwd
    let (min, max, character) = policy

    let charCount =
        List.fold (fun count c -> if c = character then (count + 1) else count) 0 pwdChars

    charCount >= min && charCount <= max

let isPasswordValid2 (wPolicy: PasswordWithPolicy) =
    let (pwd, policy) = wPolicy
    let (first, second, character) = policy

    (pwd.Chars(first - 1) = character)
    <> (pwd.Chars(second - 1) = character)

let input =
    System.IO.File.ReadLines("./2-input.txt")
    |> Seq.toList

let numberValidPasswords1 =
    List.map parsePasswordLine input
    |> List.filter isPasswordValid1
    |> List.length

let numberValidPasswords2 =
    List.map parsePasswordLine input
    |> List.filter isPasswordValid2
    |> List.length

printfn "%d" numberValidPasswords1
printfn "%d" numberValidPasswords2
