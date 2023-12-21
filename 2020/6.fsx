type Item = char
type CustomsDeclaration = Set<Item>
type GroupDeclaration = Set<Item>
type DeclarationMerger = CustomsDeclaration list -> GroupDeclaration

let union (declarations: CustomsDeclaration list): GroupDeclaration =
    declarations |> List.fold (+) (GroupDeclaration [])

let intersection (declarations: CustomsDeclaration list): GroupDeclaration =
    declarations.Tail |> Seq.fold Set.intersect (GroupDeclaration declarations.Head)

let parseDeclaration (input: string): CustomsDeclaration =
    CustomsDeclaration (Seq.toList input)

let sumDeclarationsItems (input: string) (merge: DeclarationMerger) =
    input.Split "\n\n"
             |> Seq.map (fun group -> (group.Split "\n" |> Seq.toList |> List.map parseDeclaration |> merge))
             |> Seq.map (fun group -> group.Count)
             |> Seq.sum

let input = System.IO.File.ReadAllText("./6-input.txt")

printfn "6a: %d" (sumDeclarationsItems input union)
printfn "6b: %d" (sumDeclarationsItems input intersection)
