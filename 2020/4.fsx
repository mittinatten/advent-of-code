//let input = """eyr:1972 cid:100
//hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
//
//iyr:2019
//hcl:#602927 eyr:1967 hgt:170cm
//ecl:grn pid:012533040 byr:1946
//
//hcl:dab227 iyr:2012
//ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
//
//hgt:59cm ecl:zzz
//eyr:2038 hcl:74454a iyr:2023
//pid:3556412378 byr:2007
//
//pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
//hcl:#623a2f
//
//eyr:2029 ecl:blu cid:129 byr:1989
//iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
//
//hcl:#888785
//hgt:164cm byr:2001 iyr:2015 cid:88
//pid:545766238 ecl:hzl
//eyr:2022
//
//iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
//"""

open System.Text.RegularExpressions

let requiredFields =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]

let isYearValid (min: int) (max: int) (year: string) =
    let value = int year
    value >= min && value <= max

let isValidHeight (height: string) =
    if (Regex.IsMatch(height, "^\d\d\dcm$")) then
        let h = int height.[0..2]
        h >= 150 && h <= 193
    elif (Regex.IsMatch(height, "^\d\din$")) then
        let h = int height.[0..1]
        h >= 59 && h <= 76
    else
        false

let getPassportFields (passport: string) =
    passport.Split(" ")
    |> List.ofArray
    |> List.filter (fun entry -> entry.Length > 0)
    |> List.map (fun field -> (field.Split(":")))

let hasRequiredFields passport =
    let fields = getPassportFields passport

    let fieldTypes =
        fields |> List.map (fun field -> field.[0])

    let hasField fieldType =
        List.exists (fun field -> field = fieldType) fieldTypes

    requiredFields
    |> List.fold (fun valid fieldType -> (hasField fieldType) && valid) true

let isPassportValid (passport: string) =
    let fields = getPassportFields passport

    let isFieldValid (field: string[]) =
        let fieldType = field.[0]
        let value = field.[1]
        match fieldType with
        | "byr" -> isYearValid 1920 2002 value
        | "iyr" -> isYearValid 2010 2020 value
        | "eyr" -> isYearValid 2020 2030 value
        | "hgt" -> isValidHeight value
        | "hcl" -> Regex.IsMatch(value, "^#[0-9a-f]{6}")
        | "ecl" -> Regex.IsMatch(value, "^(amb|blu|brn|gry|grn|hzl|oth)$")
        | "pid" -> Regex.IsMatch(value, "^\d{9}$")
        | _ -> true

    let allFieldsValid =
        fields |> List.map isFieldValid |> List.fold (&&) true

    hasRequiredFields passport && allFieldsValid

let input =
    System.IO.File.ReadAllText("./4-input.txt")

let passports =
    input.Split "\n\n"
    |> List.ofArray
    |> List.map (fun entry -> entry.Replace("\n", " "))

printfn
    "4a: %A"
    (List.filter hasRequiredFields passports
     |> List.length)

printf "4b: %A" ((List.filter isPassportValid passports) |> List.length)
