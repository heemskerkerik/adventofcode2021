let input = Input.realInput

let rec processLine l (s: char list) =
    if l = "" then
        (None, s)
    else
        let c = l.[0]
        match c with
        | '(' | '[' | '<' | '{' -> processLine l.[1..] ([c] @ s)
        | ')' | ']' | '>' | '}' ->
            let popped = List.head s
            let newStack = List.tail s

            let isValid = match (popped, c) with
                          | '(', ')' -> true
                          | '[', ']' -> true
                          | '<', '>' -> true
                          | '{', '}' -> true
                          | _ -> false

            if isValid then
                processLine l.[1..] newStack
            else
                (Some(c), newStack)
        | _ -> failwithf $"Unexpected character %c{c}"

let getIllegalCharacter l: char option =
    let illegalChar, _ = processLine l []
    illegalChar

let scoreIllegalCharacter c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwithf $"Unexpected character %c{c}"

let getCharactersToComplete l: char list option =
    let getCompletingCharacter c =
        match c with
        | '(' -> ')'
        | '[' -> ']'
        | '<' -> '>'
        | '{' -> '}'
        | _ -> failwithf $"Unexpected character %c{c}"

    let illegalChar, stack = processLine l []

    match illegalChar with
    | Some _ -> None
    | None -> Some(List.rev stack |> List.map getCompletingCharacter)

let illegalCharacters =
    input |> Array.choose getIllegalCharacter

let illegalSyntaxScore =
    illegalCharacters |> Array.sumBy scoreIllegalCharacter
printfn $"Illegal syntax score: %d{illegalSyntaxScore}"

let getCompletingCharacterScore c =
    match c with
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L
    | _ -> failwithf $"Unexpected character %c{c}"

let completionScore chars =
    chars
    |> List.fold (fun score c -> score * 5L + (getCompletingCharacterScore c)) 0L

let completingCharacters =
    input
    |> Array.map getCharactersToComplete
    |> Array.choose id

let scores =
    completingCharacters
    |> Array.map completionScore
    |> Array.sort
let middleScore = scores.[Array.length scores / 2]

printfn $"Middle score: %d{middleScore}"