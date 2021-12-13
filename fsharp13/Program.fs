open FParsec

let input = Input.realInput

type Point = int * int
type FoldDirection = X | Y
type Parser<'t> = Parser<'t, unit>

let parsePoint: Parser<_> =
    ((pint32 .>> pchar ',') .>>. pint32)

let parsePoints =
    many (attempt parsePoint .>> newline)

let parseFoldDirection =
    (pchar 'x' >>% X) <|> (pchar 'y' >>% Y)

let parseFoldInstruction =
    pstring "fold along " >>. parseFoldDirection .>>. (pchar '=' >>. pint32)

let parseFoldInstructions =
    sepEndBy1 parseFoldInstruction newline

let parseAll =
    parsePoints .>> newline .>>. parseFoldInstructions

let parse input =
    match run parseAll input with
    | Success((points, folds), _, _) -> (points, folds)
    | Failure(error, _, _) -> failwith error

let rawPoints, folds = parse input

let map = rawPoints |> Set

let printMap map =
    let maxX = map |> List.ofSeq |> List.maxBy fst |> fst
    let maxY = map |> List.ofSeq |> List.maxBy snd |> snd

    for y in 0..maxY do
        for x in 0..maxX do
            let c = if Set.contains (x, y) map then '█' else '‧'
            printf $"%c{c}"
        printfn ""

let fold map (direction, line) =
    map
    |> Set.map (fun (x, y) ->
                    match direction with
                    | X -> if x < line then (x, y) else ((line * 2) - x, y)
                    | Y -> if y < line then (x, y) else (x, (line * 2) - y))

let mapAfterFirstFold = fold map folds.[0]
printfn $"After first fold, %d{mapAfterFirstFold |> Set.count} points are visible"

let fullyFolded =
    folds
    |> List.fold fold map

printMap fullyFolded