open FParsec

let input = Input.realInput

type Insertion = (char * char) * char
type Parser<'t> = Parser<'t, unit>

let parseTemplate =
    restOfLine true |>> (fun s -> s.ToCharArray() |> List.ofSeq)

let parseInsertion =
    ((asciiUpper .>>. asciiUpper) .>> pstring " -> ") .>>. asciiUpper

let parseInsertions =
    (sepEndBy1 parseInsertion newline) |>> Map

let parse =
    (parseTemplate .>> newline) .>>. parseInsertions

let template, insertions = match run parse input with
                           | Success(v, _, _) -> v
                           | Failure(error, _, _) -> failwith error

let getMaps template =
    let pairMap = template
                  |> List.pairwise
                  |> List.countBy id
                  |> List.map (fun (key, count) -> (key, count |> int64))
                  |> Map
    let elementMap = template
                     |> List.countBy id
                     |> List.map (fun (key, count) -> (key, count |> int64))
                     |> Map

    (pairMap, elementMap)

let expand (pairs: Map<char * char, int64>, elements) =
    let newPairs =
        pairs
        |> Seq.map (fun pair -> let a, b = pair.Key
                                [((a, insertions.[(a, b)]), pair.Value); ((insertions.[(a, b)], b), pair.Value)])
        |> Seq.collect id
        |> Seq.groupBy fst
        |> Seq.map (fun (key, items) -> (key, items |> Seq.map snd |> Seq.sum))
        |> Map
    let newElements =
        pairs
        |> Seq.map (fun pair -> let a, b = pair.Key
                                (insertions.[(a, b)], pair.Value))
        |> Seq.fold (fun m (c, n) -> Map.change c (Option.defaultValue 0L >> ((+) n) >> Some) m) elements

    (newPairs, newElements)

let rec expandN n (pairs, elements) =
    match n with
    | 0 -> (pairs, elements)
    | _ -> expandN (n - 1) (expand (pairs, elements))

let minMaxCounts elements =
    let min = Map.values elements |> Seq.min
    let max = Map.values elements |> Seq.max

    (min, max)

let pairs, elements = getMaps template

let minMaxAfterN n =
    let _, finalElements = expandN n (pairs, elements)
    let minElement, maxElement = minMaxCounts finalElements

    printfn $"After %d{n} steps: %d{maxElement - minElement}"

minMaxAfterN 10
minMaxAfterN 40

