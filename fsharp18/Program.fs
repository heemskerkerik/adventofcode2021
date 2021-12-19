open System
open FParsec

let input = Input.realInput

type Node =
    | Leaf of int
    | Pair of Node * Node

type Parser<'t> = Parser<'t, unit>

let element, elementImpl = createParserForwardedToRef ()

let num: Parser<_> = pint32 |>> Leaf
let pair: Parser<_> =
    between (skipChar '[') (skipChar ']') (element .>>. (skipChar ',' >>. element)) |>> Pair
do elementImpl.Value <- (attempt pair <|> num)
let pairs =
    sepEndBy1 pair newline

let parse s =
    match run pairs s with
    | Success(v, _, _) -> v
    | Failure(s, _, _) -> failwith s

let add a b =
    Pair(a, b)

let rec addLeft n v =
    match n with
    | Leaf i -> Leaf(i + v)
    | Pair (l, r) -> Pair(addLeft l v, r)

let rec addRight n v =
    match n with
    | Leaf i -> Leaf(i + v)
    | Pair (l, r) -> Pair(l, addRight r v)

let rec explode node depth =
    match node with
    | Leaf l -> node, None
    | Pair (Leaf l, Leaf r) when depth > 3 -> Leaf 0, Some(l, r)
    | Pair (l, r) ->
        let l, r, carryL =
            match explode l (depth + 1) with
            | l, Some (carryL, carryR) -> l, addLeft r carryR, carryL
            | _ -> l, r, 0

        let l, r, carryR =
            match explode r (depth + 1) with
            | r, Some (carryL, carryR) -> addRight l carryL, r, carryR
            | _ -> l, r, 0

        (Pair(l, r), Some(carryL, carryR))

let rec split node =
    match node with
    | Leaf n when n >= 10 ->
        let left = Math.Round((n |> float) / 2.0, MidpointRounding.ToZero) |> int
        let right = Math.Round((n |> float) / 2.0, MidpointRounding.AwayFromZero) |> int

        Pair(Leaf(left), Leaf(right)), true
    | Leaf _ -> node, false
    | Pair (l, r) ->
        match split l with
        | splitLeft, true -> Pair(splitLeft, r), true
        | _ ->
            let splitRight, didSplit = split r
            Pair(l, splitRight), didSplit

let rec reduce node =
    match explode node 0 |> fst |> split with
    | afterSplit, true -> reduce afterSplit
    | notSplit, false -> notSplit

let rec magnitude n =
    match n with
    | Leaf v -> v
    | Pair(l, r) ->
        (magnitude l) * 3 + (magnitude r) * 2

let reduceAll list =
    list
    |> List.reduce (fun a b -> add a b |> reduce)

let nums = parse input
let allMagnitude =
    reduceAll nums |> magnitude

printfn $"Sum magnitude: %d{allMagnitude}"

let permutePairs list =
    list
    |> Seq.collect (fun a -> list |> Seq.except (Seq.singleton a) |> Seq.map (fun b -> (a, b)))

let largestMagnitude =
    permutePairs nums
    |> Seq.map (fun (a, b) -> magnitude (reduce (add a b)))
    |> Seq.max

printfn $"Largest magnitude: %d{largestMagnitude}"