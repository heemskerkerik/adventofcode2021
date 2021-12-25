open System.Collections.Generic
open FParsec

type Variable = Variable of int

type Argument =
    | Variable of Variable
    | Literal of int

type Instruction =
    | Input of Variable
    | Add of Variable * Argument
    | Multiply of Variable * Argument
    | Divide of Variable * Argument
    | Modulo of Variable * Argument
    | Equals of Variable * Argument

type State = { Variables: int64 [] }

type Parser<'t> = Parser<'t, unit>

let parseVariable =
    choice [ (pchar 'w' >>% Variable.Variable(0))
             (pchar 'x' >>% Variable.Variable(1))
             (pchar 'y' >>% Variable.Variable(2))
             (pchar 'z' >>% Variable.Variable(3)) ]

let parseLiteral = pint32 |>> Literal

let parseArgument =
    (attempt parseLiteral)
    <|> (attempt parseVariable |>> Variable)

let parseInput =
    attempt (skipString "inp " >>. parseVariable)
    |>> Input

let parseRegular str instr =
    attempt (
        (skipString (str + " ") >>. parseVariable)
        .>>. (spaces1 >>. parseArgument)
    )
    |>> instr

let parseInstruction =
    choice [ parseInput
             parseRegular "add" Add
             parseRegular "mul" Multiply
             parseRegular "div" Divide
             parseRegular "mod" Modulo
             parseRegular "eql" Equals ]

let parseProgram: Parser<_> = sepEndBy1 parseInstruction newline

let setVariable state variable value =
    let (Variable.Variable v) = variable
    Array.set state.Variables v value

let getVariable state variable =
    let (Variable.Variable v) = variable
    state.Variables[v]

let getValue state argument =
    match argument with
    | Variable v -> getVariable state v
    | Literal l -> l

let executeInput state variable value = setVariable state variable value

let executeRegular state variable argument f =
    let argument = getValue state argument
    let result = f (getVariable state variable) argument

    setVariable state variable result

let execute state instruction =
    match instruction with
    | Add (v, a) -> executeRegular state v a (+)
    | Multiply (v, a) -> executeRegular state v a (*)
    | Divide (v, a) -> executeRegular state v a (/)
    | Modulo (v, a) -> executeRegular state v a (%)
    | Equals (v, a) -> executeRegular state v a (fun v a -> if v = a then 1 else 0)
    | _ -> failwithf $"Unexpected instruction %A{instruction}"

let rec apply program (states: List<State * (int64 * int64)>) =
    match program with
    | [] -> states
    | Input v :: tail ->
        let mutable newStates: List<State * (int64 * int64)> = List()
        let indices: Dictionary<State, int> = Dictionary()

        for s, (minS, maxS) in states do
            for n in 1L..9L do
                let sNew, (minNew, maxNew) =
                    { s with Variables = Array.copy s.Variables },
                    (minS * 10L + n, maxS * 10L + n)
                executeInput sNew v n

                let exists, index = indices.TryGetValue(sNew)

                if exists then
                    let _, (minExist, maxExist) = newStates[index]
                    newStates[index] <- (sNew, (min minExist minNew, max maxExist maxNew))
                else
                    indices.Add(sNew, newStates.Count)
                    newStates.Add(sNew, (minNew, maxNew))

        printfn $"Processing %d{newStates.Count} states"
        apply tail newStates
    | i :: tail ->
        Seq.iter (fun (s, _) -> execute s i) states
        apply tail states

let program =
    match run parseProgram Input.realInput with
    | Success (v, _, _) -> v
    | Failure (s, _, _) -> failwith s

let emptyState = { Variables = [| 0; 0; 0; 0 |] }

let initialStates = List()
initialStates.Add(emptyState, (0L, 0L))
let states = apply program initialStates

let highest, lowest =
    states |> Seq.filter (fun (s, _) -> s.Variables[3] = 0L) |> Seq.map (fun (_, (min, _)) -> min) |> Seq.min,
    states |> Seq.filter (fun (s, _) -> s.Variables[3] = 0L) |> Seq.map (fun (_, (_, max)) -> max) |> Seq.max
printfn $"Highest: %d{highest}"
printfn $"Lowest: %d{lowest}"