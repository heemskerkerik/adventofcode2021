open System.Collections.Generic

type Point = int * int

type Cell =
    | EastFacingCucumber
    | SouthFacingCucumber
let parse (input: string) =
    let lines = input.Split('\n')

    lines
    |> Array.mapi
        (fun y l ->
            l.ToCharArray()
            |> Array.mapi
                (fun x c ->
                    match c with
                    | '>' -> Some(((x, y), EastFacingCucumber))
                    | 'v' -> Some(((x, y), SouthFacingCucumber))
                    | _ -> None))
    |> Array.collect id
    |> Array.choose id
    |> Map
    |> Dictionary

let input = Input.realInput

let map = parse input

let width = input.Split('\n').[0].Length
let height = input.Split('\n').Length

let addX ((x, y): Point) =
    ((x + 1) % width, y)
let addY ((x, y): Point) =
    (x, (y + 1) % height)

let step map =
    let move cellType movement (map: Dictionary<Point, Cell>) =
        let moves =
            map
            |> Seq.filter (fun c -> c.Value = cellType && (map.ContainsKey(movement c.Key) |> not))
            |> Seq.map (fun c -> c.Key)
            |> Array.ofSeq

        for p in moves do
            map.Remove(p) |> ignore
            map.Add(movement p, cellType)

        moves.Length > 0

    let moveEastFacing =
        move EastFacingCucumber addX

    let moveSouthFacing =
        move SouthFacingCucumber addY

    let movedEastFacing = moveEastFacing map
    let movedSouthFacing = moveSouthFacing map
    movedEastFacing || movedSouthFacing

let print (map: Dictionary<Point, Cell>) =
    for y in 0..(height - 1) do
        for x in 0..(width - 1) do
            let c =
                match map.TryGetValue((x, y)) with
                | true, EastFacingCucumber -> '>'
                | true, SouthFacingCucumber -> 'v'
                | false, _ -> '.'

            printf $"%c{c}"
        printfn ""

let rec stepUntilMovementStops map n =
    let moved = step map

    if not moved then
        (n, map)
    else
        if n % 100 = 0 then
            printfn $"Step %d{n}"
        stepUntilMovementStops map (n + 1)

let rec stepNTimes map n =
    match n with
    | 0 -> map
    | _ ->
        step map |> ignore
        stepNTimes map (n - 1)

let lastMoveStep, finalMap = stepUntilMovementStops (map |> Dictionary) 0
printfn $"First move in which cucumbers no longer move: %d{lastMoveStep + 1}"