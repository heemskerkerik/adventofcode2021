open System.Collections.Generic
open FParsec

let input = Input.realInput

type Parser<'t> = Parser<'t, unit>
type CubeState =
    | On | Off
type Cuboid = Cuboid of x: (int * int) * y: (int * int) * z: (int * int)
type Step = {
    State: CubeState
    Range: Cuboid
}

let prange: Parser<_> =
    pint32 .>>. (skipString ".." >>. pint32)

let pstate: Parser<_> =
    (attempt (pstring "on") >>% On) <|> (attempt (pstring "off") >>% Off)

let pstep: Parser<_> =
    pipe4 (pstate .>> spaces1)
          (skipString "x=" >>. prange)
          (skipString ",y=" >>. prange)
          (skipString ",z=" >>. prange)
          (fun s x y z -> { State = s; Range = Cuboid(x, y, z) })

let psteps =
    sepEndBy1 pstep newline

let steps =
    match run psteps input with
    | Success(v, _, _) -> v
    | Failure(s, _, _) -> failwith s

let limit = (-50, 50)

let intersectsLimit r =
    let lMin, lMax = limit
    let rMin, rMax = r
    rMax >= lMin && rMin <= lMax
let rec clampComponent c =
    let lMin, lMax = limit
    let cMin, cMax = c
    (max cMin lMin, min cMax lMax)

let rec clamp (range: Cuboid) =
    let (Cuboid (x, y, z)) = range

    if not (intersectsLimit x) || not (intersectsLimit y) || not (intersectsLimit z) then
        None
    else
        Some(Cuboid(clampComponent x, clampComponent y, clampComponent z))

let clampedSteps =
    steps
    |> List.map (fun s -> {| State = s.State; Range = (clamp s.Range) |})
    |> List.choose (fun s -> Option.map (fun r -> { State = s.State; Range = r }) s.Range)

let allVecsForRange (range: Cuboid) = seq {
    let (Cuboid ((xMin, xMax), (yMin, yMax), (zMin, zMax))) = range

    for x in xMin..(xMax - 1) do
        for y in yMin..(yMax - 1) do
            for z in zMin..(zMax - 1) do
                yield (x, y, z)
}

let stateFuns: Map<CubeState, int * int * int -> Set<int * int * int> -> Set<int * int * int>> =
    [(On, Set.add); (Off, Set.remove)] |> Map

//let cubes =
//    clampedSteps
//    |> Seq.collect (fun s -> allVecsForRange s.Range |> Seq.map (fun v -> {| State = s.State; Point = v |}))
//    |> Seq.fold (fun s c -> stateFuns[c.State] c.Point s) Set.empty
//
//printfn $"After applying all the steps, %d{Set.count cubes} cubes are on"

let xDimension r =
    let (Cuboid ((xMin, xMax), _, _)) = r
    xMin, xMax + 1

let yDimension r =
    let (Cuboid (_, (yMin, yMax), _)) = r
    yMin, yMax + 1

let zDimension r =
    let (Cuboid (_, _, (zMin, zMax))) = r
    zMin, zMax + 1

let collectDimension dim l =
    l
    |> List.map (fun s -> dim s.Range)
    |> List.collect (fun (a, b) -> [a; b])
    |> Set

let expand points =
    let indices = points |> Seq.sort |> Seq.mapi (fun i x -> (x, i)) |> Map
    let distances = points |> Seq.sort |> Seq.pairwise |> Seq.mapi (fun i (a, b) -> (i, (b - a) |> int64)) |> Map

    (indices, distances)

let solve step1 =
    let allX = collectDimension xDimension steps |> Set.add -50 |> Set.add 51
    let allY = collectDimension yDimension steps |> Set.add -50 |> Set.add 51
    let allZ = collectDimension zDimension steps |> Set.add -50 |> Set.add 51

    let expandedX, xDistances = expand allX
    let expandedY, yDistances = expand allY
    let expandedZ, zDistances = expand allZ

    let cuboidIndices c =
        let (Cuboid ((xMin, xMax), (yMin, yMax), (zMin, zMax))) = c
        Cuboid((expandedX[xMin], expandedX[xMax + 1]), (expandedY[yMin], expandedY[yMax + 1]), (expandedZ[zMin], expandedZ[zMax + 1]))

    let steps = if step1 then clampedSteps else steps

    let indices = HashSet<int * int * int>(200_000_000)

    for i, s in steps |> Seq.indexed do
        for x, y, z in allVecsForRange (cuboidIndices s.Range) do
            if s.State = On then
                indices.Add((x, y, z)) |> ignore
            else
                indices.Remove((x, y, z)) |> ignore
        printfn $"%d{i + 1} / %d{Seq.length steps} %d{indices.Count}"

    let answer =
        Seq.sumBy (fun (x, y, z) -> xDistances[x] * yDistances[y] * zDistances[z]) indices

    answer

printfn $"Step 1: %d{solve true}"
printfn $"Step 2: %d{solve false}"