open System.Collections.Generic
open FParsec

let input = Input.realInput

type Parser<'t> = Parser<'t, unit>

let range: Parser<_> =
    pint32 .>>. (pstring ".." >>. pint32)

let parseTargetArea: Parser<_> =
    ((skipString "target area: x=" >>. range) .>>. (skipString ", y=" >>. range))

type Point = int * int
type Box = Point * Point
let initial: Point = (0, 0)

let targetArea: Box =
    match run parseTargetArea input with
    | Success(v, _, _) -> v
    | Failure(s, _, _) -> failwith s

let intersects (point: Point) (box: Box) =
    let pX, pY = point
    let (x1, x2), (y1, y2) = box

    pX >= x1 && pX <= x2 && pY >= y1 && pY <= y2

let isBeyond (point: Point) (box: Box) =
    let pX, pY = point
    let (_, x2), (y1, _) = box

    pX > x2 || pY < y1

let rec shoot (probeX, probeY) (velX, velY) =
    let probeX, probeY = probeX + velX, probeY + velY

    if intersects (probeX, probeY) targetArea then
        true
    else if isBeyond (probeX, probeY) targetArea then
        false
    else
        let velX, velY = (max (velX - 1) 0), velY - 1
        shoot (probeX, probeY) (velX, velY)

let rec triangular n: int64 =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> (n |> int64) + triangular (n - 1)

let memoize f =
    let cache = Dictionary<_,_>()

    fun c ->
        let exist, value = cache.TryGetValue(c)

        match exist with
        | true -> value
        | false ->
            let value = f c
            cache.Add(c, value)
            value

let memoizedTriangular = memoize triangular

let (targetAreaX1, targetAreaX2), (targetAreaY1, targetAreaY2) = targetArea

let infiniteSeqStartingAt n =
    Seq.initInfinite ((+) n)

let minVelX =
    infiniteSeqStartingAt 1
    |> Seq.skipWhile (fun x -> (memoizedTriangular x) < targetAreaX1)
    |> Seq.head
let maxVelX =
    infiniteSeqStartingAt minVelX
    |> Seq.takeWhile (fun x -> (memoizedTriangular x) <= targetAreaX2)
    |> Seq.last

let landingShots =
    {1..(-targetAreaY1)}
    |> Seq.collect (fun y -> {minVelX..maxVelX} |> Seq.map (fun x -> (x, y)))
    |> Seq.skipWhile (shoot initial >> not)
    |> Seq.filter (shoot initial)

let maxY = landingShots |> Seq.map snd |> Seq.max |> memoizedTriangular
printfn $"Max Y aiming for target is %d{maxY}"

let velocitiesThatReachTarget =
    {targetAreaY1..(-targetAreaY1)}
    |> Seq.allPairs {minVelX..targetAreaX2}
    |> Seq.filter (shoot initial)
printfn $"Velocities that reach target: %d{Seq.length velocitiesThatReachTarget}"