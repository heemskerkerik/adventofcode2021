let input = Input.realInput

let heights =
    input
    |> Array.map (fun (s: string) -> s.ToCharArray() |> Array.map (fun c -> System.String([| c |]) |> byte))
    |> Array.transpose

let maxColumn = heights.Length - 1
let maxRow = heights.[0].Length - 1

let rec isLowPoint x y =
    let value = heights.[x].[y]

    let isWestHigher =
        x = 0 ||
        (x > 0 && heights.[x - 1].[y] > value)
    let isEastHigher =
        x = maxColumn ||
        (x < maxColumn && heights.[x + 1].[y] > value)
    let isNorthHigher =
        y = 0 ||
        (y > 0 && heights.[x].[y - 1] > value)
    let isSouthHigher =
        y = maxRow ||
        (y < maxRow && heights.[x].[y + 1] > value)

    isWestHigher && isEastHigher && isNorthHigher && isSouthHigher

let getValueAt (x, y) =
    heights.[x].[y]

let lowPoints = seq {
    let rows = heights.[0].Length

    for x in {0..heights.Length - 1} do
        for y in {0..rows - 1} do
            if isLowPoint x y then
                yield (x, y)
}
let riskValues = lowPoints |> Seq.map getValueAt |> Seq.map ((+) 1uy)

let sumOfRiskValues = riskValues |> Seq.map int |> Seq.sum
printfn $"Sum of risk values: %d{sumOfRiskValues}"

let getNeighbors (x, y) =
    let withinBounds (rX, rY) =
        x + rX >= 0
        && x + rX <= maxColumn
        && y + rY >= 0
        && y + rY <= maxRow

    [ (-1, 0)
      (1, 0)
      (0, -1)
      (0, 1) ]
    |> Seq.filter withinBounds
    |> Seq.map (fun (rX, rY) -> (x + rX, y + rY))
    |> List.ofSeq

let rec getBasinCells (x, y): (int * int) list =
    let neighbors = getNeighbors (x, y)
    let value = heights.[x].[y]

    let isOnSlope (neighborX, neighborY) =
        heights.[neighborX].[neighborY] > value && heights.[neighborX].[neighborY] <> 9uy
    let getNeighborAndItsNeighbors (neighborX, neighborY) =
        getBasinCells (neighborX, neighborY)

    let neighborsOnSlope =
        neighbors
        |> List.filter isOnSlope

    (neighborsOnSlope
    |> Seq.collect getNeighborAndItsNeighbors
    |> List.ofSeq)
    @ [ (x, y) ]

let getBasinSize (x, y) =
    let basinCells = getBasinCells (x, y)
    let basinSize = basinCells |> Seq.distinct |> Seq.length
    basinSize

let allBasins =
    lowPoints
    |> Seq.map getBasinSize
let threeLargestBasins =
    allBasins
    |> Seq.sortDescending
    |> Seq.take 3

let productOfLargestBasins =
    threeLargestBasins |> Seq.fold (*) 1

printfn $"Product of three largest basins: %d{productOfLargestBasins}"