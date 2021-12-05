open FParsec
open System.IO

//let input =
//    [| "0,9 -> 5,9"
//       "8,0 -> 0,8"
//       "9,4 -> 3,4"
//       "2,2 -> 2,1"
//       "7,0 -> 7,4"
//       "6,4 -> 2,0"
//       "0,9 -> 2,9"
//       "3,4 -> 1,4"
//       "0,0 -> 8,8"
//       "5,5 -> 8,2" |]
let input = File.ReadAllLines "input.txt"

type Point = int * int
type Line = Point * Point

let parseLine line =
    let parser =
        tuple4 pint32 (pchar ',' >>. pint32 .>> spaces1) (pstring "-> " >>. pint32) (pchar ',' >>. pint32)

    match run parser line with
    | Success((x1, y1, x2, y2), _, _) -> ((x1, y1), (x2, y2))
    | Failure(error, _, _) -> failwith error

let sortCoordinates ((x1, y1), (x2, y2)) =
    if x1 <= x2 then
        ((x1, y1), (x2, y2))
    else
        ((x2, y2), (x1, y1))

let horizontalOrVertical ((x1, y1), (x2, y2)) =
    x1 = x2 || y1 = y2

let lines =
    input |> Array.map (parseLine >> sortCoordinates)
let horizontalOrVerticalLines =
    lines |> Array.filter horizontalOrVertical

let width = (lines
             |> Array.map (fun ((x1, _), (x2, _)) -> max x1 x2)
             |> Array.max) + 1
let height = (lines
              |> Array.map (fun ((_, y1), (_, y2)) -> max y1 y2)
              |> Array.max) + 1

let intersects x y ((x1, y1), (x2, y2)) =
    if x1 = x2 then
        x = x1 && y >= (min y1 y2) && y <= (max y1 y2) // x coords are sorted, y are not
    elif y1 = y2 then
        x >= x1 && x <= x2 && y = y1
    elif y1 < y2 then // diagonally, top-left to bottom-right
        y >= y1 && y <= y2 && x = (x1 + (y - y1))
    else // diagonally, bottom-left to top-right
        y >= y2 && y <= y1 && x = (x1 + (y1 - y))

let numberOfIntersections lines (x, y) =
    lines
    |> Seq.filter (intersects x y)
    |> Seq.length

let occurrences lines = seq { 0..(width * height) - 1 }
                        |> Seq.map (fun i -> (i % width, i / width))
                        |> Seq.map (numberOfIntersections lines)

let printMap (occurrences: seq<int>) =
    let occurrences' = Array.ofSeq occurrences
    for y in 0..(height - 1) do
        let line = occurrences'
                   |> Array.skip (y * width)
                   |> Array.take width

        let lineString = System.String.Join ("", line)
        printfn "%s" lineString

let horizontalOrVerticalDangerousPoints =
    occurrences horizontalOrVerticalLines
    |> Seq.filter (fun o -> o > 1)
    |> Seq.length
printfn "Horizontal or vertical dangerous points: %d" horizontalOrVerticalDangerousPoints

let dangerousPoints =
    occurrences lines
    |> Seq.filter (fun o -> o > 1)
    |> Seq.length
printfn "Dangerous points: %d" dangerousPoints