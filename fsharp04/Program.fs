open System
open System.IO

//let input =
//    [| "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
//       ""
//       "22 13 17 11  0"
//       " 8  2 23  4 24"
//       "21  9 14 16  7"
//       " 6 10  3 18  5"
//       " 1 12 20 15 19"
//       ""
//       " 3 15  0  2 22"
//       " 9 18 13 17  5"
//       "19  8  7 25 23"
//       "20 11 10 24  4"
//       "14 21 16 12  6"
//       ""
//       "14 21 17 24  4"
//       "10 16 15  9 19"
//       "18  8 23 26 20"
//       "22 11 13  6  5"
//       " 2  0 12  3  7" |]
let input = File.ReadAllLines "input.txt"

type Cell = byte * bool
type Board = Cell list

let draws = input.[0].Split(',')
            |> Array.map byte
            |> List.ofArray

let rec parseBoards lines boards =
    let parseLine (line: string) =
        line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map byte

    let parseBoard boardLines =
        boardLines
        |> Array.collect parseLine
        |> Array.map (fun n -> (n, false))
        |> List.ofArray

    match lines with
    | [||] -> boards
    | _ ->
         let board = parseBoard (Array.take 5 (Array.skip 1 lines))
         let newBoards = boards @ [board]

         parseBoards (Array.skip 6 lines) newBoards

let playNumber number board =
    let markCell (num, marked) =
        if num = number then (num, true) else (num, marked)

    List.map markCell board

let playNumberAll number boards =
    List.map (playNumber number) boards

let boards = parseBoards (Array.skip 1 input) []

let isWinner board =
    let rows =
        [0..5..20]
        |> List.map (fun offset -> List.take 5 (List.skip offset board))

    let columns =
        [0..4]
        |> List.map (fun offset -> [0..5..20]
                                   |> List.map (fun index -> board.[offset + index]))

    let areAllMarked cells =
        Option.isNone (List.tryFind (fun (_, marked) -> not marked) cells)

    rows @ columns
    |> List.filter areAllMarked
    |> List.isEmpty
    |> not

let tryFindWinningBoard boards =
    boards
    |> List.tryFind isWinner

let firstWinningBoard = tryFindWinningBoard

let lastWinningBoard (boards: Board list) =
    match boards with
    | [_] -> tryFindWinningBoard boards
    | _ -> None

let rec play getFinalBoard boards draws  =
    let nextDraw = List.head draws
    let playedBoards = playNumberAll nextDraw boards
    let winningBoard = getFinalBoard playedBoards

    match winningBoard with
    | Some b -> (b, nextDraw)
    | None ->
        let remainingBoards = playedBoards
                              |> List.filter (isWinner >> not)
        play getFinalBoard remainingBoards (List.tail draws)

let playToWin = play firstWinningBoard
let playToLose = play lastWinningBoard

let unmarkedCells board =
    let isUnmarked ((_, marked): Cell) = not marked
    List.filter isUnmarked board

let playForStrategy strategy =
    let winner, finalDraw = strategy boards draws

    let sumOfUnmarkedCells =
        unmarkedCells winner
        |> List.map fst
        |> List.map int
        |> List.sum

    sumOfUnmarkedCells * (finalDraw |> int)

printfn $"Play to win - final score: %d{playForStrategy playToWin}"
printfn $"Play to lose - final score: %d{playForStrategy playToLose}"