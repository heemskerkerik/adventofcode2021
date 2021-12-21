open System.Collections.Generic

type PlayerState = { Position: byte; Score: int16 }

let getDieRolls die =
    (die % 100s, (die + 1s) % 100s, (die + 2s) % 100s, (die + 3s) % 100s)

let rollDie player die =
    let r1, r2, r3, die = getDieRolls die

    let newPosition =
        (((player.Position - 1uy |> int16) + r1 + r2 + r3) % 10s)
        + 1s
        |> byte

    { player with
          Position = newPosition
          Score = player.Score + (newPosition |> int16) },
    die

let rec playOld player1 player2 die rollCount =
    let player1, die = rollDie player1 die
    let newPlayer2, die = rollDie player2 die

    match player1.Score >= 1000s, newPlayer2.Score >= 1000s with
    | true, _ -> player2, rollCount + 3
    | false, true -> player1, rollCount + 6
    | _ -> playOld player1 newPlayer2 die (rollCount + 6)

//let player1 = { Position = 4uy; Score = 0s }
//let player2 = { Position = 8uy; Score = 0s }
let player1 = { Position = 8uy; Score = 0s }
let player2 = { Position = 2uy; Score = 0s }

let losingPlayer, rollCount = playOld player1 player2 1s 0

printfn
    $"Losing player has score %d{losingPlayer.Score} after %d{rollCount} rolls; outcome is %d{(losingPlayer.Score |> int32) * rollCount}"

// --- Part 2
let rolls = [ 1; 2; 3 ]

let combinations =
    rolls
    |> List.allPairs rolls
    |> List.allPairs rolls

let combinationSums =
    combinations
    |> List.groupBy (fun (a, (b, c)) -> a + b + c)
    |> List.map (fun (k, g) -> (k, List.length g |> int64))

let memoize f =
    let cache = Dictionary<_, _>()

    fun c ->
        let exist, value = cache.TryGetValue(c)

        match exist with
        | true -> value
        | false ->
            let value = f c
            cache.Add(c, value)
            value

type Play = int -> int -> int -> int -> int64 * int64

let mutable memoizedPlay: Play = (fun _ _ _ _ -> 0, 0)

let rec play player1 player2 score1 score2 =
    if score2 >= 21 then // it's always player 1's turn, since players are flipped every iteration
        (0L, 1L)
    else
        let winsPerCombination =
            combinationSums
            |> List.map
                (fun (sum, count) ->
                    let player1 = (((player1 + sum) - 1) % 10) + 1
                    let score1 = score1 + player1

                    // players are flipped here
                    let player2Wins, player1Wins =
                        memoizedPlay player2 player1 score2 score1

                    (player2Wins * count, player1Wins * count))

        winsPerCombination
        |> List.map (fun (a, b) -> (b, a)) // flip outcomes so we can sum normally
        |> List.fold (fun (a, b) (newA, newB) -> (a + newA, b + newB)) (0, 0)

memoizedPlay <- memoize play

let player1Wins, player2Wins =
    play (player1.Position |> int) (player2.Position |> int) 0 0

printfn $"Winning player wins %d{max player1Wins player2Wins} times"
