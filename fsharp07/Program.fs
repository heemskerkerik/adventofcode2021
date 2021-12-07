open System.Collections.Generic

let input = Input.realInput
let positions = input.Split(',') |> Array.map int

let min = positions |> Array.min
let max = positions |> Array.max

let fuelToMoveTo position =
    positions |> Array.map (fun p -> abs (p - position)) |> Array.sum

let fuel, optimalPosition = {min..max} |> Seq.map (fun p -> (fuelToMoveTo p, p)) |> Seq.minBy fst
printfn $"Best position to move to: %d{optimalPosition} - costs %d{fuel} fuel"

let rec triangular (n: int64): int64 =
    match n with
    | 0L -> 0
    | 1L -> 1
    | _ -> n + triangular (n - 1L)

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

let fuelToMoveToTriangular position =
    positions |> Array.map (fun p -> memoizedTriangular (abs (p - position) |> int64)) |> Array.sum

let fuel', optimalPosition' = {min..max} |> Seq.map (fun p -> (fuelToMoveToTriangular p, p)) |> Seq.minBy fst
printfn $"Triangular, best position to move to: %d{optimalPosition'} - costs %d{fuel'} fuel"
