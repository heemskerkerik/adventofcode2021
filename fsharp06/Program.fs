let input = Input.realInput
let parsedInput = input.Split(',') |> Array.map int
let initialBuckets = {0..8} |> Seq.map (fun i -> parsedInput |> Seq.filter ((=) i) |> Seq.length |> int64) |> Array.ofSeq

let discard f a _ = f a

let cycle (buckets: int64[]) =
    let spawningFishCount = buckets.[0]
    let getNewValueForIndex index =
        match index with
            | 8 -> spawningFishCount
            | 6 -> buckets.[7] + spawningFishCount
            | _ -> buckets.[index + 1]
    [|0..8|] |> Array.map getNewValueForIndex

let runSimulation cycleCount =
    let finalBuckets = {1..cycleCount} |> Seq.fold (discard cycle) initialBuckets
    Array.sum finalBuckets

let runSimulationWithOutput cycleCount =
    let fishCount = runSimulation cycleCount

    printfn $"Fish after %d{cycleCount} days: %d{fishCount}"

runSimulationWithOutput 256