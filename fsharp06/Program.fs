let input = Input.realInput
let parsedInput = input.Split(',') |> Array.map int

let cycle (buckets: int64[]) =
    let spawningFishCount = buckets.[0]
    let getNewValueForIndex index =
        match index with
            | 8 -> spawningFishCount
            | 6 -> buckets.[7] + spawningFishCount
            | _ -> buckets.[index + 1]

    [|0..8|] |> Array.map getNewValueForIndex

let runSimulation cycleCount =
    let buckets = [|0..8|] |> Array.map (fun i -> parsedInput |> Seq.filter (fun t -> t = i) |> Seq.length |> int64)
    let finalBuckets = [1..cycleCount] |> List.fold (fun b _ -> cycle b) buckets

    printfn $"Fish after %d{cycleCount} days: %d{Array.sum finalBuckets}"

runSimulation 18
runSimulation 80
runSimulation 256