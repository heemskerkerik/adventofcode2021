open System
open System.Collections.Generic

let input = Input.realInput

let parse (input: string []) =
    let width = input.[0].Length
    let height = input.Length

    Array2D.init width height (fun x y -> (input.[y].Chars x) |> string |> int)

let expandRisks risks factor =
    let width = Array2D.length1 risks
    let height = Array2D.length2 risks
    let clamp n = if n > 9 then (n % 10) + 1 else n

    Array2D.init
        (width * factor)
        (height * factor)
        (fun x y ->
            clamp (
                risks.[(x % width), (y % height)]
                + (x / width)
                + (y / height)
            ))

let risks = parse input

let shortest risks initial =
    let mutable queue = PriorityQueue<int * (int * int), int>()
    let final = (Array2D.length1 risks - 1, Array2D.length2 risks - 1)
    queue.Enqueue((0, initial), 0)
    let mutable distances = Map.empty<int * int, int>
    distances <- distances.Change(initial, (fun _ -> Some(0)))
    let mutable result = 0
    let dirs = [(0, 1); (0, -1); (1, 0); (-1, 0)]

    let withinBounds (x, y) =
        let finalX, finalY = final
        x >= 0 && y >= 0 && x <= finalX && y <= finalY
    let getDist node =
        Map.tryFind node distances |> Option.defaultValue Int32.MaxValue

    while queue.Count > 0 do
        let dist, node = queue.Dequeue ()
        if node = final then
            result <- dist
            queue.Clear ()
        else
            if (getDist node) >= dist then
                let x, y = node

                for dx, dy in dirs do
                    let newX, newY = x + dx, y + dy
                    if withinBounds (newX, newY) then
                        let newDistance = dist + risks.[newX, newY]
                        if (getDist (newX, newY)) > newDistance then
                            distances <- distances.Change((newX, newY), (fun _ -> Some(newDistance)))
                            queue.Enqueue((newDistance, (newX, newY)), newDistance)

    result

printfn $"Step 1: %d{(shortest risks (0, 0))}"

let print risks =
    let width = Array2D.length1 risks
    let height = Array2D.length2 risks

    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            printf $"%d{risks.[x, y]}"

        printfn ""

let expandedRisks = expandRisks risks 5
printfn $"Step 2: %d{shortest expandedRisks (0, 0)}"