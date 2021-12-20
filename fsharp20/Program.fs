type Point = int * int

let input = Input.realInput
let algorithm = input[0]
let inputPixels =
    input
    |> List.skip 2
    |> List.mapi (fun y l ->
                  l.ToCharArray()
                  |> Array.indexed
                  |> Array.filter (fun (_, c) -> c = '#')
                  |> Array.map (fun (x, _) -> (x, y))
                  |> List.ofArray)
    |> List.collect id
    |> Set

let alwaysBlank _ = 0
let alternating odd = if odd then 0 else 1
let defaultPixel = if algorithm[0] = '#' && algorithm[algorithm.Length - 1] = '.' then alternating else alwaysBlank

let maxX, maxY = inputPixels |> Seq.map fst |> Seq.max, inputPixels |> Seq.map snd |> Seq.max

let inputMap =
    seq {
        for x in 0..maxX do
            for y in 0..maxY do
                ((x, y), Set.contains (x, y) inputPixels)
    }
    |> Map

let getAlgorithmIndex point input odd =
    let x, y = point
    let rec get i acc =
        let pX, pY = x + ((8 - i) % 3) - 1, y + ((8 - i) / 3) - 1

        let inputPixel = match Map.containsKey (pX, pY) input, odd, algorithm[0] with
                         | false, true, '#' -> 1
                         | false, _, _ -> 0
                         | true, _, _ -> if input[(pX, pY)] then 1 else 0

        let acc = acc + (inputPixel <<< i)

        match i with
        | 0 -> acc
        | _ -> get (i - 1) acc

    get 8 0

let print map =
    let minX, maxX = (map |> Map.keys |> Seq.map fst |> Seq.min), (map |> Map.keys |> Seq.map fst |> Seq.max)
    let minY, maxY = (map |> Map.keys |> Seq.map snd |> Seq.min), (map |> Map.keys |> Seq.map snd |> Seq.max)

    for y in minY..maxY do
        for x in minX..maxX do
            let c = if map[(x, y)] then '#' else '.'
            printf $"%c{c}"
        printfn ""

let enhance input dim odd =
    let minX, maxX = -1, dim
    let minY, maxY = -1, dim

    let pixels = seq {
                     for x in minX..maxX do
                         for y in minY..maxY do
                             let index = getAlgorithmIndex (x, y) input odd
                             if algorithm[index] = '#' then
                                 yield (x, y)
                 }
                 |> Set

    let outcome = seq {
                      for x in minX..maxX do
                          for y in minY..maxY do
                              ((x + 1, y + 1), Set.contains (x, y) pixels)
                  }
                  |> Map
    //print outcome
    outcome

let enhanceNTimes n =
    let rec inner n input dim =
        match n with
        | 0 -> input
        | _ -> inner (n - 1) (enhance input dim (n % 2 = 1)) (dim + 2)
    inner n inputMap (maxX + 1)

printfn $"Lit pixels after 2 times: %d{enhanceNTimes 2 |> Seq.filter (fun pair -> pair.Value) |> Seq.length }"
printfn $"Lit pixels after 50 times: %d{enhanceNTimes 50 |> Seq.filter (fun pair -> pair.Value) |> Seq.length }"