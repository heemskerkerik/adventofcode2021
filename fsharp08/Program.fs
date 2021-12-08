open System.Collections.Generic
open Microsoft.FSharp.Collections

let input = Input.realInput

type Entry = {
    Examples: Set<char>[]
    Output: Set<char>[]
}

let parseEntry (l: string) =
    let parts = l.Split('|')

    { Examples = parts.[0].Trim().Split(' ') |> Array.map (fun (s: string) -> s.ToCharArray() |> Set.ofArray)
      Output = parts.[1].Trim().Split(' ') |> Array.map (fun (s: string) -> s.ToCharArray() |> Set.ofArray) }

let entries =
    input |> Array.map parseEntry

let isEasyDigit (v: Set<char>) =
    match v.Count with
    | 2 -> true // 1
    | 3 -> true // 7
    | 4 -> true // 4
    | 7 -> true // 8
    | _ -> false

let easyDigitCount =
    entries
    |> Array.map (fun l -> l.Output |> Array.filter isEasyDigit |> Array.length)
    |> Array.sum
printfn $"Number of easy digits: %d{easyDigitCount}"

let pow n p =
    (n |> float) ** p |> int

let solveEntry e =
    let map = Dictionary<int, Set<char>>()

    // 4 has length 4
    map.[4] <- e.Examples |> Array.find (fun e -> e.Count = 4)
    // 1 has length 2
    map.[1] <- e.Examples |> Array.find (fun e -> e.Count = 2)
    // 7 has length 3
    map.[7] <- e.Examples |> Array.find (fun e -> e.Count = 3)
    // 8 has length 7
    map.[8] <- e.Examples |> Array.find (fun e -> e.Count = 7)

    // 6 is the only digit of length 6 that doesn't have both c and f elements, which make up 1
    map.[6] <- e.Examples |> Array.find (fun e -> e.Count = 6 && not (Set.isSuperset e map.[1]))
    // 9 has 6 digits and is the only one to intersect both 4 and 7
    map.[9] <- e.Examples |> Array.find (fun e -> e.Count = 6 && Set.isSuperset e (Set.union map.[4] map.[7]))
    // 0 has length 6 but is not 6 or 9
    map.[0] <- e.Examples |> Array.find (fun e -> e.Count = 6 && not (e = map.[6]) && not (e = map.[9]))

    // 3 has 5 elements and has both elements of 1
    map.[3] <- e.Examples |> Array.find (fun e -> e.Count = 5 && Set.isSuperset e map.[1])

    // the only difference between 3 and 9 is element b
    let elementB = Set.difference map.[9] map.[3] |> Set.minElement

    // out of 2 and 5, only 5 has element b
    map.[5] <- e.Examples |> Array.find (fun e -> e.Count = 5 && not (e = map.[3]) && (Set.contains elementB e))

    // 2 is the element with length 5 that is not 3 or 5
    map.[2] <- e.Examples |> Array.find (fun e -> e.Count = 5 && not (e = map.[3]) && not (e = map.[5]))

    let reverseMap = map |> Seq.map (fun pair -> (pair.Value, pair.Key)) |> dict

    e.Output
    |> Array.map (fun s -> reverseMap.[s])
    |> Array.rev
    |> Array.mapi (fun i v -> v * (pow 10 i))
    |> Array.sum

let sum =
    entries
    |> Array.map solveEntry
    |> Array.sum

printfn $"Sum of entries: %d{sum}"