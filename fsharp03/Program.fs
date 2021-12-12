open System.IO

let lines = File.ReadAllLines "input.txt"
let codes = lines
            |> Array.map (fun l -> System.Convert.ToInt32(l, 2))
let bitCount = lines.[0].Length |> byte

let getBitAt bit value =
    let raw = value &&& (1 <<< (bit |> int))
    if raw > 0 then 1uy else 0uy

let getBits values = seq {
    for bit in 0uy..(bitCount - 1uy) do
        values
        |> Array.map (getBitAt bit)
}

let getCounts values =
    let counts = Array.countBy id values
    let ones = Array.tryFind (fun (v, _) -> v = 1uy) counts
               |> Option.defaultValue (1uy, 0)
    let zeroes = Array.tryFind (fun (v, _) -> v = 0uy) counts
                 |> Option.defaultValue (0uy, 0)
    (snd zeroes, snd ones)

let getMostFrequentBit (zeroes, ones) =
    if zeroes > ones then 0uy else 1uy

let bitCounts = codes
                |> getBits
                |> Array.ofSeq
                |> Array.map getCounts
let mostFrequentBits = bitCounts
                       |> Array.map getMostFrequentBit

let bitsToNumber bits =
    bits
    |> Array.mapi (fun i v -> (v |> int) <<< i)
    |> Array.sum

let gamma = mostFrequentBits |> bitsToNumber

let invertBit bit =
    if bit = 0uy then 1uy else 0uy

let epsilon = mostFrequentBits
              |> Array.map invertBit
              |> bitsToNumber

let powerConsumption = gamma * epsilon

printfn $"Gamma: %d{gamma}"
printfn $"Epsilon: %d{epsilon}"

printfn $"Power consumption: %d{powerConsumption}"

let rec getRating values bit predicate =
    let bits = getBits values
               |> Array.ofSeq
    let allBitCounts = bits |> Array.map getCounts
    let thisCounts = allBitCounts.[bit |> int]

    let bitToFind = predicate thisCounts

    let matchingValues = values
                         |> Array.filter (fun v -> (getBitAt bit v) = bitToFind)

    match matchingValues with
    | [| v |] -> v
    | _ -> getRating matchingValues (bit - 1uy) predicate

let oxygenGeneratorRating = getRating codes (bitCount - 1uy) (fun (zeroes, ones) -> if zeroes > ones then 0uy else 1uy)
let co2ScrubberRating = getRating codes (bitCount - 1uy) (fun (zeroes, ones) -> if zeroes <= ones then 0uy else 1uy)

printfn $"Oxygen generator rating: %d{oxygenGeneratorRating}"
printfn $"CO2 scrubber rating: %d{co2ScrubberRating}"

let lifeSupportRating = oxygenGeneratorRating * co2ScrubberRating

printfn $"Life support rating: %d{lifeSupportRating}"