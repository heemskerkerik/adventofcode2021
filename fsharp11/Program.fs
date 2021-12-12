open Spectre.Console

let input = Input.realInput

type Octopus = {
    EnergyLevel: byte
    Flashed: bool
}

type Point = int * int

type State = {
    Octopuses: Map<Point, Octopus>
    FlashCount: int
}

let toMap (octopuses: Octopus[][]): Map<Point, Octopus> =
    octopuses
    |> Array.indexed
    |> Array.map (fun (y, row) -> row |> Array.indexed |> Array.map (fun (x, octopus) -> ((x, y), octopus)))
    |> Array.collect id
    |> Map

let octopuses =
    input
    |> Array.map (fun (s: string) -> s.ToCharArray()
                                     |> Array.map (fun c -> {
                                         EnergyLevel = System.String([| c |]) |> byte
                                         Flashed = false
                                     }))
    |> Array.transpose
    |> toMap

let maxY = input.[0].Length - 1
let maxX = input.Length - 1

let incrementEnergyLevels =
    Map.map (fun _ o -> { o with EnergyLevel = o.EnergyLevel + 1uy; Flashed = false })

let rec flash (octopuses: Map<Point, Octopus>) =
    let mutable newOctopuses = octopuses

    let neighbors (x, y) =
        seq {
            for rX in {-1..1} do
                for rY in {-1..1} do
                    if x + rX >= 0
                       && x + rX <= maxX
                       && y + rY >= 0
                       && y + rY <= maxY
                       && (rX <> 0 || rY <> 0) then
                       yield (x + rX, y + rY)
        }
        |> List.ofSeq

    let rec flashOctopus point =
        if newOctopuses.[point].Flashed then
            ()
        else
            newOctopuses <- Map.change point (fun o -> Option.map (fun oo -> { oo with Flashed = true }) o) newOctopuses

            let neighbors = neighbors point
            for neighbor in neighbors do
                newOctopuses <- Map.change neighbor (fun o -> Option.map (fun oo -> { oo with EnergyLevel = oo.EnergyLevel + 1uy }) o) newOctopuses

            for neighbor in neighbors do
                if newOctopuses.[neighbor].EnergyLevel > 9uy then
                    flashOctopus neighbor

    for pair in newOctopuses do
        if pair.Value.EnergyLevel > 9uy then
            flashOctopus pair.Key

    newOctopuses

let countFlashes octopuses =
    octopuses
    |> Map.map (fun _ o -> if o.Flashed then 1 else 0)
    |> Map.values
    |> Seq.sum

let resetFlashedToZero =
    Map.map (fun _ o -> if o.Flashed = false then o else { o with EnergyLevel = 0uy })

let cycle state _ =
    let flashedOctopuses = (incrementEnergyLevels >> flash) state.Octopuses
    let flashCount = state.FlashCount + countFlashes flashedOctopuses
    let finalOctopuses = resetFlashedToZero flashedOctopuses

    let newState =
        { Octopuses = finalOctopuses
          FlashCount = flashCount }
    newState

let discard2 f a _ = f a

let initialState =
    { Octopuses = octopuses
      FlashCount = 0 }

let stateAsResult f a b = (f a b, a)
let allCycles = Seq.initInfinite id
                |> Seq.scan cycle initialState

let stateAfter100Cycles =
    allCycles
    |> Seq.skip 100
    |> Seq.head

AnsiConsole.MarkupLine(sprintf $"After 100 cycles, there have been [bold]%d{stateAfter100Cycles.FlashCount}[/] flashes")

let allOctopusesFlash octopuses =
    octopuses
    |> Map.forall (fun _ o -> o.Flashed)

let afterFirstTimeAllOctopusesFlash =
    allCycles
    |> Seq.indexed
    |> Seq.skipWhile (fun (_, s) -> not (allOctopusesFlash s.Octopuses))
    |> Seq.head
    |> fst

AnsiConsole.MarkupLine(sprintf $"The first time all octopuses flash is in cycle [bold]%d{afterFirstTimeAllOctopusesFlash}[/]")