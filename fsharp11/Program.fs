open Spectre.Console

let input = Input.realInput

type Octopus = {
    EnergyLevel: byte
    Flashed: bool
}

type State = {
    Octopuses: Octopus[][]
    FlashCount: int
}

let octopuses =
    input
    |> Array.map (fun (s: string) -> s.ToCharArray()
                                     |> Array.map (fun c -> {
                                         EnergyLevel = System.String([| c |]) |> byte
                                         Flashed = false
                                     }))
    |> Array.transpose

let maxY = octopuses.[0].Length - 1
let maxX = octopuses.Length - 1

let mapAll f =
    Array.map (fun row -> row |> Array.map f)

let incrementEnergyLevels =
    mapAll (fun o -> { o with EnergyLevel = o.EnergyLevel + 1uy; Flashed = false })

let rec flash (octopuses: Octopus[][]) =
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

    let rec flashOctopus (x, y) =
        if newOctopuses.[x].[y].Flashed then
            ()
        else
            newOctopuses.[x].[y] <- { newOctopuses.[x].[y] with Flashed = true }

            let neighbors = neighbors (x, y)
            for nX, nY in neighbors do
                newOctopuses.[nX].[nY] <- { newOctopuses.[nX].[nY] with EnergyLevel = newOctopuses.[nX].[nY].EnergyLevel + 1uy }

            for nX, nY in neighbors do
                if newOctopuses.[nX].[nY].EnergyLevel > 9uy then
                    flashOctopus (nX, nY)

    for x in 0..maxX do
        for y in 0..maxY do
            if newOctopuses.[x].[y].EnergyLevel > 9uy then
                flashOctopus (x, y)

    newOctopuses

let countFlashes octopuses =
    octopuses
    |> mapAll (fun o -> if o.Flashed then 1 else 0)
    |> Array.collect id
    |> Array.sum

let resetFlashedToZero =
    mapAll (fun o -> if o.Flashed = false then o else { o with EnergyLevel = 0uy })

let printState state cycle =
    AnsiConsole.MarkupLine(sprintf $"After cycle %d{cycle}: ([bold]%d{state.FlashCount}[/] flashes)")

    for row in state.Octopuses |> Array.transpose do
        for octopus in row do
            let format = if octopus.Flashed then "[bold]" else "[dim]"
            AnsiConsole.Markup(sprintf $"%s{format}%d{octopus.EnergyLevel}[/]")
        printfn ""

    printfn ""

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
    |> mapAll (fun o -> o.Flashed)
    |> Array.collect id
    |> Array.forall id

let afterFirstTimeAllOctopusesFlash =
    allCycles
    |> Seq.mapi (fun i s -> (i + 1, s))
    |> Seq.skipWhile (fun (_, s) -> not (allOctopusesFlash s.Octopuses))
    |> Seq.head
    |> fst

AnsiConsole.MarkupLine(sprintf $"The first time all octopuses flash is in cycle [bold]%d{afterFirstTimeAllOctopusesFlash - 1}[/]")