open System
open FParsec

let input = Input.realInput

type NodeLabel =
    | Start
    | End
    | SmallRoom of string
    | BigRoom of string

type Node = NodeLabel * NodeLabel list

let parseNodeType node =
    match node with
    | "start" -> Start
    | "end" -> End
    | _ when Char.IsLower(node.[0]) -> SmallRoom(node)
    | _ when Char.IsUpper(node.[0]) -> BigRoom(node)
    | _ -> failwithf $"Unexpected node %s{node}"

let parseLine line =
    let pnode = choice [ pstring "start" >>% Start
                         pstring "end" >>% End
                         many1Satisfy isAsciiUpper |>> (fun n -> BigRoom(n))
                         many1Satisfy isAsciiLower |>> (fun n -> SmallRoom(n))]
    let parser =
        tuple2 (pnode .>> pstring "-") pnode

    match run parser line with
    | Success((a, b), _, _) -> (a, b)
    | Failure(error, _, _) -> failwith error

let parseGraph input =
    let connections =
        input |> Array.map parseLine

    let distinctRooms =
        connections |> Array.map fst |> Set
        |> Set.union (connections |> Array.map snd |> Set)

    let getConnections room: NodeLabel list =
        connections
        |> Array.filter (fun (a, _) -> a = room)
        |> Array.map snd
        |> Array.append
            (connections
             |> Array.filter (fun (_, b) -> b = room)
             |> Array.map fst)
        |> List.ofArray

    let rooms =
        distinctRooms
        |> Array.ofSeq
        |> Array.map (fun r -> (r, (r, getConnections r)))
        |> Map

    rooms

let rooms = parseGraph input

let anySmallRoomVisitedOnce map label =
    not (Map.containsKey label map)

let singleSmallRoomVisitedTwice map label =
    ((Map.containsKey label map) && not (Map.values map |> Seq.contains 2)) || not (Map.containsKey label map)

let rec getPaths currentPath smallRoomsVisited smallRoomVisitPredicate node =
    let label, children = node
    let getChildPaths =
        lazy (
            let suitableChildren = children |> List.except [Start] |> List.map (fun label -> rooms.[label])
            let newSmallRoomsVisited =
                match label with
                | SmallRoom _ -> Map.change label (fun c -> Some((c |> Option.defaultValue 0) + 1)) smallRoomsVisited
                | _ -> smallRoomsVisited

            if suitableChildren.IsEmpty then
                None
            else
                let paths = suitableChildren
                            |> List.map (getPaths (currentPath @ [label]) newSmallRoomsVisited smallRoomVisitPredicate)
                            |> List.collect (Option.defaultValue [])

                if paths.IsEmpty then
                    None
                else
                    Some(paths)
        )

    match label with
    | End -> Some([currentPath])
    | Start -> getChildPaths.Value
    | BigRoom _ -> getChildPaths.Value
    | SmallRoom _ when smallRoomVisitPredicate smallRoomsVisited label -> getChildPaths.Value
    | _ -> None

let paths1 = getPaths [] Map.empty anySmallRoomVisitedOnce rooms[Start]
             |> Option.get
printfn $"Number of paths visiting small rooms at most once: %d{paths1.Length}"

let paths2 = getPaths [] Map.empty singleSmallRoomVisitedTwice rooms[Start]
             |> Option.get
printfn $"Number of paths visiting any single small rooms at most twice: %d{paths2.Length}"