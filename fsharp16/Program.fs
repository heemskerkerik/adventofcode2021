let input = Input.realInput

type Operator =
    | Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo

type Packet =
    | Literal of version: byte * value: int64
    | Operation of version: byte * operator: Operator * packets: Packet list

let nibbleBits (i: int) =
    System.Convert.ToString(i, 2).PadLeft(4, '0').ToCharArray ()
let nibbleChar (i: int) =
    i.ToString("X").Chars 0

let nibbles =
    {0..15}
    |> Seq.map (fun i -> (nibbleChar i, nibbleBits i))
    |> Map

let bits (s: string) =
    s.ToCharArray ()
    |> Array.map (fun c -> nibbles[c])
    |> Array.collect id

let readBits n (slice: char[]) =
    let bits = slice[..n - 1]

    let outcome = System.Convert.ToInt32 (System.String bits, 2)
    (outcome, slice[n..])

let rec readPacket slice =
    let readLiteralPacket slice version =
        let rec readLiteral slice current =
            let finalNibble, slice = readBits 1 slice
            let nibble, slice = readBits 4 slice
            let result = (current |> int64 <<< 4) + (nibble |> int64)

            match finalNibble with
            | 0 -> (result, slice)
            | 1 -> readLiteral slice result
            | _ -> failwithf $"Unexpected bit %d{finalNibble}"

        let literal, slice = readLiteral slice 0
        (Literal(version, literal), slice)

    let readInnerPacketsUntilEnd slice =
        let rec readPackets slice current =
            match slice with
            | [||] -> current
            | _ ->
                let packet, slice = readPacket slice
                readPackets slice (current @ [packet])
        readPackets slice []

    let readInnerPackets (n: int) (slice: char[]) =
        let rec readPackets slice current n =
            match n with
            | 0 -> (current, slice)
            | _ ->
                let packet, slice = readPacket slice
                readPackets slice (current @ [packet]) (n - 1)

        readPackets slice [] n

    let readOperationPacket slice version packetType =
        let lengthType, slice = readBits 1 slice

        let packets, slice =
            match lengthType with
            | 0 ->
                let length, slice = readBits 15 slice
                (readInnerPacketsUntilEnd slice[..length - 1], slice[length..])
            | 1 ->
                let numPackets, slice = readBits 11 slice
                readInnerPackets numPackets slice
            | _ -> failwithf $"Unexpected length type %d{lengthType}"

        let operator =
            match packetType with
            | 0 -> Sum
            | 1 -> Product
            | 2 -> Minimum
            | 3 -> Maximum
            | 5 -> GreaterThan
            | 6 -> LessThan
            | 7 -> EqualTo
            | _ -> failwithf $"Unexpected packet type %d{packetType}"

        (Operation(version, operator, packets), slice)

    let version, slice = readBits 3 slice
    let packetType, slice = readBits 3 slice

    let packet, slice = match packetType with
                        | 4 -> readLiteralPacket slice (version |> byte)
                        | _ -> readOperationPacket slice (version |> byte) packetType

    (packet, slice)

let readOuterPacket (slice: char[]) =
    let beforeLength = slice.Length
    let result, slice = readPacket slice
    let packetLength = beforeLength - slice.Length

    if packetLength % 8 > 0 then
        let _, slice = readBits (8 - (packetLength % 8)) slice
        (result, slice)
    else
        (result, slice)

let packet, slice = readOuterPacket (bits input)
assert (slice.Length = 0)

let rec versionSum packet =
    match packet with
    | Literal(version, _) -> version |> int64
    | Operation(version, _, packets) -> (version |> int64) + List.sumBy versionSum packets

printfn $"Version sum: %d{versionSum packet}"

let rec eval packet: int64 =
    let eval2 f (packets: Packet list) =
        assert (packets.Length = 2)
        if f (eval packets[0]) (eval packets[1]) then 1L else 0L
    let evalN f (packets: Packet list) =
        List.map eval packets |> f
    let operatorEval o =
        match o with
        | Sum -> evalN List.sum
        | Product -> evalN (List.fold (*) 1)
        | Minimum -> evalN List.min
        | Maximum -> evalN List.max
        | GreaterThan -> eval2 (>)
        | LessThan -> eval2 (<)
        | EqualTo -> eval2 (=)

    match packet with
    | Literal(_, value) -> value
    | Operation(_, operator, packets) -> (operatorEval operator) packets

printfn $"Packet evaluates to: %d{eval packet}"
