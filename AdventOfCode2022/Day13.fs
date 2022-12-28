module Day13

type Token =
    | Number of int
    | StartArray
    | EndArray

let tokenizePacket (packet: string) =
    0
    |> Seq.unfold (fun index ->
        if index = packet.Length then
            None
        else
            match packet[index] with
            | '[' -> Some(Some StartArray, index + 1)
            | ']' -> Some(Some EndArray, index + 1)
            | ',' -> Some(None, index + 1)
            | _ ->
                let digits =
                    packet |> Seq.skip index |> Seq.takeWhile System.Char.IsDigit |> Seq.toList

                let num = int (System.String.Concat(digits))
                Some(Some(Number num), index + digits.Length))
    |> Seq.choose id
    |> Seq.toList

// Unfold that also returns accumulated state.
let unfold generator state =
    let rec loop build state =
        match generator state with
        | Some (value, state) -> loop (fun newValue -> build (value :: newValue)) state
        | None -> (build []), state

    loop id state

type Value =
    | SingleValue of int
    | MultiValue of Value list

let parsePacket packetStr =
    let packet = tokenizePacket packetStr

    let rec parse (index: int) =
        let unfoldResult =
            index
            |> unfold (fun index ->
                if index = packet.Length then
                    None
                else
                    match packet[index] with
                    | StartArray ->
                        let read, lastIndex = parse (index + 1)
                        Some(MultiValue read, lastIndex + 1)
                    | Number num -> Some(SingleValue num, index + 1)
                    | EndArray -> None)

        let collection, lastState = unfoldResult
        (collection, lastState)

    parse 0 |> fst |> Seq.head

type Order =
    | Right
    | Wrong
    | Inconclusive

let rec comparePair (left: Value) (right: Value) =
    match (left, right) with
    | SingleValue lv, SingleValue rv ->
        if lv = rv then
            Inconclusive
        else
            (if lv < rv then Right else Wrong)
    | SingleValue _, MultiValue _ -> comparePair (MultiValue [ left ]) right
    | MultiValue _, SingleValue _ -> comparePair left (MultiValue [ right ])
    | MultiValue lv, MultiValue rv ->
        let decision =
            (lv, rv)
            ||> Seq.zip
            |> Seq.map (fun pair -> pair ||> comparePair)
            |> Seq.tryFind (fun comp -> comp <> Inconclusive)

        match decision with
        | Some Right -> Right
        | Some Wrong -> Wrong
        | None ->
            if lv.Length < rv.Length then Right
            else if lv.Length > rv.Length then Wrong
            else Inconclusive
        | _ -> failwith "Cannot happen."

let packets =
    Helpers.readInput 13
    |> Seq.filter (fun line -> line <> "")
    |> Seq.map parsePacket

let part1 =
    packets
    |> Seq.chunkBySize 2
    |> Seq.mapi (fun i pair -> (i + 1, pair))
    |> Seq.filter (fun (_, pair) -> (comparePair pair[0] pair[1]) = Right)
    |> Seq.sumBy fst
    |>string

let part2 =
    let dividerPackets = [ parsePacket "[[2]]"; parsePacket "[[6]]" ]

    let orderedPackets =
        packets
        |> Seq.append dividerPackets
        |> Seq.sortWith (fun a b ->
            match comparePair a b with
            | Right -> -1
            | Wrong -> 1
            | Inconclusive -> 0)
        |> Seq.toList

    dividerPackets
    |> Seq.map (fun dividerPacket ->
        orderedPackets
        |> Seq.findIndex (fun orderedPacket -> dividerPacket = orderedPacket))
    |> Seq.map (fun i -> i + 1)
    |> Seq.reduce (fun a b -> a * b)
    |>string

Helpers.assertEqual "4643" part1
Helpers.assertEqual "21614" part2
