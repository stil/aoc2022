module Day15

let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let sensors =
    Helpers.readInput 15
    |> Seq.map (fun line ->
        let parts =
            line
                .Replace("Sensor at x=", "")
                .Replace(", y=", " ")
                .Replace(": closest beacon is at x=", " ")
                .Split(' ')
            |> Seq.map int
            |> Seq.toList

        let sensor = (parts[0], parts[1])
        let closestBeacon = (parts[2], parts[3])
        let radius = manhattanDistance sensor closestBeacon
        (sensor, closestBeacon, radius))
    |> Seq.toList

let part1x () =
    let mutable score = 0

    let positionsWithoutBeacon (xSensor, ySensor) (xBeacon, yBeacon) yTested =
        let radius = manhattanDistance (xSensor, ySensor) (xBeacon, yBeacon)

        let positions =
            [ (xSensor - radius) .. (xSensor + radius) ]
            |> Seq.map (fun x -> [ yTested ] |> Seq.map (fun y -> (x, y)))
            |> Seq.collect id
            |> Seq.filter (fun pos -> manhattanDistance (xSensor, ySensor) pos <= radius)
            |> Seq.toList

        score <- score + positions.Length

        let onTestedY = positions |> Seq.filter (fun (x, y) -> y = yTested) |> Set

        onTestedY

    let testedY = 2000000

    let prohibitedPositions =
        sensors
        |> Seq.map (fun (sensor, beacon, radius) -> positionsWithoutBeacon sensor beacon testedY)
        |> Seq.collect id
        |> Set

    let beaconPositions =
        sensors |> Seq.map (fun (sensor, beacon, radius) -> beacon) |> Set

    let result = Set.difference prohibitedPositions beaconPositions |> Set.count

    result

let part1 () = part1x () |> string

let part2 () =
    let range = 4000000

    let pointOpt, notDetected =
        Some((0, 0))
        |> Seq.unfold (fun pos ->

            match pos with
            | Some (x, y) ->
                let detected =
                    sensors
                    |> Seq.tryFind (fun (sensor, beacon, radius) ->
                        let currentPointDistance = manhattanDistance (x, y) sensor
                        currentPointDistance <= radius)

                let nextPos =
                    match detected with
                    | Some (sensor, beacon, radius) ->
                        let distanceX = radius - abs ((snd sensor) - y)

                        let skipTo = (fst sensor) + distanceX

                        let nextX = if x = skipTo then x + 1 else skipTo
                        let nextY = if nextX > range then y + 1 else y
                        let nextX = if nextX > range then 0 else nextX
                        if nextY > range then None else Some(nextX, nextY)
                    | None -> None


                Some((pos, detected |> Option.isNone), nextPos)
            | None -> None)
        |> Seq.filter (fun (testedPosition, notDetected) -> notDetected)
        |> Seq.head

    let point = pointOpt.Value
    let result = (uint64 (fst point)) * 4000000UL + (uint64 (snd point))
    result |> string
