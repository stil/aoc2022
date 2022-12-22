module Day17

open System

let emptyRow = "......."

let rocks =
    [ "..####"
      "...#.\n..###\n...#."
      "....#\n....#\n..###"
      "..#\n..#\n..#\n..#"
      "..##\n..##" ]
    |> Seq.map (fun v -> v.Split("\n") |> Seq.toList)
    |> Seq.toList

let windPattern = Helpers.readInput 17 |> Seq.head |> Seq.toList

let rockSeq =
    seq {
        while true do
            yield! rocks
    }

let getCurrentRock rockIndex = rocks[rockIndex % rocks.Length]

let getCurrentWind windIndex =
    windPattern[windIndex % windPattern.Length]

let getRockParticles (rock: string list) baseRow =
    let particles =
        rock
        |> Seq.mapi (fun rowI row -> row |> Seq.mapi (fun colI c -> (rowI, colI, c)))
        |> Seq.collect id
        |> Seq.filter (fun (row, col, c) -> c = '#')
        |> Seq.map (fun (row, col, _) -> (baseRow + row, col))
        |> Seq.toList

    particles

let drawParticles (particles: (int * int) list) (chamber: string list) =
    let collides =
        particles
        |> Seq.exists (fun (pRow, pCol) ->
            pRow < 0 || pCol < 0 || pCol >= chamber[0].Length || chamber[pRow][pCol] = '#')

    if collides then
        None
    else
        let newChamber =
            particles
            |> Seq.fold
                (fun (chamber: string list) (pRow, pCol) ->
                    let newRow =
                        System.String.Concat(chamber[pRow] |> Seq.toList |> List.updateAt pCol '#')

                    chamber |> List.updateAt pRow newRow)
                chamber

        Some(newChamber)

let undrawParticles (particles: (int * int) list) (chamber: string list) =
    particles
    |> Seq.fold
        (fun (chamber: string list) (pRow, pCol) ->
            let newRow =
                System.String.Concat(chamber[pRow] |> Seq.toList |> List.updateAt pCol '.')

            chamber |> List.updateAt pRow newRow)
        chamber

let applyWind rockParticles windDirection =
    let spaceLeft = rockParticles |> Seq.map (fun (pRow, pCol) -> pCol) |> Seq.min

    let spaceRight =
        emptyRow.Length
        - 1
        - (rockParticles |> Seq.map (fun (pRow, pCol) -> pCol) |> Seq.max)

    let offsetLeft = if spaceLeft > 0 then -1 else 0
    let offsetRight = if spaceRight > 0 then 1 else 0

    rockParticles
    |> Seq.map (fun (pRow, pCol) ->
        let offsetX =
            match windDirection with
            | '<' -> offsetLeft
            | '>' -> offsetRight
            | _ -> failwith "Unsupported wind direction."


        let newX = min (emptyRow.Length - 1) (max 0 pCol + offsetX)
        (pRow, newX))
    |> Seq.toList

let applyUpwardsMovement rockParticles =
    rockParticles |> Seq.map (fun (pRow, pCol) -> (pRow - 1, pCol)) |> Seq.toList

type ChamberState =
    { chamber: string list; windIndex: int }

type ChamberRockState =
    { chamber: string list
      windIndex: int
      rockParticles: (int * int) list }

let printChamber (chamber: string list) =
    //System.Console.Clear()
    chamber |> Seq.rev |> Seq.iter (fun line -> printfn $"|%s{line}|")
    printfn "+-------+"
    printfn ""
    printfn ""
    printfn ""

let part1 =
    let pattern =
        "#####..######.#.#.#..#.#....###....###....#####.###.###.####.#.####.#.#..#...#..#...#####......#.....###..####...###....###....####....#.....###.....#.....###...##.#...##.#......#......#......#......#...#####...#.###..#..#...#.###..#...#.##...#.##......####....#.##..#########...#.#....#.#....#####..#...#.....###.....#....###...##.#...##.#......#......#......#......#...#####..######.#.#.#..#.#....###....###....#####.###.###.####.#.####.#.#..#...#..#...#####......#.....###..####...###....###....####....#.....###.....#.....###...##.#...##.#......#......#......#......#...#####...#.###..#..#...#.###..#...#.##...#.##......####....#.##..#########...#.#....#.#....#####..#...#.....###.....#....###...##.#...##.#......#......#......#......#..."

    // if flattened.Contains(pattern) then
    //     printChamber chamber
    //     printfn "cycle end"

    let inspectChamber chamber = ()
    // printChamber chamber
    //checkIfCycleEnd chamber

    let advanceWind (state: ChamberRockState) =
        let currentWind = getCurrentWind state.windIndex
        let movedParticles = applyWind state.rockParticles currentWind

        let newChamber =
            (state.chamber |> undrawParticles state.rockParticles)
            |> drawParticles movedParticles

        match newChamber with
        | Some chamber ->
            Some(
                { chamber = chamber
                  windIndex = state.windIndex + 1
                  rockParticles = movedParticles }
            )
        | None -> None

    let advanceFall (state: ChamberRockState) =
        let movedParticles = applyUpwardsMovement state.rockParticles

        let newChamber =
            (state.chamber |> undrawParticles state.rockParticles)
            |> drawParticles movedParticles

        match newChamber with
        | Some chamber ->
            Some(
                { state with
                    chamber = chamber
                    rockParticles = movedParticles }
            )
        | None -> None

    let mutable prevRockIndex = 0
    let mutable prevWindIndex = 0
    let mutable prevHeight = 0

    let advanceRock (state: ChamberState) rockIndex =
        let rock = getCurrentRock (rockIndex) |> List.rev // Flip vertically for falling upwards.
        let initialRockPosition = state.chamber.Length + 3
        let rockParticles = getRockParticles rock initialRockPosition

        let chamber =
            seq {
                yield emptyRow
                yield emptyRow
                yield emptyRow
                yield emptyRow
                yield emptyRow
                yield emptyRow
                yield emptyRow
            }
            |> Seq.append state.chamber
            |> Seq.toList

        let chamberInitial = (drawParticles rockParticles chamber).Value // Always safe

        let chamberRockState =
            { chamber = chamberInitial
              windIndex = state.windIndex
              rockParticles = rockParticles }

        let rec transform chamberRockState =
            let chamberAfterWind = advanceWind chamberRockState

            match chamberAfterWind with
            | Some stateAfterWind ->
                let chamberAfterFall = advanceFall stateAfterWind

                match chamberAfterFall with
                | Some stateAfterFall ->
                    inspectChamber stateAfterFall.chamber
                    transform stateAfterFall
                | None -> stateAfterWind
            | None ->
                let stateAfterWind =
                    { chamberRockState with windIndex = chamberRockState.windIndex + 1 }

                let chamberAfterFall = advanceFall stateAfterWind

                match chamberAfterFall with
                | Some stateAfterFall ->
                    inspectChamber stateAfterFall.chamber
                    transform stateAfterFall
                | None -> stateAfterWind




        let finalState = transform chamberRockState

        let result =
            { chamber =
                finalState.chamber
                |> Seq.rev
                |> Seq.skipWhile (fun row -> row = emptyRow)
                |> Seq.rev
                |> Seq.toList
              windIndex = finalState.windIndex }

        let flattened = String.Concat(result.chamber)

        if flattened.IndexOf(pattern) >= 0 then
            let parts = flattened.Split(pattern)
            let lastPart = parts |> Seq.last

            if lastPart.Length < 10 then
                let rockDistance = rockIndex - prevRockIndex
                let windDistance = result.windIndex - prevWindIndex
                let heightDistance = result.chamber.Length - prevHeight
                let distance = rockIndex - prevRockIndex
                prevHeight <- result.chamber.Length
                prevRockIndex <- rockIndex
                prevWindIndex <- result.windIndex
                printfn "Rock distance: %d wind index = %d" rockDistance windDistance
                printfn "Rock distance: %d wind index = %d  height distance %d" rockDistance windDistance heightDistance

        //printChamber result.chamber

        // Rock distance: 105 wind index = 598
        // Rock distance: 70 wind index = 400
        // Rock distance: 70 wind index = 400
        // Rock distance: 70 wind index = 400
        // Rock distance: 70 wind index = 400

        // ( 2022 - 105 ) / 70

        result

    // let finalChamber =
    //     Seq.init 2022 id
    //     |> Seq.fold advanceRock { chamber = []; windIndex = 0 }

    let patternToChamber =
        pattern
        |> Seq.rev
        |> Seq.chunkBySize emptyRow.Length
        |> Seq.rev
        |> Seq.map String.Concat
        |> Seq.toList

    let targetRockIndex = 2022

    let rockOffset = 105
    let rockPeriod = 70

    let windOffset = 598
    let windPeriod = 400

    let initialHeightPeriod = 167
    let heightPeriodPerRockPeriod = 106

    let rockPeriodCount = ((targetRockIndex - rockOffset - 1) / rockPeriod)

    let precomputedHeight =
        initialHeightPeriod + rockPeriodCount * heightPeriodPerRockPeriod

    let rockStartIndex = rockOffset + rockPeriod * rockPeriodCount
    let windStartIndex = windOffset + windPeriod

    printfn "start"

    let finalChamber =
        Seq.init (targetRockIndex - rockStartIndex - 3) (fun i -> i + rockStartIndex)
        |> Seq.fold
            advanceRock
            { chamber = patternToChamber
              windIndex = windStartIndex }

    //let longString = System.String.Concat(finalChamber.chamber)
    // printChamber finalChamber.chamber

    // firstDistance = 105
    // rock distance = 70


    let result = finalChamber.chamber.Length + precomputedHeight - patternToChamber.Length
    result

let part2 = 0
