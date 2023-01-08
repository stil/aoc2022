module Day16

open System.Collections.Generic

type ValveDestination = { name: string; costInMinutes: int }

type ValveDefinition =
    { name: string
      flowRate: int
      leadsTo: ValveDestination Set }

type Node =
    { minutesLeft: int
      valveLabel: string
      brokenOrOpenValves: string Set
      pressureReleased: int }

let rec flattenDestination2
    (visited: string Set)
    (leadsToFn: string -> ValveDestination Set)
    (destination: ValveDestination)
    =
    let result =
        (leadsToFn destination.name)
        |> Seq.filter (fun innerDest -> not (visited |> Set.contains innerDest.name))
        |> Seq.map (fun innerDest ->
            ({ innerDest with costInMinutes = innerDest.costInMinutes + destination.costInMinutes }
             |> flattenDestination2 (visited |> Set.add innerDest.name) leadsToFn
             |> Seq.toList))
        |> Seq.collect id
        |> Seq.toList

    destination :: result

let valveDefinitionsTangled =
    let definitions =
        Helpers.readInput 16
        |> Seq.map (fun line ->
            let parts =
                line
                    .Replace("Valve ", "")
                    .Replace(" has flow rate=", "\t")
                    .Replace("tunnels", "tunnel")
                    .Replace("valves", "valve")
                    .Replace("leads", "lead")
                    .Replace("; tunnel lead to valve ", "\t")
                    .Split("\t")

            { name = parts[0]
              flowRate = (int parts[1])
              leadsTo =
                parts[ 2 ].Split(", ")
                |> Seq.map (fun destValveName ->
                    { name = destValveName
                      costInMinutes = 1 })
                |> Set })
        |> Seq.toList

    let valveDefinitionsDict = definitions |> Seq.map (fun vd -> (vd.name, vd)) |> dict
    let getValve valveName = valveDefinitionsDict[valveName]

    let leadsToFn valveLabel =
        let valve = getValve valveLabel
        valve.leadsTo

    let flattenValve2 (valve: ValveDefinition) =
        { valve with
            leadsTo =
                valve.leadsTo
                |> Seq.map (flattenDestination2 (Set [ valve.name ]) leadsToFn)
                |> Seq.collect id
                |> Seq.sortBy (fun (x: ValveDestination) -> (x.name, x.costInMinutes))
                |> Seq.distinctBy (fun (x: ValveDestination) -> x.name)
                |> Set }

    let result = definitions |> Seq.map flattenValve2 |> Seq.toList

    result

let optimizedValveDefinitions =
    let valveDefinitionsDict =
        valveDefinitionsTangled |> Seq.map (fun vd -> (vd.name, vd)) |> dict

    let getValve valveName = valveDefinitionsDict[valveName]

    valveDefinitionsTangled
    |> Seq.filter (fun def -> def.flowRate > 0 || def.name = "AA")
    |> Seq.map (fun valve ->
        { valve with
            leadsTo =
                valve.leadsTo
                |> Set.filter (fun destValve ->
                    let valve = getValve destValve.name
                    valve.flowRate > 0 || valve.name = "AA") })
    |> Seq.toList

let valveDefinitionsDict =
    optimizedValveDefinitions |> Seq.map (fun vd -> (vd.name, vd)) |> dict

let getValve valveName = valveDefinitionsDict[valveName]

let part1 () =
    let allValveNames =
        optimizedValveDefinitions |> Seq.map (fun valve -> valve.name) |> Set

    let getNeighbors (node: Node) =
        let closedUsefulValves = Set.difference allValveNames node.brokenOrOpenValves

        if node.minutesLeft <= 0 then
            Seq.empty
        elif closedUsefulValves.Count = 0 && node.minutesLeft > 0 then
            [ { node with minutesLeft = 0 } ]
        else
            let canBeOpened = closedUsefulValves |> Set.contains node.valveLabel
            let valve = getValve node.valveLabel

            seq {
                // Open current valve (if closed and useful)
                if canBeOpened then
                    let minutesUsed = node.minutesLeft - 1

                    yield
                        { node with
                            minutesLeft = node.minutesLeft - 1
                            brokenOrOpenValves = node.brokenOrOpenValves |> Set.add node.valveLabel
                            pressureReleased = node.pressureReleased + (minutesUsed * valve.flowRate) }


                // Travel to other valves
                let nextDestinations =
                    valve.leadsTo
                    |> Seq.map (fun dest ->
                        { node with
                            valveLabel = dest.name
                            minutesLeft = node.minutesLeft - dest.costInMinutes })

                yield! nextDestinations
            }

    let timeLimit = 30

    let brokenValves =
        optimizedValveDefinitions
        |> Seq.filter (fun valve -> valve.flowRate = 0)
        |> Seq.map (fun valve -> valve.name)
        |> Set

    let startNode =
        { valveLabel = "AA"
          minutesLeft = timeLimit
          brokenOrOpenValves = brokenValves
          pressureReleased = 0 }

    let toVisit = Stack<Node>()
    toVisit.Push(startNode)
    let mutable maxVal = 0
    let mutable goal = None

    while toVisit.Count > 0 do
        let current = toVisit.Pop()

        if current.minutesLeft <= 0 then
            if current.pressureReleased > maxVal then
                maxVal <- max maxVal current.pressureReleased
                goal <- Some current
        else
            let closedUsefulValvesTotalFlow =
                Set.difference allValveNames current.brokenOrOpenValves
                |> Seq.map getValve
                |> Seq.map (fun v -> v.flowRate)
                |> Seq.sortDescending
                |> Seq.toList

            let timer =
                (current.minutesLeft - 2)
                |> Seq.unfold (fun state -> if state > 1 then Some(state, state - 2) else None)

            let potential =
                timer
                |> Seq.zip closedUsefulValvesTotalFlow
                |> Seq.map (fun (a, b) -> a * b)
                |> Seq.sum

            if current.pressureReleased + potential < maxVal then
                ()
            else
                for next in (getNeighbors current) do
                    toVisit.Push(next)


    let result = maxVal
    result |> string

type DoubleNode =
    { brokenOrOpenValves: string Set
      pressureReleased: int
      humanMinutesLeft: int
      humanValveLabel: string
      elephantMinutesLeft: int
      elephantValveLabel: string }

let part2 () =
    let allValveNames =
        optimizedValveDefinitions |> Seq.map (fun valve -> valve.name) |> Set

    let getNeighbors (node: DoubleNode) =
        let closedUsefulValves =
            Set.difference allValveNames (Set.union node.brokenOrOpenValves (Set.singleton "AA"))

        if node.humanMinutesLeft <= 0 && node.elephantMinutesLeft <= 0 then
            Seq.empty
        elif
            closedUsefulValves.Count = 0
            && (node.humanMinutesLeft > 0 || node.elephantMinutesLeft > 0)
        then
            [ { node with
                  humanMinutesLeft = 0
                  elephantMinutesLeft = 0 } ]
        else
            seq {
                let humanValve = getValve node.humanValveLabel

                let humanCanBeOpened =
                    humanValve.flowRate > 0
                    && node.humanMinutesLeft > 0
                    && (closedUsefulValves |> Set.contains node.humanValveLabel)

                let elephantValve = getValve node.elephantValveLabel

                let elephantCanBeOpened =
                    elephantValve.flowRate > 0
                    && node.elephantMinutesLeft > 0
                    && (closedUsefulValves |> Set.contains node.elephantValveLabel)


                let humanOpenActions = if humanCanBeOpened then [ true; false ] else [ false ]
                let elephantOpenActions = if elephantCanBeOpened then [ true; false ] else [ false ]

                let actions =
                    seq {
                        for humanOpenAction in humanOpenActions do
                            for elephantOpenAction in elephantOpenActions do
                                yield (humanOpenAction, elephantOpenAction)
                    }
                    |> Set

                let actionsFiltered =
                    if (node.humanValveLabel = node.elephantValveLabel) then
                        actions |> Set.remove (true, true)
                    else
                        actions

                for humanOpenAction, elephantOpenAction in actionsFiltered do
                    if humanOpenAction && elephantOpenAction then
                        yield
                            { node with
                                humanMinutesLeft = node.humanMinutesLeft - 1
                                elephantMinutesLeft = node.elephantMinutesLeft - 1
                                brokenOrOpenValves =
                                    node.brokenOrOpenValves
                                    |> Set.add node.humanValveLabel
                                    |> Set.add node.elephantValveLabel
                                pressureReleased =
                                    node.pressureReleased
                                    + ((node.humanMinutesLeft - 1) * humanValve.flowRate)
                                    + ((node.elephantMinutesLeft - 1) * elephantValve.flowRate) }
                    elif humanOpenAction && not (elephantOpenAction) then
                        yield
                            { node with
                                humanMinutesLeft = node.humanMinutesLeft - 1
                                brokenOrOpenValves = node.brokenOrOpenValves |> Set.add node.humanValveLabel
                                pressureReleased =
                                    node.pressureReleased + ((node.humanMinutesLeft - 1) * humanValve.flowRate) }
                    elif not humanOpenAction && elephantOpenAction then
                        yield
                            { node with
                                elephantMinutesLeft = node.elephantMinutesLeft - 1
                                brokenOrOpenValves = node.brokenOrOpenValves |> Set.add node.elephantValveLabel
                                pressureReleased =
                                    node.pressureReleased
                                    + ((node.elephantMinutesLeft - 1) * elephantValve.flowRate) }

                for humanDest in
                    (if node.humanMinutesLeft > 0 then
                         humanValve.leadsTo
                     else
                         ([ { name = node.humanValveLabel
                              costInMinutes = 0 } ]
                          |> Set)) do
                    for elephantDest in
                        (if node.elephantMinutesLeft > 0 then
                             elephantValve.leadsTo
                         else
                             [ { name = node.elephantValveLabel
                                 costInMinutes = 0 } ]
                             |> Set) do
                        yield
                            { node with
                                humanValveLabel = humanDest.name
                                humanMinutesLeft = node.humanMinutesLeft - humanDest.costInMinutes
                                elephantValveLabel = elephantDest.name
                                elephantMinutesLeft = node.elephantMinutesLeft - elephantDest.costInMinutes }


            }

    let timeLimit = 26

    let startNode =
        { brokenOrOpenValves = Set.empty
          pressureReleased = 0
          humanMinutesLeft = timeLimit
          humanValveLabel = "AA"
          elephantMinutesLeft = timeLimit
          elephantValveLabel = "AA" }

    let toVisit = Stack<DoubleNode>()
    toVisit.Push(startNode)
    let mutable maxVal = 0
    let mutable goal = None

    while toVisit.Count > 0 do
        let current = toVisit.Pop()

        if current.humanMinutesLeft <= 0 && current.elephantMinutesLeft <= 0 then
            if current.pressureReleased > maxVal then
                maxVal <- max maxVal current.pressureReleased
                goal <- Some current
        else
            let neighbors = getNeighbors current

            let closedUsefulValvesTotalFlow =
                Set.difference allValveNames current.brokenOrOpenValves
                |> Seq.map getValve
                |> Seq.map (fun v -> v.flowRate)
                |> Seq.sortDescending
                |> Seq.chunkBySize 1
                |> Seq.toList

            let timer =
                (min current.humanMinutesLeft current.elephantMinutesLeft)
                |> Seq.unfold (fun state -> if state > 2 then Some(state, state - 2) else None)

            let potential =
                timer
                |> Seq.zip closedUsefulValvesTotalFlow
                |> Seq.map (fun (a, b) -> b * (a |> Seq.sum))
                |> Seq.sum

            if current.pressureReleased + potential < maxVal then
                ()
            else
                for next in neighbors do
                    toVisit.Push(next)


    let result = maxVal
    result |> string
