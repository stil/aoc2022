module Day16

open System.Collections.Generic

type ValveDestination = { name: string; costInMinutes: int }

type ValveDefinition =
    { name: string
      flowRate: int
      leadsTo: ValveDestination Set }

let rec valveDefinitions () =
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

    let rec flattenDestinations depth (destinations: ValveDestination seq) =
        if depth = 11 then
            []
        else
            let leadsTo =
                destinations
                |> Seq.map (fun dest ->
                    let destValve = getValve dest.name

                    if destValve.flowRate = 0 then
                        destValve.leadsTo |> flattenDestinations (depth + 1)
                    else
                        [ { dest with costInMinutes = depth } ])
                |> Seq.collect id
                |> Seq.toList

            leadsTo

    let flattenValve (valve: ValveDefinition) =
        { valve with
            leadsTo =
                valve.leadsTo
                |> flattenDestinations 1
                |> Set
                |> Seq.sortBy (fun dest -> (dest.name, dest.costInMinutes))
                |> Seq.distinctBy (fun dest -> dest.name)
                |> Seq.filter (fun dest -> dest.name <> valve.name)
                |> Set }

    let result =
        definitions
        |> List.map flattenValve
        |> Seq.filter (fun def -> def.name = "AA" || def.flowRate > 0)
        |> Seq.toList


    result

let aStar neighborsFn heuristicFn costFn (start: 'a) (goalReachedFn: 'a -> bool) (currentFn: 'a -> unit) =
    let frontier = PriorityQueue<'a, int>()
    let cameFrom = Dictionary<'a, 'a option>()
    let costSoFar = Dictionary<'a, int>()

    frontier.Enqueue(start, 0)
    cameFrom[start] <- None
    costSoFar[start] <- 0

    let mutable goal = None

    while Option.isNone goal && frontier.Count > 0 do
        let current = frontier.Dequeue()
        currentFn current

        if goalReachedFn current then
            goal <- Some current
        else
            neighborsFn current
            |> Seq.iter (fun next ->
                let newCost = costSoFar[current] + costFn current next

                if not (costSoFar.ContainsKey(next)) || newCost < costSoFar[next] then
                    costSoFar[next] <- newCost
                    let priority = newCost + heuristicFn next
                    frontier.Enqueue(next, priority)
                    cameFrom[next] <- Some(current)
                    ()

                ())


    if Option.isNone goal then
        None
    else
        let shortestPath =
            goal
            |> Seq.unfold (fun somePoint ->
                match somePoint with
                | Some point ->
                    let found, prevPoint = cameFrom.TryGetValue(point)
                    Some(somePoint, prevPoint)
                | None -> None)
            |> Seq.toList

        if shortestPath |> Seq.last = Some(start) then
            Some(shortestPath)
        else
            None


type Node =
    { minutesLeft: int
      currentlyAtValve: ValveDefinition
      openValves: string Set
      pressureReleased: int
      traveledPath: string list }

let part1 =
    let valveDefinitions = valveDefinitions ()

    let valveDefinitionsDict =
        valveDefinitions |> Seq.map (fun vd -> (vd.name, vd)) |> dict

    let getValve valveName = valveDefinitionsDict[valveName]

    let allValveNames = valveDefinitions |> Seq.map (fun valve -> valve.name) |> Set

    let brokenValves =
        valveDefinitions
        |> Seq.filter (fun valve -> valve.flowRate = 0)
        |> Seq.map (fun valve -> valve.name)
        |> Set

    let getNeighbors (node: Node) =
        let alreadyOpenValves = node.openValves

        let closedUsefulValves =
            Set.difference allValveNames (Set.union alreadyOpenValves brokenValves)

        if node.minutesLeft <= 0 then
            []
        elif node.minutesLeft > 0 && closedUsefulValves.Count = 0 then
            // Stay at current valve and do nothing.
            [ { node with
                  minutesLeft = node.minutesLeft - 1
                  traveledPath = [ node.currentlyAtValve.name ] |> List.append node.traveledPath } ]
        else
            let canBeOpened = closedUsefulValves |> Set.contains node.currentlyAtValve.name

            seq {
                // Open current valve (if closed and useful)
                if canBeOpened then
                    yield!
                        [ { node with
                              minutesLeft = node.minutesLeft - 1
                              openValves = node.openValves |> Set.add node.currentlyAtValve.name
                              pressureReleased =
                                  node.pressureReleased
                                  + ((node.minutesLeft - 1) * node.currentlyAtValve.flowRate)
                              traveledPath = [ node.currentlyAtValve.name ] |> List.append node.traveledPath } ]
                // Travel to other valves
                yield!
                    node.currentlyAtValve.leadsTo
                    |> Seq.filter (fun v -> node.minutesLeft >= v.costInMinutes)
                    |> Seq.map (fun v -> (getValve v.name, v.costInMinutes))
                    |> Seq.map (fun (destinationValve, travelCostInMinutes) ->
                        { node with
                            currentlyAtValve = destinationValve
                            minutesLeft = node.minutesLeft - travelCostInMinutes
                            traveledPath = [ node.currentlyAtValve.name ] |> List.append node.traveledPath })
            }
            |> Seq.toList

    let heuristicFn (node: Node) =

        let rec potential node depth =
            if depth = 0 then
                node.pressureReleased
            else
                let neighbors = getNeighbors node

                let result =
                    if neighbors.Length > 0 then
                        neighbors |> Seq.map (fun n -> potential n (depth - 1)) |> Seq.max
                    else
                        node.pressureReleased

                result

        let potentialValue = potential node 12

        -(potentialValue)

    let costFn (fromNode: Node) (toNode: Node) = 0

    let timeLimit = 30

    let startNode =
        { currentlyAtValve = getValve "AA"
          minutesLeft = timeLimit
          openValves = Set.empty
          pressureReleased = 0
          traveledPath = [] }

    let currentFn (node: Node) = ()
    //printfn "Current node: %s" node.currentlyAtValve.name

    let goalReachedFn (node: Node) = node.minutesLeft = 0

    let bestPath =
        aStar getNeighbors heuristicFn costFn startNode goalReachedFn currentFn

    let pressureReleased = bestPath.Value.Head.Value.pressureReleased
    pressureReleased

let part2 = 0
