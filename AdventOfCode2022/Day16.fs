module Day16

open System.Collections.Generic


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


type ValveDefinition =
    { name: string
      flowRate: int
      leadsTo: string Set }

type OpenValve =
    { valve: ValveDefinition
      openAtMinutesLeft: int }

type Node =
    { minutesLeft: int
      currentlyAtValve: ValveDefinition
      openValves: OpenValve list
      traveledPath: string list }

let part1 =
    let valveDefinitions =
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
              leadsTo = parts[ 2 ].Split(", ") |> Set })
        |> Seq.toList

    let getValve valveName =
        valveDefinitions |> Seq.find (fun valve -> valve.name = valveName)

    let allValveNames = valveDefinitions |> Seq.map (fun valve -> valve.name) |> Set

    let brokenValves =
        valveDefinitions
        |> Seq.filter (fun valve -> valve.flowRate = 0)
        |> Seq.map (fun valve -> valve.name)
        |> Set

    let getNeighbors (node: Node) =
        let alreadyOpenValves =
            node.openValves |> Seq.map (fun openValve -> openValve.valve.name) |> Set

        let closedUsefulValves =
            Set.difference allValveNames (Set.union alreadyOpenValves brokenValves)

        if node.minutesLeft = 0 then
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
                              openValves =
                                  [ { valve = node.currentlyAtValve
                                      openAtMinutesLeft = node.minutesLeft - 1 } ]
                                  |> List.append node.openValves
                              traveledPath = [ node.currentlyAtValve.name ] |> List.append node.traveledPath } ]
                // Travel to other valves
                yield!
                    node.currentlyAtValve.leadsTo
                    |> Seq.map getValve
                    |> Seq.map (fun destinationValve ->
                        { currentlyAtValve = destinationValve
                          minutesLeft = node.minutesLeft - 1
                          openValves = node.openValves
                          traveledPath = [ node.currentlyAtValve.name ] |> List.append node.traveledPath })
            }
            |> Seq.toList

    let pressureReleased (node: Node) =
        let pressureReleasedSoFar =
            node.openValves
            |> Seq.sumBy (fun openValve -> openValve.valve.flowRate * openValve.openAtMinutesLeft)

        pressureReleasedSoFar

    let heuristicFn (node: Node) = 0

    let costFn (fromNode: Node) (toNode: Node) =
        let fromReleased = pressureReleased fromNode
        let toReleased = pressureReleased toNode
        let result = toReleased - fromReleased
        -result

    let timeLimit = 30

    let startNode =
        { currentlyAtValve = getValve "AA"
          minutesLeft = timeLimit
          openValves = []
          traveledPath = [] }

    let currentFn (node: Node) = ()
    //printfn "Current node: %s" node.currentlyAtValve.name

    let goalReachedFn (node: Node) = node.minutesLeft = 0

    let bestPath =
        aStar getNeighbors heuristicFn costFn startNode goalReachedFn currentFn

    let pressureReleased = pressureReleased bestPath.Value.Head.Value
    0

let part2 = 0
