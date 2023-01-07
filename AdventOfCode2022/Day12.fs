module Day12

open System.Collections.Generic

let findShortestPathGeneric neighborsFn heuristicFn costFn (start: 'a) (goal: 'a) =
    let frontier = PriorityQueue<'a, int>()
    let cameFrom = Dictionary<'a, 'a option>()
    let costSoFar = Dictionary<'a, int>()

    frontier.Enqueue(start, 0)
    cameFrom[start] <- None
    costSoFar[start] <- 0

    let mutable goalReached = false

    while not goalReached && frontier.Count > 0 do
        let current = frontier.Dequeue()

        if current = goal then
            goalReached <- true
        else
            neighborsFn current
            |> Seq.iter (fun next ->
                let newCost = costSoFar[current] + costFn current next

                if not (costSoFar.ContainsKey(next)) || newCost < costSoFar[next] then
                    costSoFar[next] <- newCost
                    let priority = newCost + heuristicFn goal next
                    frontier.Enqueue(next, priority)
                    cameFrom[next] <- Some(current)
                    ()

                ())

    let shortestPath =
        Some(goal)
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

let input = Helpers.readInput 12
let width = input[0].Length
let height = input.Length

let filterPoints charPredicate =
    [ 0 .. (height - 1) ]
    |> Seq.collect (fun y -> [ 0 .. (width - 1) ] |> Seq.map (fun x -> ((y, x), input[y][x])))
    |> Seq.filter (fun (_, v) -> charPredicate v)
    |> Seq.map fst

let goal = filterPoints (fun c -> c = 'E') |> Seq.head

let charToElevation char =
    let normalized =
        match char with
        | 'S' -> 'a'
        | 'E' -> 'z'
        | _ -> char

    int normalized

let getElevation (y, x) = charToElevation (input[y][x])

let getNeighbors (y, x) =
    let currentElevation = getElevation (y, x)

    [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ]
    |> Seq.filter (fun (y, x) -> y >= 0 && x >= 0 && y < height && x < width)
    |> Seq.filter (fun (y, x) -> (abs (getElevation (y, x)) - currentElevation) <= 1)

let heuristic (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)
let getCost fromNode toNode = 1

let findShortestPathLength start =
    match findShortestPathGeneric getNeighbors heuristic getCost start goal with
    | Some path -> Some(path.Length - 1)
    | None -> None

let part1 () =
    let start = filterPoints (fun c -> c = 'S') |> Seq.head
    findShortestPathLength start |> Option.get |> string

let part2 () =
    filterPoints (fun c -> charToElevation c = charToElevation 'a')
    |> Seq.choose findShortestPathLength
    |> Seq.min
    |> string
