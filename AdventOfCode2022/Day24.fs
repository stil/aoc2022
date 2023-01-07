module Day24

open System.Collections.Generic

type Wind =
    | Left
    | Right
    | Up
    | Down
    | Stay

let width = (Helpers.readInput 24 |> Seq.head |> Seq.length) - 2
let height = (Helpers.readInput 24 |> Seq.length) - 2

let initialWinds =
    Helpers.readInput 24
    |> Seq.mapi (fun y row ->
        row
        |> Seq.mapi (fun x c ->
            match c with
            | '>' -> Some((y, x), Right)
            | '^' -> Some((y, x), Up)
            | '<' -> Some((y, x), Left)
            | 'v' -> Some((y, x), Down)
            | _ -> None))
    |> Seq.collect id
    |> Seq.choose id
    |> Seq.toList

type State = { steps: int; position: int * int }

let findShortestPathGeneric neighborsFn heuristicFn costFn (start: 'a) (isGoalFn: 'a -> bool) =
    let frontier = PriorityQueue<'a, int>()
    let cameFrom = Dictionary<'a, 'a option>()
    let costSoFar = Dictionary<'a, int>()

    frontier.Enqueue(start, 0)
    cameFrom[start] <- None
    costSoFar[start] <- 0

    let mutable goal = None

    while Option.isNone goal && frontier.Count > 0 do
        let current = frontier.Dequeue()

        if isGoalFn current then
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

    match goal with
    | Some _ ->
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
    | None -> None

let move (y, x) dir =
    let nextPos =
        match dir with
        | Right -> (y, x + 1)
        | Left -> (y, x - 1)
        | Up -> (y - 1, x)
        | Down -> (y + 1, x)
        | Stay -> (y, x)

    nextPos

let part1 () =
    let initialState = { steps = 0; position = (0, 1) }

    let finalPositionY, finalPositionX = (height + 1, width)

    let walls =
        seq {
            yield! [ 0..width ] |> Seq.map (fun i -> (0, i)) // Top horizontal
            yield! [ 0 .. (width + 1) ] |> Seq.map (fun i -> (height + 1, i)) // Bottom horizontal
            yield! [ 0..height ] |> Seq.map (fun i -> (i, 0)) // Left vertical
            yield! [ 0 .. (height + 1) ] |> Seq.map (fun i -> (i, width + 1)) // Right vertical
        }
        |> Set
        |> Set.remove ((finalPositionY, finalPositionX))

    let rec computeWinds winds =
        let nextWinds =
            winds
            |> Seq.map (fun (yx, dir) ->
                let (nextY, nextX) = move yx dir

                let wrap v size =
                    if v < 1 then size
                    elif v >= (size + 1) then 1
                    else v

                let nextYWrapped = wrap nextY height
                let nextXWrapped = wrap nextX width
                ((nextYWrapped, nextXWrapped), dir))
            |> Seq.toList

        let prohibitedPositions = nextWinds |> Seq.map fst |> Set |> Set.union walls

        seq {
            (nextWinds, prohibitedPositions)
            yield! computeWinds nextWinds
        }

    let windsSeq =
        seq {
            yield (initialWinds, initialWinds |> Seq.map fst |> Set |> Set.union walls)
            yield! computeWinds initialWinds
        }
        |> Seq.take 1000
        |> Seq.toList

    let nextStates (state: State) =
        let (winds, prohibitedPositions) = windsSeq[state.steps + 1]

        let states =
            [ Up; Right; Down; Left; Stay ]
            |> Seq.map (move state.position)
            |> Seq.filter (fun dest -> prohibitedPositions |> Set.contains dest |> not)
            |> Seq.filter (fun (y, x) -> y >= 0 && x >= 0 && y <= (height + 1) && x <= (width + 1))
            |> Seq.map (fun dest ->
                { state with
                    steps = state.steps + 1
                    position = dest })
            |> Seq.toList

        states

    let heuristic (state: State) =
        let y, x = state.position
        abs (y - finalPositionY) + abs (x - finalPositionX)

    let costFn _ _ = 1

    let isGoalFn (state: State) =
        state.position = (finalPositionY, finalPositionX)

    let shortestPath =
        findShortestPathGeneric nextStates heuristic costFn initialState isGoalFn

    // match shortestPath with
    // | Some path ->
    //     for state in path |> Seq.rev do
    //         let grid = Array2D.init (height + 2) (width + 2) (fun y x -> '.')
    //
    //         walls |> Seq.iter (fun (y, x) -> Array2D.set grid y x '#')
    //         Array2D.set grid (fst state.Value.position) (snd state.Value.position) 'E'
    //
    //         windsSeq[state.Value.steps]
    //         |> fst
    //         |> Seq.groupBy fst
    //         |> Seq.iter (fun ((y, x), winds) ->
    //             let windsLength = winds |> Seq.length
    //
    //             let windSymbol =
    //                 if windsLength > 1 then
    //                     windsLength.ToString()[0]
    //                 else
    //                     match winds |> Seq.head |> snd with
    //                     | Down -> 'v'
    //                     | Left -> '<'
    //                     | Right -> '>'
    //                     | Up -> '^'
    //
    //             Array2D.set grid y x windSymbol)
    //
    //         [ 0 .. ((grid |> Array2D.length1) - 1) ]
    //         |> Seq.map (fun y -> [ 0 .. ((grid |> Array2D.length2) - 1) ] |> Seq.map (fun x -> grid[y, x]))
    //         |> Seq.map System.String.Concat
    //         |> Seq.iter (fun line -> printfn $"%s{line}")
    //
    //         printfn ""
    // | _ -> failwith "todo"

    let result = shortestPath.Value.Length - 1

    result |> string

let part2 () =
    let finalPositionY, finalPositionX = (height + 1, width)

    let walls =
        seq {
            yield! [ 0..width ] |> Seq.map (fun i -> (0, i)) // Top horizontal
            yield! [ 0 .. (width + 1) ] |> Seq.map (fun i -> (height + 1, i)) // Bottom horizontal
            yield! [ 0..height ] |> Seq.map (fun i -> (i, 0)) // Left vertical
            yield! [ 0 .. (height + 1) ] |> Seq.map (fun i -> (i, width + 1)) // Right vertical
        }
        |> Set
        |> Set.remove (0, 1)
        |> Set.remove (finalPositionY, finalPositionX)



    let rec computeWinds winds =
        let nextWinds =
            winds
            |> Seq.map (fun (yx, dir) ->
                let (nextY, nextX) = move yx dir

                let wrap v size =
                    if v < 1 then size
                    elif v >= (size + 1) then 1
                    else v

                let nextYWrapped = wrap nextY height
                let nextXWrapped = wrap nextX width
                ((nextYWrapped, nextXWrapped), dir))
            |> Seq.toList

        let prohibitedPositions = nextWinds |> Seq.map fst |> Set |> Set.union walls

        seq {
            (nextWinds, prohibitedPositions)
            yield! computeWinds nextWinds
        }

    let windsSeq =
        seq {
            yield (initialWinds, initialWinds |> Seq.map fst |> Set |> Set.union walls)
            yield! computeWinds initialWinds
        }
        |> Seq.take 9999
        |> Seq.toList

    let nextStates removeFromProhibited (state: State) =
        let (winds, prohibitedPositions) = windsSeq[state.steps + 1]

        let states =
            [ Up; Right; Down; Left; Stay ]
            |> Seq.map (move state.position)
            |> Seq.filter (fun dest -> prohibitedPositions |> Set.contains dest |> not)
            |> Seq.filter (fun (y, x) -> y >= 0 && x >= 0 && y <= (height + 1) && x <= (width + 1))
            |> Seq.map (fun dest ->
                { state with
                    steps = state.steps + 1
                    position = dest })
            |> Seq.toList

        states

    // |> Set.remove ((0, 1))
    // |> Set.remove ((finalPositionY, finalPositionX))

    let heuristicGeneric (finalY, finalX) (state: State) =
        let y, x = state.position
        abs (y - finalY) + abs (x - finalX)


    let initialState = { steps = 0; position = (0, 1) }


    let heuristicToGoal = heuristicGeneric (finalPositionY, finalPositionX)
    let heuristicToStart = heuristicGeneric (0, 1)

    let costFn _ _ = 1

    let isGoalFn (state: State) =
        state.position = (finalPositionY, finalPositionX)

    let isGoalFn2 (state: State) = state.position = (0, 1)

    let nextStates1 = nextStates (finalPositionY, finalPositionX)
    let nextStates2 = nextStates (0, 1)


    let startToGoalPath =
        findShortestPathGeneric nextStates1 heuristicToGoal costFn initialState isGoalFn

    let goalToStartPath =
        findShortestPathGeneric nextStates2 heuristicToStart costFn startToGoalPath.Value.Head.Value isGoalFn2

    let startToGoalAgainPath =
        findShortestPathGeneric nextStates1 heuristicToGoal costFn goalToStartPath.Value.Head.Value isGoalFn

    let result =
        startToGoalPath.Value.Length
        + goalToStartPath.Value.Length
        + startToGoalAgainPath.Value.Length
        - 3

    result |> string
