module Day23

type Direction =
    | N
    | S
    | W
    | E
    | NE
    | NW
    | SE
    | SW

let rec move direction (y, x) =
    match direction with
    | N -> (y - 1, x)
    | S -> (y + 1, x)
    | W -> (y, x - 1)
    | E -> (y, x + 1)
    | NE -> (y, x) |> move N |> move E
    | NW -> (y, x) |> move N |> move W
    | SE -> (y, x) |> move S |> move E
    | SW -> (y, x) |> move S |> move W

type MoveInstruction =
    { noElfIn: Direction list
      moveTo: Direction }

type State =
    { grid: string list
      directions: MoveInstruction list
      movements: int }

let rec transpose xs =
    [ match xs with
      | [] -> failwith "cannot transpose a 0-by-n matrix"
      | [] :: xs -> ()
      | xs ->
          yield List.map List.head xs
          yield! transpose (List.map List.tail xs) ]


let trimGrid (grid: string list) =
    let trimmed =
        grid
        |> List.rev
        |> List.skipWhile (fun str -> str.Replace(".", "") = "")
        |> List.rev
        |> List.skipWhile (fun str -> str.Replace(".", "") = "")

    let trimmed2 =
        trimmed
        |> List.map (fun str -> str |> Seq.toList)
        |> transpose
        |> List.rev
        |> List.skipWhile (fun str -> System.String.Concat(str).Replace(".", "") = "")
        |> List.rev
        |> List.skipWhile (fun str -> System.String.Concat(str).Replace(".", "") = "")
        |> transpose
        |> List.map (fun chars -> System.String.Concat(chars))

    trimmed2


let part1 () =
    let grid = Helpers.readInput 23

    let initialState =
        { grid = grid
          directions =
            [ { noElfIn = [ N; NE; NW ]; moveTo = N }
              { noElfIn = [ S; SE; SW ]; moveTo = S }
              { noElfIn = [ W; NW; SW ]; moveTo = W }
              { noElfIn = [ E; NE; SE ]; moveTo = E } ]
          movements = 0 }

    let advanceRound (state: State) =
        let wider =
            state.grid
            |> Seq.map (fun chars ->
                System.String.Concat(
                    seq {
                        yield '.'
                        yield! chars
                        yield '.'
                    }
                ))
            |> Seq.toList

        let enlargedGrid =
            seq {
                yield System.String('.', wider[0].Length)
                yield! wider
                yield System.String('.', wider[0].Length)
            }
            |> Seq.toList

        let positions =
            enlargedGrid
            |> Seq.mapi (fun row _ -> enlargedGrid[row] |> Seq.mapi (fun col _ -> (row, col, enlargedGrid[row][col])))
            |> Seq.collect id

        let elves = positions |> Seq.filter (fun (row, col, c) -> c = '#') |> Seq.toList

        let movementProposals =
            elves
            |> Seq.map (fun (row, col, _) ->
                let isPositionOccupied direction =
                    let r, c = move direction (row, col)
                    let value = enlargedGrid[r][c]
                    value = '#'

                let someAdjacentPositionsOccupied =
                    [ N; NE; E; SE; S; SW; W; NW ] |> Seq.exists isPositionOccupied

                if someAdjacentPositionsOccupied then
                    let firstValidDirection =
                        state.directions
                        |> Seq.tryFind (fun dir ->
                            let adjacentPositionsOccupied = dir.noElfIn |> Seq.exists isPositionOccupied
                            not adjacentPositionsOccupied)

                    match firstValidDirection with
                    | Some dir ->
                        let targetRow, targetCol = move dir.moveTo (row, col)

                        Some((row, col), (targetRow, targetCol))
                    | None -> None

                else
                    None)

        let distinctTargetPositions =
            movementProposals
            |> Seq.choose id
            |> Seq.filter (fun (src, target) ->
                movementProposals
                |> Seq.choose id
                |> Seq.filter (fun (a, b) -> target = b)
                |> Seq.length = 1)
            |> Seq.toList

        let charSeqToString (charSeq: char seq) = System.String.Concat(charSeq)

        let updatedGrid =
            distinctTargetPositions
            |> Seq.fold
                (fun (acc: string list) ((srcRow, srcCol), (targetRow, targetCol)) ->
                    let clean =
                        acc
                        |> List.updateAt
                            srcRow
                            (acc[srcRow] |> Seq.toList |> List.updateAt srcCol '.' |> charSeqToString)

                    let result =
                        clean
                        |> List.updateAt
                            targetRow
                            (clean[targetRow] |> Seq.toList |> List.updateAt targetCol '#' |> charSeqToString)

                    result)
                enlargedGrid

        // printfn ""
        // printfn ""
        // printfn ""
        // printfn ""
        // printfn "%s" (System.String.Join("\r\n", updatedGrid))

        { state with
            grid = trimGrid updatedGrid
            directions =
                seq {
                    yield! (state.directions |> Seq.skip 1)
                    yield state.directions |> Seq.head
                }
                |> Seq.toList }

    let finalState =
        Seq.init 10 id |> Seq.fold (fun acc _ -> advanceRound acc) initialState


    // printfn ""
    // printfn ""
    // printfn ""
    // printfn ""
    // printfn "%s" (System.String.Join("\r\n", trimmed2))

    let empty =
        finalState.grid
        |> Seq.map (fun row -> row |> Seq.map (fun c -> c))
        |> Seq.collect id
        |> Seq.filter (fun c -> c = '.')
        |> Seq.length

    empty |> string

let part2 () =
    let grid = Helpers.readInput 23

    let initialState =
        { grid = grid
          directions =
            [ { noElfIn = [ N; NE; NW ]; moveTo = N }
              { noElfIn = [ S; SE; SW ]; moveTo = S }
              { noElfIn = [ W; NW; SW ]; moveTo = W }
              { noElfIn = [ E; NE; SE ]; moveTo = E } ]
          movements = 0 }

    let advanceRound (state: State) =
        let wider =
            state.grid
            |> Seq.map (fun chars ->
                System.String.Concat(
                    seq {
                        yield '.'
                        yield! chars
                        yield '.'
                    }
                ))
            |> Seq.toList

        let enlargedGrid =
            seq {
                yield System.String('.', wider[0].Length)
                yield! wider
                yield System.String('.', wider[0].Length)
            }
            |> Seq.toList

        let positions =
            enlargedGrid
            |> Seq.mapi (fun row _ -> enlargedGrid[row] |> Seq.mapi (fun col _ -> (row, col, enlargedGrid[row][col])))
            |> Seq.collect id

        let elves = positions |> Seq.filter (fun (row, col, c) -> c = '#') |> Seq.toList

        let movementProposals =
            elves
            |> Seq.map (fun (row, col, _) ->
                let isPositionOccupied direction =
                    let r, c = move direction (row, col)
                    let value = enlargedGrid[r][c]
                    value = '#'

                let someAdjacentPositionsOccupied =
                    [ N; NE; E; SE; S; SW; W; NW ] |> Seq.exists isPositionOccupied

                if someAdjacentPositionsOccupied then
                    let firstValidDirection =
                        state.directions
                        |> Seq.tryFind (fun dir ->
                            let adjacentPositionsOccupied = dir.noElfIn |> Seq.exists isPositionOccupied
                            not adjacentPositionsOccupied)

                    match firstValidDirection with
                    | Some dir ->
                        let targetRow, targetCol = move dir.moveTo (row, col)

                        Some((row, col), (targetRow, targetCol))
                    | None -> None

                else
                    None)
            |> Seq.choose id
            |> Seq.toList

        let dict2 =
            movementProposals
            |> Seq.groupBy snd
            |> Seq.map (fun (key, list) -> (key, list |> Seq.length))
            |> dict


        let distinctTargetPositions =
            movementProposals
            |> Seq.filter (fun (src, target) -> dict2[target] = 1)
            |> Seq.toList

        let charSeqToString (charSeq: char seq) = System.String.Concat(charSeq)

        let updatedGrid =
            distinctTargetPositions
            |> Seq.fold
                (fun (acc: string list) ((srcRow, srcCol), (targetRow, targetCol)) ->
                    let clean =
                        acc
                        |> List.updateAt
                            srcRow
                            (acc[srcRow] |> Seq.toList |> List.updateAt srcCol '.' |> charSeqToString)

                    let result =
                        clean
                        |> List.updateAt
                            targetRow
                            (clean[targetRow] |> Seq.toList |> List.updateAt targetCol '#' |> charSeqToString)

                    result)
                enlargedGrid

        // printfn ""
        // printfn ""
        // printfn ""
        // printfn ""
        // printfn "%s" (System.String.Join("\r\n", updatedGrid))

        { state with
            grid = trimGrid updatedGrid
            movements = distinctTargetPositions |> Seq.length
            directions =
                seq {
                    yield! (state.directions |> Seq.skip 1)
                    yield state.directions |> Seq.head
                }
                |> Seq.toList }

    let rec stateSeq initialState =
        let nextState = advanceRound initialState

        seq {
            yield nextState
            yield! stateSeq nextState
        }

    let result =
        stateSeq initialState
        |> Seq.takeWhile (fun state -> state.movements > 0)
        |> Seq.length

    result + 1 |> string
