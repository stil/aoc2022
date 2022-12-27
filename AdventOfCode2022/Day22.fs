module Day22

open System.Diagnostics

let inputParts = (Helpers.readInputFull 22).Split("\r\n\r\n")
let map = inputParts[ 0 ].Split("\r\n")

type Instruction =
    | Move of int
    | TurnR
    | TurnL

let instrString = inputParts[1]

let instrMatches =
    System.Text.RegularExpressions.Regex.Matches(instrString, "(\d+|R|L)")

let instructions =
    instrMatches
    |> Seq.map (fun m ->
        match m.Groups[1].Value with
        | "R" -> TurnR
        | "L" -> TurnL
        | _ -> Move(int m.Groups[1].Value))
    |> Seq.toList

type Direction =
    | Right
    | Down
    | Left
    | Up

type State =
    { Row: int // 0-based
      Column: int // 0-based
      Facing: Direction }

let password state =
    [ (state.Row + 1) * 1000
      (state.Column + 1) * 4
      match state.Facing with
      | Right -> 0
      | Down -> 1
      | Left -> 2
      | Up -> 3 ]
    |> Seq.sum

let makeStep row column facing =
    match facing with
    | Right -> (row, column + 1)
    | Down -> (row + 1, column)
    | Left -> (row, column - 1)
    | Up -> (row - 1, column)

let initialState =
    { Row = 0
      Column = map[0] |> Seq.findIndex (fun c -> c = '.')
      Facing = Right }

let part1 =
    let measureLine line =
        let min = line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length

        let rowLength =
            line |> Seq.skip min |> Seq.takeWhile (fun c -> c <> ' ') |> Seq.length

        let max = min + rowLength

        (min, max)

    let applyWrap facing row column =
        if facing = Up || facing = Down then
            let vertical =
                [ 0 .. (map.Length - 1) ]
                |> Seq.map (fun rowIndex ->
                    if rowIndex >= map.Length then ' '
                    elif rowIndex < 0 then ' '
                    elif column >= map[rowIndex].Length then ' '
                    else map[rowIndex][column])
                |> Seq.toList

            let minRowIncl, maxRowExcl = measureLine vertical

            let rowWrapped =
                if row >= maxRowExcl then minRowIncl
                elif row < minRowIncl then maxRowExcl - 1
                else row

            (rowWrapped, column)
        else
            let minColIncl, maxColExcl = measureLine map[row]

            let colWrapped =
                if column >= maxColExcl then minColIncl
                elif column < minColIncl then maxColExcl - 1
                else column

            (row, colWrapped)

    let advanceInstruction (state: State) (instr: Instruction) =
        match instr with
        | TurnR ->
            { state with
                Facing =
                    match state.Facing with
                    | Right -> Down
                    | Down -> Left
                    | Left -> Up
                    | Up -> Right }
        | TurnL ->
            { state with
                Facing =
                    match state.Facing with
                    | Right -> Up
                    | Down -> Right
                    | Left -> Down
                    | Up -> Left }
        | Move steps ->

            let finalState =
                Seq.init steps id
                |> Seq.fold
                    (fun state _ ->
                        let nextRow, nextColumn =
                            makeStep state.Row state.Column state.Facing ||> applyWrap state.Facing

                        if map[nextRow][nextColumn] = '#' then
                            state
                        else
                            { state with
                                Column = nextColumn
                                Row = nextRow })
                    state



            finalState

    let finalState = instructions |> Seq.fold advanceInstruction initialState
    let result = password finalState
    result

type Face =
    | Front
    | Left
    | Right
    | Up
    | Down
    | Back

let part2 =
    let measureLine line =
        let min = line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length

        let rowLength =
            line |> Seq.skip min |> Seq.takeWhile (fun c -> c <> ' ') |> Seq.length

        let max = min + rowLength

        (min, max)

    let size = 4

    let faces =
        [ [ None; None; Some(Up) ]
          [ Some(Back); Some(Left); Some(Front) ]
          [ None; None; Some(Down); Some(Right) ] ]
        |> Seq.mapi (fun rowI row ->
            row
            |> Seq.mapi (fun colI face ->
                match face with
                | Some face -> Some((face, (rowI * size, colI * size)))
                | None -> None))
        |> Seq.collect id
        |> Seq.choose id
        |> Seq.toList


    let wrapRules =
        [ ((Face.Up, Direction.Up), (Face.Back, Direction.Down))
          ((Face.Up, Direction.Right), (Face.Right, Direction.Left))
          ((Face.Up, Direction.Left), (Face.Left, Direction.Down))
          ((Face.Up, Direction.Down), (Face.Front, Direction.Down))
          // --
          ((Face.Front, Direction.Up), (Face.Up, Direction.Up))
          ((Face.Front, Direction.Right), (Face.Right, Direction.Down))
          ((Face.Front, Direction.Left), (Face.Left, Direction.Left))
          ((Face.Front, Direction.Down), (Face.Down, Direction.Down))
          // --
          ((Face.Right, Direction.Up), (Face.Front, Direction.Left))
          ((Face.Right, Direction.Right), (Face.Up, Direction.Left))
          ((Face.Right, Direction.Left), (Face.Down, Direction.Left))
          ((Face.Right, Direction.Down), (Face.Back, Direction.Right))
          // --
          ((Face.Left, Direction.Up), (Face.Up, Direction.Right))
          ((Face.Left, Direction.Right), (Face.Front, Direction.Right))
          ((Face.Left, Direction.Left), (Face.Back, Direction.Left))
          ((Face.Left, Direction.Down), (Face.Down, Direction.Right))
          // --
          ((Face.Back, Direction.Up), (Face.Up, Direction.Down))
          ((Face.Back, Direction.Right), (Face.Left, Direction.Right))
          ((Face.Back, Direction.Left), (Face.Right, Direction.Up))
          ((Face.Back, Direction.Down), (Face.Down, Direction.Up))
          // --
          ((Face.Down, Direction.Up), (Face.Front, Direction.Up))
          ((Face.Down, Direction.Right), (Face.Right, Direction.Right))
          ((Face.Down, Direction.Left), (Face.Left, Direction.Up))
          ((Face.Down, Direction.Down), (Face.Back, Direction.Up)) ]

    let makeStep row column (facing: Direction) =
        let currentFace =
            faces
            |> Seq.find (fun (faceName, (r, c)) -> row >= r && row < r + size && column >= c && column < c + size)


        let newRow, newColumn =
            match facing with
            | Direction.Right -> (row, column + 1)
            | Direction.Down -> (row + 1, column)
            | Direction.Left -> (row, column - 1)
            | Direction.Up -> (row - 1, column)

        let nextFace =
            faces
            |> Seq.tryFind (fun (faceName, (r, c)) ->
                newRow >= r && newRow < r + size && newColumn >= c && newColumn < c + size)

        let outOfBounds = Option.isNone nextFace || nextFace.Value <> currentFace

        if outOfBounds then
            let (nextFace, nextDirection) =
                wrapRules
                |> Seq.find (fun ((f, d), _) -> f = (fst currentFace) && d = facing)
                |> snd

            let _, (nextFaceBaseRow, nextFaceBaseCol) =
                faces |> Seq.find (fun (faceName, (r, c)) -> faceName = nextFace)

            let relPos =
                match facing with
                | Direction.Down -> column - (snd (snd currentFace))
                | Direction.Up -> column - (snd (snd currentFace))
                | Direction.Left -> row - (fst (snd currentFace))
                | Direction.Right -> row - (fst (snd currentFace))

            let lastPos = size - 1

            let nextRow, nextColumn =
                match (facing, nextDirection) with
                | Direction.Down, Direction.Down -> (0, relPos)
                | Direction.Up, Direction.Down -> (0, lastPos - relPos)
                | Direction.Left, Direction.Down -> (0, relPos)
                | Direction.Right, Direction.Down -> (0, lastPos - relPos)
                // --
                | Direction.Down, Direction.Up -> (lastPos, lastPos - relPos)
                | Direction.Up, Direction.Up -> (lastPos, relPos)
                | Direction.Left, Direction.Up -> (lastPos, lastPos - relPos)
                | Direction.Right, Direction.Up -> (lastPos,  relPos)
                // --
                | Direction.Down, Direction.Left -> (relPos, lastPos)
                | Direction.Up, Direction.Left -> (lastPos - relPos, lastPos)
                | Direction.Left, Direction.Left -> (relPos, lastPos)
                | Direction.Right, Direction.Left -> (lastPos - relPos, lastPos)
                // --
                | Direction.Down, Direction.Right -> (lastPos - relPos, 0)
                | Direction.Up, Direction.Right -> (relPos, 0)
                | Direction.Left, Direction.Right -> (lastPos - relPos, 0)
                | Direction.Right, Direction.Right -> (relPos, 0)




            let result =
                (nextFaceBaseRow + nextRow, nextFaceBaseCol + nextColumn, nextDirection)

            result
        else
            (newRow, newColumn, facing)



    let advanceInstruction (state: State) (instr: Instruction) =
        match instr with
        | TurnR ->
            { state with
                Facing =
                    match state.Facing with
                    | Direction.Right -> Direction.Down
                    | Direction.Down -> Direction.Left
                    | Direction.Left -> Direction.Up
                    | Direction.Up -> Direction.Right }
        | TurnL ->
            { state with
                Facing =
                    match state.Facing with
                    | Direction.Right -> Direction.Up
                    | Direction.Down -> Direction.Right
                    | Direction.Left -> Direction.Down
                    | Direction.Up -> Direction.Left }
        | Move steps ->

            let finalState =
                Seq.init steps id
                |> Seq.fold
                    (fun state _ ->
                        let mapCopy = map |> Seq.map (fun line -> line |> Seq.toList) |> Seq.toList

                        let mapCopy2 =
                            mapCopy
                            |> Seq.map (fun line -> line |> Seq.toList)
                            |> Seq.toList
                            |> List.updateAt
                                state.Row
                                (mapCopy[state.Row] |> Seq.toList |> List.updateAt state.Column 'x')
                            |> Seq.map System.String.Concat

                        printfn "%s" (System.String.Join("\r\n", mapCopy2))
                        printfn ""
                        printfn ""
                        printfn ""
                        printfn ""


                        let nextRow, nextColumn, nextFacing = makeStep state.Row state.Column state.Facing

                        if map[nextRow][nextColumn] = '#' then
                            state
                        else
                            { state with
                                Column = nextColumn
                                Row = nextRow
                                Facing = nextFacing })
                    state



            finalState

    let finalState = instructions |> Seq.fold advanceInstruction initialState
    let result = password finalState
    result
