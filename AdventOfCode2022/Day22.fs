module Day22

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

type Facing =
    | Right
    | Down
    | Left
    | Up

type State =
    { Row: int // 0-based
      Column: int // 0-based
      Facing: Facing }

let part1 =
    let initialState =
        { Row = 0
          Column = map[0] |> Seq.findIndex (fun c -> c = '.')
          Facing = Right }

    let makeStep row column facing =
        match facing with
        | Right -> (row, column + 1)
        | Down -> (row + 1, column)
        | Left -> (row, column - 1)
        | Up -> (row - 1, column)

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
                    else map[rowIndex][column]
                    )
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
                        let (nextRow, nextColumn) =
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

    let password state =
        [ (state.Row + 1) * 1000
          (state.Column + 1) * 4
          match state.Facing with
          | Right -> 0
          | Down -> 1
          | Left -> 2
          | Up -> 3 ]
        |> Seq.sum

    let result = password finalState
    result


let part2 = 0
