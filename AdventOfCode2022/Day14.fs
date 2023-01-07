module Day14

let scanTraces =
    Helpers.readInput 14
    |> Seq.map (fun line ->
        line.Split(" -> ")
        |> Seq.map (fun coords ->
            let xy = coords.Split(',')
            (int xy[1], int xy[0]))
        |> Seq.toList)
    |> Seq.toList

let maxY = scanTraces |> Seq.collect id |> Seq.map fst |> Seq.max
let maxX = scanTraces |> Seq.collect id |> Seq.map snd |> Seq.max

let grid () =
    let grid = Array2D.init (maxY + 10) (maxX + 500) (fun y x -> '.')

    scanTraces
    |> List.iter (fun trace ->
        trace
        |> List.iteri (fun i point ->
            let current = point
            Array2D.set grid (fst current) (snd current) '#'
            let next = trace |> List.tryItem (i + 1)

            match next with
            | Some nextPoint ->
                let yRange = [ fst current; fst nextPoint ]
                let xRange = [ snd current; snd nextPoint ]

                [ (yRange |> Seq.min) .. (yRange |> Seq.max) ]
                |> Seq.iter (fun y ->
                    [ (xRange |> Seq.min) .. (xRange |> Seq.max) ]
                    |> Seq.iter (fun x -> Array2D.set grid y x '#'))

                ()
            | _ -> ()

            ())

        ())

    grid

let printGrid grid =
    let y0 = grid |> Array2D.base1
    let y1 = y0 + (grid |> Array2D.length1) - 1

    let x0 = grid |> Array2D.base2
    let x1 = x0 + (grid |> Array2D.length2) - 1

    let charSeqToString (chars: char seq) = System.String.Concat(chars)

    let cropLeft = 400

    let array =
        [ y0..y1 ]
        |> Seq.map (fun y ->
            [ x0..x1 ]
            |> Seq.map (fun x -> grid[y, x])
            |> Seq.skip cropLeft
            |> charSeqToString)

    array |> Seq.iter (fun line -> printfn $"%s{line}")
    ()

let pourSand grid isEmptyFn stopPouringFn =
    let sandPos = (0, 500)

    let rec nextPos prevPos =
        if (fst prevPos > (maxY + 10)) then
            prevPos
        else
            let down = (fst prevPos + 1, snd prevPos)
            let left = (fst prevPos + 1, snd prevPos - 1)
            let right = (fst prevPos + 1, snd prevPos + 1)

            if isEmptyFn down then (nextPos down)
            elif isEmptyFn left then left
            elif isEmptyFn right then right
            else prevPos

    let y, x = Seq.init 1000 id |> Seq.fold (fun acc _ -> nextPos acc) sandPos

    if y <= (grid |> Array2D.length1) then
        Array2D.set grid y x 'o'
    else
        ()

    not (stopPouringFn y)

let part1 () =
    let grid = grid ()

    let isEmpty (y, x) =
        if y > maxY then true else grid[y, x] = '.'

    let stopPouring y = y > maxY

    let result =
        Seq.init 1000 id
        |> Seq.takeWhile (fun i -> pourSand grid isEmpty stopPouring)
        |> Seq.length

    result |> string


let part2 () =
    let grid = grid ()
    let floorY = maxY + 2

    let isEmpty (y, x) =
        if y >= floorY then false else (Array2D.get grid y x) = '.'

    let stopPouring y = y = 0

    let result =
        Seq.initInfinite id
        |> Seq.takeWhile (fun _ -> pourSand grid isEmpty stopPouring)
        |> Seq.length

    // printGrid grid

    (result + 1) |> string
