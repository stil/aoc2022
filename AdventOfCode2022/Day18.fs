module Day18

type Cube = int * int * int

type Side =
    | Front
    | Back
    | Left
    | Right
    | Up
    | Down

// Left handed cartesian coordinates.
let getCubeOnSide (x, y, z) side =
    match side with
    | Front -> (x, y, z - 1)
    | Back -> (x, y, z + 1)
    | Left -> (x - 1, y, z)
    | Right -> (x + 1, y, z)
    | Up -> (x, y + 1, z)
    | Down -> (x, y - 1, z)

let sides = [ Front; Back; Left; Right; Up; Down ]

let input =
    Helpers.readInput 18
    |> Seq.map (fun line -> line.Split(',') |> Seq.map int |> Seq.toList)
    |> Seq.map (fun parts -> (parts[0], parts[1], parts[2]))
    |> Seq.toArray

let minX = input |> Seq.map (fun (x, y, z) -> x) |> Seq.min
let minY = input |> Seq.map (fun (x, y, z) -> y) |> Seq.min
let minZ = input |> Seq.map (fun (x, y, z) -> z) |> Seq.min
let maxX = input |> Seq.map (fun (x, y, z) -> x) |> Seq.max
let maxY = input |> Seq.map (fun (x, y, z) -> y) |> Seq.max
let maxZ = input |> Seq.map (fun (x, y, z) -> z) |> Seq.max

let grid = Array3D.create (maxX + 1) (maxY + 1) (maxZ + 1) false
input |> Seq.iter (fun (x, y, z) -> Array3D.set grid x y z true)

let isNotOccupied (x, y, z) =
    x < minX
    || y < minY
    || z < minZ
    || x > maxX
    || y > maxY
    || z > maxZ
    || not (Array3D.get grid x y z)

let isInBounds (x, y, z) =
    let result =
        x >= -1
        && x <= Array3D.length1 grid
        && y >= -1
        && y <= Array3D.length2 grid
        && z >= -1
        && z <= Array3D.length3 grid

    // if not result then
    //     printfn "uchh"

    result


let part1 =
    let result =
        input
        |> Seq.map (fun (x, y, z) -> sides |> Seq.map (getCubeOnSide (x, y, z)) |> Seq.filter isNotOccupied)
        |> Seq.sumBy (fun uncoveredSides -> uncoveredSides |> Seq.length)

    result


let part2 =
    // For each cube inside the grid, check if its reachable from the outside.

    let neighborsFn node =
        let neighbors =
            sides
            |> Seq.map (getCubeOnSide node)
            |> Seq.filter isNotOccupied
            |> Seq.filter isInBounds

        neighbors

    let heuristicFn (x1, y1, z1) (x2, y2, z2) =
        abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

    let costFn fromNode toNode = 1
    let start = (-1, -1, -1)

    let pathFn = Day12.findShortestPathGeneric neighborsFn heuristicFn costFn start

    [ 0..maxX ]
    |> Seq.iter (fun x ->
        [ 0..maxY ]
        |> Seq.iter (fun y ->
            [ 0..maxZ ]
            |> Seq.iter (fun z ->
                if isNotOccupied (x, y, z) then
                    match pathFn (x, y, z) with
                    | None ->
                        Array3D.set grid x y z true
                        printfn $"no path to (%d{x}, %d{y}, %d{z})"
                    | _ -> ())

        )

    )

    let result =
        input
        |> Seq.map (fun (x, y, z) -> sides |> Seq.map (getCubeOnSide (x, y, z)) |> Seq.filter isNotOccupied)
        |> Seq.sumBy (fun uncoveredSides -> uncoveredSides |> Seq.length)


    result
