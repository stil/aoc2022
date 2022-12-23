module Day18

type Cube = int * int * int

type Side =
    | Front
    | Back
    | Left
    | Right
    | Up
    | Down

let part1 =
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

    // Left handed cartesian coordinates.
    let getCubeOnSide (x, y, z) side =
        match side with
        | Front -> (x, y, z - 1)
        | Back -> (x, y, z + 1)
        | Left -> (x - 1, y, z)
        | Right -> (x + 1, y, z)
        | Up -> (x, y + 1, z)
        | Down -> (x, y - 1, z)

    let grid = Array3D.create (maxX + 1) (maxY + 1) (maxZ + 1) false

    input |> Seq.iter (fun (x, y, z) -> Array3D.set grid x y z true)

    let isTaken (x, y, z) =
        x < minX
        || y < minY
        || z < minZ
        || x > maxX
        || y > maxY
        || z > maxZ
        || not (Array3D.get grid x y z)

    let sides = [ Front; Back; Left; Right; Up; Down ]

    let result =
        input
        |> Seq.map (fun (x, y, z) -> sides |> Seq.map (getCubeOnSide (x, y, z)) |> Seq.filter isTaken)
        |> Seq.sumBy (fun uncoveredSides -> uncoveredSides |> Seq.length)

    result


let part2 = 0
