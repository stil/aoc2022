module Day08

let input = Helpers.readInput 8

let grid =
    Array2D.init input.Length input[0].Length (fun y x -> int (input[y][x] - '0'))

let getHeight = Array2D.get grid
let height = Array2D.length1 grid
let width = Array2D.length2 grid

let range minIncl maxExcl =
    Seq.init (maxExcl - minIncl) (fun i -> minIncl + i)

let part1 =
    let down =
        range 0 width |> Seq.map (fun x -> range 0 height |> Seq.map (fun y -> (y, x)))

    let right =
        range 0 height |> Seq.map (fun y -> range 0 width |> Seq.map (fun x -> (y, x)))

    let up = down |> Seq.map Seq.rev
    let left = right |> Seq.map Seq.rev

    let rays = [ down; up; right; left ] |> Seq.concat

    let visibleTrees ray =
        let maxHeightSoFar, tallerCount =
            ray
            |> Seq.fold
                (fun (maxHeightSoFar, tallerList) (y, x) ->
                    let height = getHeight y x
                    let taller = height > maxHeightSoFar
                    (max height maxHeightSoFar, (if taller then (y, x) :: tallerList else tallerList)))
                (-1, [])

        tallerCount

    rays |> Seq.map visibleTrees |> Seq.collect id |> Set |> Set.count

let part2 =
    let viewingDistance treeHeight (ray: (int * int) list) =
        if ray.Length = 0 then
            0
        else
            let tallerCount, blocked =
                ray
                |> Seq.fold
                    (fun (tallerList, skip) (y, x) ->
                        let height = getHeight y x
                        let blocked = height >= treeHeight

                        ((if skip then tallerList else (y, x) :: tallerList), (if skip then true else blocked)))
                    ([], false)

            tallerCount |> Seq.length

    (range 0 height, range 0 width)
    ||> Seq.allPairs
    |> Seq.map (fun (houseY, houseX) ->
        let down = range (houseY + 1) height |> Seq.map (fun y -> (y, houseX))
        let right = range (houseX + 1) width |> Seq.map (fun x -> (houseY, x))
        let up = range 0 houseY |> Seq.map (fun y -> (houseY - 1 - y, houseX))
        let left = range 0 houseX |> Seq.map (fun x -> (houseY, houseX - 1 - x))

        let currentHeight = getHeight houseY houseX

        [ up; down; left; right ]
        |> Seq.map (Seq.toList >> viewingDistance currentHeight)
        |> Seq.reduce (*))
    |> Seq.max

Helpers.assertEqual 1763 part1
Helpers.assertEqual 671160 part2
