module Day08

let input = Helpers.readInput 8

let getTreeHeight y x = int (input[y][x] - '0')
let height = input.Length
let width = input[0].Length

let range minIncl maxExcl = { minIncl .. (maxExcl - 1) }

let outOfBounds y x =
    y < 0 || x < 0 || y >= height || x >= width

let walkPath (dirY, dirX) y0 x0 =
    (y0, x0)
    |> Seq.unfold (fun (y, x) ->
        if outOfBounds y x then
            None
        else
            Some((y, x), (y + dirY, x + dirX)))

let walkDown = walkPath (1, 0)
let walkRight = walkPath (0, 1)
let walkUp = walkPath (-1, 0)
let walkLeft = walkPath (0, -1)

let part1 () =
    let visibleTrees ray =
        ray
        |> Seq.fold
            (fun (tallerList, maxHeightSoFar) (y, x) ->
                let height = getTreeHeight y x
                let taller = height > maxHeightSoFar
                ((if taller then (y, x) :: tallerList else tallerList), max height maxHeightSoFar))
            ([], -1)
        |> fst

    let down = range 0 width |> Seq.map (fun x -> (0, x) ||> walkDown)
    let right = range 0 height |> Seq.map (fun y -> (y, 0) ||> walkRight)
    let up = down |> Seq.map Seq.rev
    let left = right |> Seq.map Seq.rev

    let rays = [ down; up; right; left ] |> Seq.concat
    rays |> Seq.map visibleTrees |> Seq.collect id |> Set |> Set.count |> string

let part2 () =
    let viewingDistance treeHeight ray =
        ray
        |> Seq.fold
            (fun (tallerCount, skip) (y, x) ->
                let height = getTreeHeight y x
                let blocked = height >= treeHeight
                ((if skip then tallerCount else tallerCount + 1), (if skip then true else blocked)))
            (0, false)
        |> fst

    (range 0 height, range 0 width)
    ||> Seq.allPairs
    |> Seq.map (fun (houseY, houseX) ->
        [ walkDown; walkRight; walkUp; walkLeft ]
        |> Seq.map (fun walkFn -> (houseY, houseX) ||> walkFn |> Seq.skip 1)
        |> Seq.map (Seq.toList >> viewingDistance (getTreeHeight houseY houseX))
        |> Seq.reduce (*))
    |> Seq.max
    |> string
