module Day09

let step (dirY, dirX) y0 x0 = (y0 + dirY, x0 + dirX)

let input =
    Helpers.readInput 9
    |> Seq.map (fun line ->
        (int line[2..],
         match line[0] with
         | 'R' -> step (0, 1)
         | 'L' -> step (0, -1)
         | 'U' -> step (1, 0)
         | 'D' -> step (-1, 0)
         | _ -> failwith "Unsupported direction."))
    |> Seq.collect (fun instr -> instr ||> Seq.replicate)

let distance y0 x0 y1 x1 = (y1 - y0, x1 - x0)
let isTouching (x, y) = abs x <= 1 && abs y <= 1

let clamp value =
    if value >= 0 then min 1 value else max -1 value

let pullVector (x, y) = (clamp -x, clamp -y)

let findNextTailPos (tailPos: int * int) (nextHeadPos: int * int) =
    let distance = tailPos ||> (nextHeadPos ||> distance)
    let isTouching = isTouching distance

    if not isTouching then
        let pullVector = pullVector distance
        (fst tailPos + fst pullVector, snd tailPos + snd pullVector)
    else
        tailPos

let adjustRope rope =
    [ 0 .. (rope |> List.length) - 2 ]
    |> Seq.fold
        (fun accRope linkIndex ->
            let headPos = accRope |> List.skip linkIndex |> List.head
            let tailPos = accRope |> List.skip (linkIndex + 1) |> List.head
            let nextTailPos = findNextTailPos tailPos headPos
            accRope |> List.updateAt (linkIndex + 1) nextTailPos)
        rope

let countUniqueVisitedByTail ropeLength =
    let rope = List.replicate ropeLength (0, 0)

    let finalRope, visitedByTail =
        ((rope, []), input)
        ||> Seq.fold (fun (rope, visited) stepFn ->
            let newRope = rope |> List.updateAt 0 (rope |> List.head ||> stepFn) |> adjustRope
            let newRopeTail = newRope |> List.last
            (newRope, newRopeTail :: visited))

    visitedByTail |> Set |> Set.count

let part1 = countUniqueVisitedByTail 2
let part2 = countUniqueVisitedByTail 10

Helpers.assertEqual 6026 part1
Helpers.assertEqual 2273 part2
