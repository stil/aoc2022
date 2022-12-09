module Day09

let step (dirY, dirX) y0 x0 = (y0 + dirY, x0 + dirX)
let stepUp = step (1, 0)
let stepDown = step (-1, 0)
let stepRight = step (0, 1)
let stepLeft = step (0, -1)

let input =
    Helpers.readInput 9
    |> Seq.collect (fun line ->
        let steps = int line[2..]

        let stepFn =
            match line[0] with
            | 'R' -> stepRight
            | 'L' -> stepLeft
            | 'U' -> stepUp
            | 'D' -> stepDown
            | _ -> failwith "Unsupported direction."

        Seq.replicate steps stepFn)
    |> Seq.toList

let part1 =
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

    let (finalHeadPos, finalTailPos, finalVisited) =
        (((0, 0), (0, 0), []), input)
        ||> Seq.fold (fun (headPos, tailPos, visited) stepFn ->
            let nextHeadPos = headPos ||> stepFn
            let nextTailPos = findNextTailPos tailPos nextHeadPos
            (nextHeadPos, nextTailPos, nextTailPos :: visited))

    let uniqueVisited = finalVisited |> Set |> Set.count

    uniqueVisited

let part2 =
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

    let rec adjustRope rope =
        let visitedLinkIndices = [ 0 .. (rope |> List.length) - 2 ]

        let adj rope =
            visitedLinkIndices
            |> Seq.fold
                (fun accRope linkIndex ->
                    let headPos = accRope |> List.skip linkIndex |> List.head
                    let tailPos = accRope |> List.skip (linkIndex + 1) |> List.head
                    let nextTailPos = findNextTailPos tailPos headPos
                    let newRope = accRope |> List.updateAt (linkIndex + 1) nextTailPos
                    newRope)
                rope

        let newRope =
            rope |> adj |> adj |> adj |> adj |> adj |> adj |> adj |> adj |> adj |> adj

        newRope

    // head to tail order
    let countVisitedByTail ropeLength =
        let rope = Seq.replicate ropeLength (0, 0) |> Seq.toList

        let (finalRope, visitedByTail) =
            ((rope, []), input)
            ||> Seq.fold (fun (rope, visited) stepFn ->
                let newRope = rope |> List.updateAt 0 (rope |> List.head ||> stepFn) |> adjustRope
                let newRopeTail = newRope |> List.last
                (newRope, newRopeTail :: visited))

        let uniqueVisited = visitedByTail |> Set |> Set.count

        uniqueVisited

    let result = countVisitedByTail 10
    Helpers.assertEqual 6026 (countVisitedByTail 2)
    Helpers.assertEqual 2273 (countVisitedByTail 10)

    result
