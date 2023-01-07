module Day09

let add (y0, x0) (y1, x1) = (y0 + y1, x0 + x1)
let subtract (y0, x0) (y1, x1) = (y0 - y1, x0 - x1)

let stepFnIntoDirection dirChar =
    match dirChar with
    | 'R' -> add (0, 1)
    | 'L' -> add (0, -1)
    | 'U' -> add (1, 0)
    | 'D' -> add (-1, 0)
    | _ -> failwith "Unsupported direction."

let input () =
    Helpers.readInput 9
    |> Seq.collect (fun line -> (int line[2..], stepFnIntoDirection line[0]) ||> Seq.replicate)

let isTouching (x, y) = abs x <= 1 && abs y <= 1
let clamp value = value |> max -1 |> min 1

let pullVector (x, y) =
    if isTouching (x, y) then (0, 0) else (clamp -x, clamp -y)

let findNextTailPos tailPos headPos =
    let distance = subtract tailPos headPos
    let pullVector = pullVector distance
    add tailPos pullVector

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
        ((rope, []), input ())
        ||> Seq.fold (fun (rope, visited) stepFn ->
            let newRope = rope |> List.updateAt 0 (rope |> List.head |> stepFn) |> adjustRope
            let newRopeTail = newRope |> List.last
            (newRope, newRopeTail :: visited))

    visitedByTail |> Set |> Set.count

let part1 () = countUniqueVisitedByTail 2 |> string
let part2 () = countUniqueVisitedByTail 10 |> string
