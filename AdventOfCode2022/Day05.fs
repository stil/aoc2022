module Day05

let partial = Helpers.readInputFull 5

let parts =
    partial.Split("\r\n\r\n") |> Seq.map (fun p -> p.Split("\r\n")) |> Seq.toArray

let stackInput = parts[0]

let initialStacks () =
    [ 0..8 ]
    |> Seq.map (fun col ->
        [ 0..7 ]
        |> Seq.map (fun row -> stackInput[row][col * 4 + 1])
        |> Seq.filter (fun c -> not (c = ' '))
        |> Seq.toList)
    |> Seq.toArray


let instructions =
    parts[1]
    |> Seq.map (fun instr ->
        instr
            .Replace("move ", "")
            .Replace(" from ", " ")
            .Replace(" to ", " ")
            .Split(' '))
    |> Seq.map (fun parts -> parts |> Seq.map int |> Seq.toArray)
    |> Seq.map (fun parts -> (parts[0], parts[1], parts[2]))
    |> Seq.toArray

let finalStacks collectFn =
    instructions
    |> Seq.collect collectFn
    |> Seq.fold
        (fun (previousStacks: char list[]) (howMany, moveFrom, moveTo) ->



            let slice = previousStacks[moveFrom - 1][0 .. (howMany - 1)]
            let newTargetStack = [ slice; previousStacks[moveTo - 1] ] |> List.concat
            let newSourceStack = previousStacks[moveFrom - 1] |> List.removeManyAt 0 howMany
            Array.set previousStacks (moveTo - 1) newTargetStack
            Array.set previousStacks (moveFrom - 1) newSourceStack
            previousStacks)
        (initialStacks ())
    |> Seq.map (fun stack -> stack |> Seq.head)
    |> Seq.toArray

let part1 =
    let resultStacks =
        finalStacks (fun (howMany, moveFrom, moveTo) -> [ 1..howMany ] |> Seq.map (fun _ -> (1, moveFrom, moveTo)))

    System.String.Concat(resultStacks)

let part2 =
    let resultStacks = finalStacks (fun instr -> [ instr ])

    System.String.Concat(resultStacks)
