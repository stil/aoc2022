module Day05

let partial = Helpers.readInputFull 5
let split (delim: string) (str: string) = str.Split(delim)
let replace (oldValue: string) (newValue: string) (str: string) = str.Replace(oldValue, newValue)
let parts = partial |> split "\r\n\r\n" |> Seq.map (split "\r\n") |> Seq.toList
let concat (chars: char[]) = System.String.Concat(chars)

let stackInput = parts[0]

let initialCrateStacks =
    [ 0..8 ]
    |> Seq.map (fun col ->
        [ 0..7 ]
        |> Seq.map (fun row -> stackInput[row][col * 4 + 1])
        |> Seq.filter (fun c -> c <> ' ')
        |> Seq.toList)
    |> Seq.toList


let instructions =
    parts[1]
    |> Seq.map (replace "move " "" >> replace " to " " " >> replace " from " " ")
    |> Seq.map (split " " >> Seq.map int >> Seq.toList)
    |> Seq.map (fun parts -> (parts[0], parts[1], parts[2]))
    |> Seq.toList

let crateStacks collectFn =
    instructions
    |> Seq.collect collectFn
    |> Seq.fold
        (fun (previousStacks: char list list) (howMany, moveFrom, moveTo) ->
            let slice = previousStacks[moveFrom - 1][0 .. (howMany - 1)]
            let newTargetStack = slice @ previousStacks[moveTo - 1]
            let newSourceStack = previousStacks[moveFrom - 1] |> List.removeManyAt 0 howMany

            previousStacks
            |> List.updateAt (moveTo - 1) newTargetStack
            |> List.updateAt (moveFrom - 1) newSourceStack)
        initialCrateStacks
    |> Seq.map (fun stack -> stack |> Seq.head)
    |> Seq.toArray

let part1 =
    let collectFn =
        (fun (howMany, moveFrom, moveTo) -> [ 1..howMany ] |> Seq.map (fun _ -> (1, moveFrom, moveTo)))

    crateStacks collectFn |> concat

let part2 = crateStacks (fun instr -> [ instr ]) |> concat
