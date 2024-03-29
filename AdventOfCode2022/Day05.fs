module Day05

let split (delim: string) (str: string) = str.Split(delim)
let remove (oldValue: string) (str: string) = str.Replace(oldValue, "")

let initialCrateStacks () =
    let input = Helpers.readInput 5

    [ 1..9 ]
    |> List.map (fun col ->
        [ 1..8 ]
        |> List.map (fun row -> input[row - 1][col * 4 - 3])
        |> List.filter (fun c -> c <> ' '))

let topCrates collectFn =
    Helpers.readInput 5
    |> Seq.skipWhile (fun line -> line.Length > 0)
    |> Seq.skip 1
    |> Seq.map (remove "move " >> remove "from " >> remove "to " >> split " " >> Array.map int)
    |> Seq.map (fun parts -> (parts[0], parts[1], parts[2]))
    |> Seq.collect collectFn
    |> Seq.fold
        (fun (previousStacks: char list list) (howMany, moveFrom, moveTo) ->
            previousStacks
            |> List.updateAt
                (moveTo - 1)
                (previousStacks[moveFrom - 1][0 .. (howMany - 1)] @ previousStacks[moveTo - 1])
            |> List.updateAt (moveFrom - 1) (previousStacks[moveFrom - 1][howMany..]))
        (initialCrateStacks ())
    |> Seq.map (Seq.head >> string)
    |> String.concat ""

let part1 () =
    topCrates (fun (howMany, moveFrom, moveTo) -> Seq.init howMany (fun _ -> (1, moveFrom, moveTo)))

let part2 () = topCrates (fun instr -> [ instr ])
