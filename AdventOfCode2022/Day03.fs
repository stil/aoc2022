module Day03

open System

let getItemPriority symbol =
    if Char.IsLower symbol then
        int symbol - int 'a' + 1
    else
        int symbol - int 'A' + 27

let part1 =
    let result =
        Helpers.readInput 3
        |> Seq.map (fun e -> e |> Seq.chunkBySize (e.Length / 2) |> Seq.map Set)
        |> Seq.map Set.intersectMany
        |> Seq.map Seq.head
        |> Seq.map getItemPriority
        |> Seq.sum

    printfn $"Result: %i{result}."

let part2 =
    let result =
        Helpers.readInput 3
        |> Seq.chunkBySize 3
        |> Seq.map (Seq.map Set)
        |> Seq.map Set.intersectMany
        |> Seq.map Seq.head
        |> Seq.map getItemPriority
        |> Seq.sum

    printfn $"Result: %i{result}."
