module Day03

let getItemPriority symbol =
    if System.Char.IsLower symbol then
        int symbol - int 'a' + 1
    else
        int symbol - int 'A' + 27

let part1 () =
    Helpers.readInput 3
    |> Seq.map (fun e -> e |> Seq.chunkBySize (e.Length / 2) |> Seq.map Set)
    |> Seq.map Set.intersectMany
    |> Seq.map Seq.head
    |> Seq.sumBy getItemPriority

let part2 () =
    Helpers.readInput 3
    |> Seq.chunkBySize 3
    |> Seq.map (Seq.map Set)
    |> Seq.map Set.intersectMany
    |> Seq.map Seq.head
    |> Seq.sumBy getItemPriority
