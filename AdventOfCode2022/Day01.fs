module Day01

let partialResult =
    (Helpers.readInputFull 1).Split("\r\n\r\n")
    |> Seq.map (fun elf -> elf.Split("\r\n") |> Seq.sumBy int)
    |> Seq.sortDescending

let part1 =
    let result = partialResult |> Seq.head
    printfn $"Result: %i{result}."

let part2 =
    let result = partialResult |> Seq.take 3 |> Seq.sum
    printfn $"Result: %i{result}."
