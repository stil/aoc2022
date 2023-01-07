module Day01

let partialResult () =
    (Helpers.readInputFull 1).Split("\r\n\r\n")
    |> Seq.map (fun elf -> elf.Split("\r\n") |> Seq.sumBy int)
    |> Seq.sortDescending

let part1 () = partialResult () |> Seq.head |> string

let part2 () =
    partialResult () |> Seq.take 3 |> Seq.sum |> string
