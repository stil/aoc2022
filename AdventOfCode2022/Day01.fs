module Day01

let partialResult () =
    (Helpers.readInputFull 1).Split("\r\n\r\n")
    |> Seq.map (fun elf -> elf.Split("\r\n") |> Seq.sumBy int)


let part1 () = partialResult () |> Seq.max

let part2 () =
    partialResult () |> Seq.sortDescending |> Seq.take 3 |> Seq.sum
