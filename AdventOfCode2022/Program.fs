open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

printfn $"Part 1: %i{Day03.part1 ()}"
printfn $"Part 2: %i{Day03.part2 ()}"

[<MemoryDiagnoser>]
type DayBenchmark() =
    [<Benchmark>]
    member _.Part1() = Day01.part1 ()

    [<Benchmark>]
    member _.Part2() = Day01.part2 ()

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<DayBenchmark>() |> ignore
    0 // return an integer exit code
