module Input

let readInput (day: int) =
    let readLines filePath = System.IO.File.ReadLines(filePath)

    let num = sprintf "%02i" day

    readLines $"Days\\Day{num}.txt"
