module Helpers

let readInput (day: int) =
    let readLines filePath = System.IO.File.ReadLines(filePath)

    let num = sprintf "%02i" day

    readLines $"Days\\Day{num}.txt"

let readInputFull (day: int) =
    let readLines filePath = System.IO.File.ReadAllText(filePath)

    let num = sprintf "%02i" day

    readLines $"Days\\Day{num}.txt"
