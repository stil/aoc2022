module Helpers

let readInput (day: int) =
    let readLines filePath =
        System.IO.File.ReadLines(filePath) |> Seq.toList

    let num = sprintf "%02i" day

    readLines $"Input\\Day{num}.txt"

let readInputFull (day: int) =
    let readLines filePath = System.IO.File.ReadAllText(filePath)

    let num = sprintf "%02i" day

    readLines $"Input\\Day{num}.txt"

type Result =
    | Number of int
    | Text of string

exception InvalidResult of string

let assertEqual expected actual =
    if expected = actual then
        printfn "Result correct."
    else
        raise (InvalidResult($"Invalid result. Expected '{expected}', got '{actual}'."))
