module Helpers

open System.IO
open System.Reflection

let assembly = Assembly.GetExecutingAssembly()

let dayFromResourceName (resName: string) =
    let str = resName.Replace("AdventOfCode2022.Input.Day", "").Replace(".txt", "")
    int str

let inputs =
    assembly.GetManifestResourceNames()
    |> Seq.filter (fun res -> res.Contains("Day") && res.EndsWith(".txt"))
    |> Seq.map (fun res ->
        (res |> dayFromResourceName,
         let stream = assembly.GetManifestResourceStream(res)
         let streamReader = new StreamReader(stream)
         streamReader.ReadToEnd()))
    |> dict

let rec readlines (stream: StreamReader) =
    seq {
        let line = stream.ReadLine()

        if line <> null then
            yield line
            yield! readlines (stream)
    }

let readInput (day: int) = inputs[ day ].Split("\r\n")

let readInputFull (day: int) = inputs[day]
