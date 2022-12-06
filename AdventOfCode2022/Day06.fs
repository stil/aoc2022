module Day06

let input = Helpers.readInput 6 |> List.head

let markerPosition markerLength =
    [ markerLength + 1 .. input.Length - 1 ]
    |> Seq.find (fun i -> Set input[i - markerLength .. i - 1] |> Set.count = markerLength)

let part1 = markerPosition 4
let part2 = markerPosition 14
