module Day06

let markerPosition markerLength =
    let input = Helpers.readInput 6 |> List.head

    [ markerLength + 1 .. input.Length - 1 ]
    |> Seq.find (fun i -> Set input[i - markerLength .. i - 1] |> Set.count = markerLength)

let part1 () = markerPosition 4 |> string
let part2 () = markerPosition 14 |> string
