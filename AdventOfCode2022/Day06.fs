module Day06

let input = Helpers.readInput 6 |> Seq.head

let markerPosition markerLength =
    let partial =
        input
        |> Seq.mapi (fun i char -> (i, char))
        |> Seq.map (fun (i, _) -> (i, Seq.init markerLength (fun off -> input[off + i])))
        |> Seq.filter (fun (pos, coll) -> coll |> Seq.distinct |> Seq.length = markerLength)
        |> Seq.head

    fst partial + markerLength

let part1 = markerPosition 4
let part2 = markerPosition 14
