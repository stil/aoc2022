module Day04

let parseTuple (str: string) (separator: char) mapFn =
    let parts = str.Split(separator) |> Seq.map mapFn |> Seq.toArray
    (parts[0], parts[1])

let mapPair fn (a, b) = (fn a, fn b)
let parsePair pair = parseTuple pair ',' id
let parseRange range = parseTuple range '-' int

let assignmentPairs =
    Helpers.readInput 4
    |> Seq.map parsePair
    |> Seq.map (mapPair parseRange)
    |> Seq.map (mapPair (fun (min, max) -> Set { min..max }))

let part1 =
    assignmentPairs
    |> Seq.filter (fun elfs -> elfs ||> Set.isSubset || elfs ||> Set.isSuperset)
    |> Seq.length
    |> string

let part2 =
    assignmentPairs
    |> Seq.filter (fun elfs -> elfs ||> Set.intersect |> Set.isEmpty |> not)
    |> Seq.length
    |> string
