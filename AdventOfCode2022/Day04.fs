module Day04

let parseTuple (str: string) (separator: char) mapFn =
    let parts = str.Split(separator) |> Seq.map mapFn |> Seq.toArray
    (parts[0], parts[1])

let parsePair pair = parseTuple pair ',' id
let parseRange range = parseTuple range '-' int

let assignmentPairs =
    Helpers.readInput 4
    |> Seq.map parsePair
    |> Seq.map (fun (elf1, elf2) -> (parseRange elf1, parseRange elf2))
    |> Seq.map (fun (elf1, elf2) -> (seq { fst elf1 .. snd elf1 }, seq { fst elf2 .. snd elf2 }))
    |> Seq.map (fun (elf1, elf2) -> (Set elf1, Set elf2))

let part1 =
    assignmentPairs
    |> Seq.filter (fun elfs -> elfs ||> Set.isSubset || elfs ||> Set.isSuperset)
    |> Seq.length

let part2 =
    assignmentPairs
    |> Seq.filter (fun elfs -> elfs ||> Set.intersect |> Set.count > 0)
    |> Seq.length
