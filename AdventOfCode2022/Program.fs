open System.Diagnostics

let allSolutions =
    [ (01, 1, Day01.part1, "71471")
      (01, 2, Day01.part2, "211189")

      (02, 1, Day02.part1, "13009")
      (02, 2, Day02.part2, "10398")

      (03, 1, Day03.part1, "7826")
      (03, 2, Day03.part2, "2577")

      (04, 1, Day04.part1, "524")
      (04, 2, Day04.part2, "798")

      (05, 1, Day05.part1, "CVCWCRTVQ")
      (05, 2, Day05.part2, "CNSCZWLVT")

      (06, 1, Day06.part1, "1343")
      (06, 2, Day06.part2, "2193")

      (07, 1, Day07.part1, "2104783")
      (07, 2, Day07.part2, "5883165")

      (08, 1, Day08.part1, "1763")
      (08, 2, Day08.part2, "671160")

      (09, 1, Day09.part1, "6026")
      (09, 2, Day09.part2, "2273")

      (10, 1, Day10.part1, "12560")
      (10, 2, Day10.part2, "PLPAFBCL")

      (11, 1, Day11.part1, "99840")
      (11, 2, Day11.part2, "20683044837")

      (12, 1, Day12.part1, "504")
      (12, 2, Day12.part2, "500")

      (13, 1, Day13.part1, "4643")
      (13, 2, Day13.part2, "21614")

      (14, 1, Day14.part1, "728")
      (14, 2, Day14.part2, "27623")

      (15, 1, Day15.part1, "4793062") // 104 s
      (15, 2, Day15.part2, "10826395253551") // 11 s

      (16, 1, Day16.part1, "1880")
      (16, 2, Day16.part2, "2520") // 568 s ~ 9 min

      (17, 1, Day17.part1, "3114")
      (17, 2, Day17.part2, "1540804597682")

      (18, 1, Day18.part1, "4340")
      (18, 2, Day18.part2, "2468") // 179 s

      (19, 1, Day19.part1, "2160") // 18 s
      (19, 2, Day19.part2, "13340") // 761 s ~ 12 min

      (20, 1, Day20.part1, "8302")
      (20, 2, Day20.part2, "656575624777")

      (21, 1, Day21.part1, "194058098264286")
      (21, 2, Day21.part2, "3592056845086")

      (22, 1, Day22.part1, "196134")
      (22, 2, Day22.part2, "146011")

      (23, 1, Day23.part1, "3864") // 25 s
      (23, 2, Day23.part2, "946") // 16 s

      (24, 1, Day24.part1, "322") // 12 s
      (24, 2, Day24.part2, "974") // 114 s

      (25, 1, Day25.part1, "2=020-===0-1===2=020") ]


allSolutions
|> Seq.iter (fun (day, part, solutionFn, expected) ->
    let watch = Stopwatch.StartNew()
    let actual = solutionFn ()
    printfn $"Day %d{day} part %d{part} result: %s{actual}, solved in %d{watch.ElapsedMilliseconds} ms."
    Helpers.assertEqual expected actual)
