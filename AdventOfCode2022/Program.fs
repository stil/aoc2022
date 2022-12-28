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

      (15, 1, Day15.part1, "4793062")
      (15, 2, Day15.part2, "10826395253551") ]


allSolutions
|> Seq.iter (fun (day, part, actual, expected) ->
    printfn $"Day %d{day} part %d{part} result: %s{actual}"
    Helpers.assertEqual expected actual)
