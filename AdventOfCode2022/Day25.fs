module Day25

let part1 =
    let snafuToDec snafu =
        snafu
        |> Seq.rev
        |> Seq.mapi (fun i c -> (i, c))
        |> Seq.sumBy (fun (i, c) ->
            (pown 5L i)
            * match c with
              | '2' -> 2L
              | '1' -> 1L
              | '0' -> 0L
              | '-' -> -1L
              | '=' -> -2L
              | _ -> failwith "Unexpected character.")

    let resultDec = Helpers.readInput 25 |> Seq.map snafuToDec |> Seq.sum

    let decToSnafu (dec: int64) =
        let mutable number = dec
        let mutable digits = []

        while number > 0 do
            let divisionResult = int64 (round (float number / 5.0))
            let divisionRemainder = number - (divisionResult * 5L)
            digits <- divisionRemainder :: digits
            number <- divisionResult

        let snafu =
            digits
            |> Seq.map (fun d ->
                match int d with
                | -2 -> '='
                | -1 -> '-'
                | 0 -> '0'
                | 1 -> '1'
                | 2 -> '2'
                | _ -> failwith "Unexpected number.")

        System.String.Concat(snafu)

    let result = decToSnafu resultDec

    result


let part2 = 0
