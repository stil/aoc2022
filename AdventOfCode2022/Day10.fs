module Day10

type Instruction =
    | ADDX of int
    | NOOP

let input =
    Helpers.readInput 10
    |> Seq.collect (fun line ->
        let parts = line.Split(' ')

        match parts[0] with
        | "addx" -> [ NOOP; ADDX(int parts[1]) ]
        | "noop" -> [ NOOP ]
        | _ -> failwith "Unrecognized instruction.")
    |> Seq.fold
        (fun cycleList instr ->
            let currentX = cycleList |> List.last

            let nextX =
                match instr with
                | ADDX value -> currentX + value
                | NOOP -> currentX

            cycleList @ [ nextX ])
        [ 1 ]

let part1 =
    let interestingCycles = [ 20..40..220 ] |> Set

    input
    |> Seq.fold
        (fun (signalStrengthSum, cycles) xRegister ->
            let newCycles = cycles + 1
            let signalStrength = xRegister * newCycles

            let newSignalStrengthSum =
                match interestingCycles |> Set.contains newCycles with
                | true -> signalStrengthSum + signalStrength
                | false -> signalStrengthSum

            (newSignalStrengthSum, newCycles))
        (0, 0)
    |> fst

let part2 =
    let crt = Array2D.init 6 40 (fun _ _ -> '.')

    let nextCrtOffset (y, x) =
        let y = if x >= (Array2D.length2 crt - 1) then y + 1 else y
        let x = (x + 1) % (Array2D.length2 crt)
        let y = y % (Array2D.length1 crt)
        (y, x)

    input
    |> Seq.fold
        (fun (crtY, crtX) xRegister ->
            if crtX >= (xRegister - 1) && crtX <= (xRegister + 1) then
                Array2D.set crt crtY crtX '#'

            nextCrtOffset (crtY, crtX))
        (0, 0)
    |> ignore

    let printCrt () =
        crt
        |> Array2D.iteri (fun y x v ->
            printf $"%s{(string v)}"
            if x = Array2D.length2 crt - 1 then printfn "" else ())

    printCrt ()
    0

Helpers.assertEqual 12560 part1
