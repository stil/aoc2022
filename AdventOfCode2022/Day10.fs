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


let part1 =

    let state = (1, 0, 0)

    let interestingCycles = [ 20..40..220 ] |> Set

    let (xRegister, completedCycles, signalStrengthSum) =
        input
        |> Seq.fold
            (fun (xRegister, completedCycles, signalStrengthSum) instr ->
                let newXRegister =
                    match instr with
                    | ADDX value -> xRegister + value
                    | NOOP -> xRegister

                let newCompletedCycles = completedCycles + 1
                let isInterestingCycle = interestingCycles |> Set.contains newCompletedCycles
                let signalStrength = xRegister * newCompletedCycles

                let newSignalStrengthSum =
                    if isInterestingCycle then
                        signalStrengthSum + signalStrength
                    else
                        signalStrengthSum

                (newXRegister, newCompletedCycles, newSignalStrengthSum))
            state

    signalStrengthSum

let part2 =
    let crt = Array2D.init 6 40 (fun y x -> '.')

    let printCrt crt =
        crt
        |> Array2D.iteri (fun y x v ->
            let str = string v
            printf $"%s{str}"
            if x = Array2D.length2 crt - 1 then printfn "" else ())

    let nextCrtOffset (y, x) =
        let y = if x >= (Array2D.length2 crt - 1) then y + 1 else y
        let x = if x >= (Array2D.length2 crt - 1) then 0 else x + 1
        let y = if y > (Array2D.length1 crt - 1) then 0 else y
        (y, x)

    Helpers.assertEqual (0, 1) (nextCrtOffset (0, 0))
    Helpers.assertEqual (1, 0) (nextCrtOffset (0, 39))
    Helpers.assertEqual (0, 0) (nextCrtOffset (5, 39))


    let startState = (crt, 1, (0, 0))

    let litPixel crt (y, x) =
        Array2D.set crt y x '#'
        crt

    let result =
        input
        |> Seq.fold
            (fun (crt, xRegister, crtOffset) instr ->
                let spriteRange = [ (xRegister - 1) .. (xRegister + 1) ]

                let drawnOffset = spriteRange |> Seq.tryFind (fun o -> o = (snd crtOffset))

                let newCrtOffset = nextCrtOffset crtOffset

                let newXRegister =
                    match instr with
                    | ADDX value -> xRegister + value
                    | NOOP -> xRegister

                let newCrt =
                    match drawnOffset with
                    | Some offset -> litPixel crt (fst crtOffset, offset)
                    | None -> crt

                printCrt newCrt
                printfn ""

                (newCrt, newXRegister, newCrtOffset)

                )
            startState

    printCrt crt


    0
