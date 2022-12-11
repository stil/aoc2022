module Day11

// a * divisorsMultiplied + b
type SpecialNumber = uint64 * uint64

type Monkey =
    { Number: int
      Items: SpecialNumber list
      OperationFn: SpecialNumber -> SpecialNumber
      Divisor: uint64
      MonkeyNumWhenTrue: int
      MonkeyNumWhenFalse: int
      InspectionCount: uint64 }

let divisorsProduct =
    Helpers.readInput 11
    |> Seq.filter (fun line -> line.Contains("Test: divisible by"))
    |> Seq.map (fun line -> uint64 (line.Replace("  Test: divisible by ", "")))
    |> Seq.reduce (fun a b -> a * b)

let uint64ToSpecialNumber (input: uint64) =
    let quotient = input / divisorsProduct
    let remainder = input % divisorsProduct
    let quotient = min quotient 1UL // Reduce.
    (quotient, remainder)

let specialNumberToUint64 ((quotient, remainder): SpecialNumber) = quotient * divisorsProduct + remainder

let binaryFunctionOnIntegers left right fn =
    let result = fn (specialNumberToUint64 left) (specialNumberToUint64 right)
    uint64ToSpecialNumber result

let add left right = binaryFunctionOnIntegers left right (+)
let multiply left right = binaryFunctionOnIntegers left right (*)

let divideByInt divident (divisor: uint64) =
    ((divident |> specialNumberToUint64) / divisor) |> uint64ToSpecialNumber

let divisibilityTest divident (divisor: uint64) =
    (specialNumberToUint64 divident) % divisor = 0UL

let monkeys =
    (Helpers.readInputFull 11).Split("\r\n\r\n")
    |> Seq.map (fun m ->
        let lines = m.Split("\r\n")

        let operationParts = lines[ 2 ].Replace("  Operation: new = old ", "").Split(' ')

        let operationFn =
            match operationParts[0] with
            | "+" -> add
            | "*" -> multiply
            | _ -> failwith "Unsupported operator."

        let partialOperationFn left =
            match operationParts[1] with
            | "old" -> operationFn left left
            | _ -> operationFn left (uint64ToSpecialNumber (uint64 operationParts[1]))

        { Number = int (lines[ 0 ].Replace("Monkey ", "").Replace(":", ""))
          Items =
            lines[ 1 ].Replace("  Starting items: ", "").Split(", ")
            |> Seq.map (fun v -> (uint64ToSpecialNumber (uint64 (int v))))
            |> Seq.toList
          OperationFn = partialOperationFn
          Divisor = uint64 (lines[ 3 ].Replace("  Test: divisible by ", ""))
          MonkeyNumWhenTrue = int (lines[ 4 ].Replace("    If true: throw to monkey ", ""))
          MonkeyNumWhenFalse = int (lines[ 5 ].Replace("    If false: throw to monkey ", ""))
          InspectionCount = 0UL })
    |> Seq.toList

let solve roundCount afterInspectionFn =
    let playSingleItem (monkeyList: Monkey list) itemWorryLevel monkeyNum =
        let monkey = monkeyList[monkeyNum]
        let worryLevelAtInspection = monkey.OperationFn itemWorryLevel
        let worryLevelAfterInspection = afterInspectionFn worryLevelAtInspection

        let targetMonkeyNum =
            match divisibilityTest worryLevelAfterInspection monkey.Divisor with
            | true -> monkey.MonkeyNumWhenTrue
            | false -> monkey.MonkeyNumWhenFalse

        let targetMonkey = monkeyList[targetMonkeyNum]

        let updatedTargetMonkey =
            { targetMonkey with Items = ([ worryLevelAfterInspection ] |> List.append targetMonkey.Items) }

        let updatedCurrentMonkey =
            { monkey with
                Items = (monkey.Items |> List.skip 1)
                InspectionCount = monkey.InspectionCount + 1UL }

        monkeyList
        |> List.updateAt monkey.Number updatedCurrentMonkey
        |> List.updateAt targetMonkeyNum updatedTargetMonkey

    let playRound (monkeys: Monkey list) =
        monkeys
        |> Seq.map (fun monkey -> monkey.Number)
        |> Seq.fold
            (fun (monkeyList: Monkey list) monkeyNum ->
                monkeyList[monkeyNum].Items
                |> Seq.fold (fun monkeyList worryLevel -> playSingleItem monkeyList worryLevel monkeyNum) monkeyList)
            monkeys

    Seq.init roundCount id
    |> Seq.fold (fun monkeyList roundNum -> playRound monkeyList) monkeys
    |> Seq.map (fun m -> m.InspectionCount)
    |> Seq.sortByDescending id
    |> Seq.take 2
    |> Seq.reduce (fun a b -> a * b)

let part1 = solve 20 (fun x -> divideByInt x 3UL)
let part2 = solve 10000 id

Helpers.assertEqual 99840UL part1
Helpers.assertEqual 20683044837UL part2
