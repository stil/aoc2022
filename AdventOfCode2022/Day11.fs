module Day11

// a * divisorsProduct + b
type ComplexNumber = uint64 * uint64

type Monkey =
    { Items: ComplexNumber list
      WorryOperation: ComplexNumber -> ComplexNumber
      Divisor: uint64
      MonkeyNumWhenTrue: int
      MonkeyNumWhenFalse: int
      InspectionCount: uint64 }

let divisorsProduct =
    Helpers.readInput 11
    |> Seq.filter (fun line -> line.Contains("Test: divisible by"))
    |> Seq.map (fun line -> uint64 (line.Replace("  Test: divisible by ", "")))
    |> Seq.reduce (fun a b -> a * b)

let intToComplexNum (input: uint64) =
    let quotient = (input / divisorsProduct) |> min 1UL // Reduce.
    let remainder = input % divisorsProduct
    (quotient, remainder)

let complexNumToInt ((quotient, remainder): ComplexNumber) = quotient * divisorsProduct + remainder
let mapTuple fn (a, b) = (fn a, fn b)

let binaryFunctionOnIntegers left right fn =
    (left, right) |> mapTuple complexNumToInt ||> fn |> intToComplexNum

let add left right = binaryFunctionOnIntegers left right (+)
let multiply left right = binaryFunctionOnIntegers left right (*)

let divisibilityTest divident divisor =
    (complexNumToInt divident) % divisor = 0UL

let monkeys () =
    (Helpers.readInputFull 11).Split("\r\n\r\n")
    |> Seq.map (fun m ->
        let lines = m.Split("\r\n")

        let operationParts = lines[ 2 ].Replace("  Operation: new = old ", "").Split(' ')

        let partialOperationFn =
            match operationParts[0] with
            | "+" -> add
            | "*" -> multiply
            | _ -> failwith "Unsupported operator."

        let operationFn left =
            match operationParts[1] with
            | "old" -> partialOperationFn left left
            | _ -> partialOperationFn left (intToComplexNum (uint64 operationParts[1]))

        { Items =
            lines[ 1 ].Replace("  Starting items: ", "").Split(", ")
            |> Seq.map (fun v -> (intToComplexNum (uint64 (int v))))
            |> Seq.toList
          WorryOperation = operationFn
          Divisor = uint64 (lines[ 3 ].Replace("  Test: divisible by ", ""))
          MonkeyNumWhenTrue = int (lines[ 4 ].Replace("    If true: throw to monkey ", ""))
          MonkeyNumWhenFalse = int (lines[ 5 ].Replace("    If false: throw to monkey ", ""))
          InspectionCount = 0UL })
    |> Seq.toList

let solve roundCount afterInspectionFn =
    let advanceSingleItem (monkeys: Monkey list) monkeyNum worry =
        let monkey = monkeys[monkeyNum]
        let worry = worry |> monkey.WorryOperation |> afterInspectionFn

        let targetMonkeyNum =
            match divisibilityTest worry monkey.Divisor with
            | true -> monkey.MonkeyNumWhenTrue
            | false -> monkey.MonkeyNumWhenFalse

        let targetMonkey = monkeys[targetMonkeyNum]

        let updatedTargetMonkey =
            { targetMonkey with Items = ([ worry ] |> List.append targetMonkey.Items) }

        let updatedCurrentMonkey =
            { monkey with
                Items = (monkey.Items |> List.skip 1)
                InspectionCount = monkey.InspectionCount + 1UL }

        monkeys
        |> List.updateAt monkeyNum updatedCurrentMonkey
        |> List.updateAt targetMonkeyNum updatedTargetMonkey

    let advanceMonkey (monkeys: Monkey list) monkeyNum =
        monkeys[monkeyNum].Items
        |> Seq.fold (fun monkeys -> advanceSingleItem monkeys monkeyNum) monkeys

    let advanceRound (monkeys: Monkey list) =
        monkeys
        |> Seq.mapi (fun i _ -> i)
        |> Seq.fold (fun (monkeys: Monkey list) -> advanceMonkey monkeys) monkeys

    Seq.init roundCount id
    |> Seq.fold (fun monkeys roundNum -> advanceRound monkeys) (monkeys ())
    |> Seq.map (fun m -> m.InspectionCount)
    |> Seq.sortByDescending id
    |> Seq.take 2
    |> Seq.reduce (fun a b -> a * b)

let part1 () =
    solve 20 (fun x -> ((x |> complexNumToInt) / 3UL) |> intToComplexNum) |> string

let part2 () = solve 10000 id |> string
