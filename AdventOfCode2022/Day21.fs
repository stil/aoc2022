module Day21

open System.Text.RegularExpressions

type Operation =
    | Addition of Value * Value
    | Subtraction of Value * Value
    | Multiplication of Value * Value
    | Division of Value * Value
    | Equality of Value * Value

and Value =
    | ActualNumber of int64
    | Reference of string
    | Operation of Operation

let monkeys =
    Helpers.readInput 21
    |> Seq.map (fun line ->
        let parts = line.Split(": ")

        let operation =
            if Regex.IsMatch(parts[1], "^\d+$") then
                ActualNumber(int parts[1])
            else
                let m = Regex.Match(parts[1], "^([a-z]+) (.) ([a-z]+)$")
                let l = Reference(m.Groups[1].Value)
                let r = Reference(m.Groups[3].Value)

                match m.Groups[2].Value with
                | "+" -> Operation(Addition(l, r))
                | "-" -> Operation(Subtraction(l, r))
                | "*" -> Operation(Multiplication(l, r))
                | "/" -> Operation(Division(l, r))
                | _ -> failwith "Unsupported operation"

        (parts[0], operation))
    |> Seq.toList

type Equation = { Left: Value; Right: Value }

let part1 =
    let resolvePass monkeys =
        let actualNumbersDict =
            monkeys
            |> Seq.map (fun (mName, mValue) ->
                match mValue with
                | ActualNumber num -> Some(mName, num)
                | _ -> None)
            |> Seq.choose id
            |> dict

        let rec resolveValue (value: Value) =
            match value with
            | ActualNumber num -> ActualNumber(num)
            | Reference refId ->
                let found, actualValue = actualNumbersDict.TryGetValue(refId)

                if found then
                    ActualNumber(actualValue)
                else
                    Reference(refId)
            | Operation operation ->
                match operation with
                | Addition (l, r) ->
                    match (l, r) with
                    | ActualNumber ln, ActualNumber rn -> ActualNumber(ln + rn)
                    | _, _ -> Operation(Addition(resolveValue l, resolveValue r))
                | Subtraction (l, r) ->
                    match (l, r) with
                    | ActualNumber ln, ActualNumber rn -> ActualNumber(ln - rn)
                    | _, _ -> Operation(Subtraction(resolveValue l, resolveValue r))
                | Multiplication (l, r) ->
                    match (l, r) with
                    | ActualNumber ln, ActualNumber rn -> ActualNumber(ln * rn)
                    | _, _ -> Operation(Multiplication(resolveValue l, resolveValue r))
                | Division (l, r) ->
                    match (l, r) with
                    | ActualNumber ln, ActualNumber rn -> ActualNumber(ln / rn)
                    | _, _ -> Operation(Division(resolveValue l, resolveValue r))


        let resolved =
            monkeys
            |> Seq.map (fun (mName, mValue) -> (mName, resolveValue mValue))
            |> Seq.toList

        resolved

    let monkeysResolved =
        Seq.init 5000 id
        |> Seq.fold (fun acc _ -> resolvePass acc) monkeys
        |> Seq.toList

    let rootValue =
        monkeysResolved |> List.find (fun (mName, _) -> mName = "root") |> snd

    let result =
        match rootValue with
        | ActualNumber result -> result
        | _ -> failwith "Could not solve."

    result

let part2 =
    let monkeys =
        Helpers.readInput 21
        |> Seq.map (fun line ->
            let parts = line.Split(": ")

            let operation =
                if parts[0] = "humn" then
                    Reference("XXXX")
                elif Regex.IsMatch(parts[1], "^\d+$") then
                    ActualNumber(int parts[1])
                else
                    let m = Regex.Match(parts[1], "^([a-z]+) (.) ([a-z]+)$")
                    let l = Reference(m.Groups[1].Value)
                    let r = Reference(m.Groups[3].Value)

                    match m.Groups[2].Value with
                    | "+" ->
                        if parts[0] = "root" then
                            Operation(Equality(l, r))
                        else
                            Operation(Addition(l, r))
                    | "-" -> Operation(Subtraction(l, r))
                    | "*" -> Operation(Multiplication(l, r))
                    | "/" -> Operation(Division(l, r))
                    | _ -> failwith "Unsupported operation"

            (parts[0], operation))
        |> Seq.toList

    let resolvePass monkeys =
        let actualNumbersDict =
            monkeys
            |> Seq.map (fun (mName, mValue) ->
                match mValue with
                | ActualNumber num -> Some(mName, num)
                | _ -> None)
            |> Seq.choose id
            |> dict

        let rec resolveValue (value: Value) =
            match value with
            | ActualNumber num -> ActualNumber(num)
            | Reference refId ->
                let found, actualValue = actualNumbersDict.TryGetValue(refId)

                if found then
                    ActualNumber(actualValue)
                else
                    Reference(refId)
            | Operation operation ->
                match operation with
                | Addition (l, r) ->
                    match (l, r) with
                    | ActualNumber ln, ActualNumber rn -> ActualNumber(ln + rn)
                    | _, _ -> Operation(Addition(resolveValue l, resolveValue r))
                | Subtraction (l, r) ->
                    match (l, r) with
                    | ActualNumber ln, ActualNumber rn -> ActualNumber(ln - rn)
                    | _, _ -> Operation(Subtraction(resolveValue l, resolveValue r))
                | Multiplication (l, r) ->
                    match (l, r) with
                    | ActualNumber ln, ActualNumber rn -> ActualNumber(ln * rn)
                    | _, _ -> Operation(Multiplication(resolveValue l, resolveValue r))
                | Division (l, r) ->
                    match (l, r) with
                    | ActualNumber ln, ActualNumber rn -> ActualNumber(ln / rn)
                    | _, _ -> Operation(Division(resolveValue l, resolveValue r))
                | Equality (l, r) ->
                    match (l, r) with
                    | ActualNumber ln, ActualNumber rn -> value
                    | _, _ -> Operation(Equality(resolveValue l, resolveValue r))

        let resolved =
            monkeys
            |> Seq.map (fun (mName, mValue) -> (mName, resolveValue mValue))
            |> Seq.toList

        resolved

    let monkeysResolved =
        Seq.init 5000 id
        |> Seq.fold (fun acc _ -> resolvePass acc) monkeys
        |> Seq.toList

    let rootValue =
        monkeysResolved |> List.find (fun (mName, _) -> mName = "root") |> snd

    let comparedValues =
        match rootValue with
        | Operation operation ->
            match operation with
            | Equality (left, right) -> [ left; right ]
            | _ -> failwith "Unexpected."
        | _ -> failwith "Unexpected."

    let knownValue =
        comparedValues
        |> Seq.map (fun v ->
            match v with
            | ActualNumber n -> Some(v)
            | _ -> None)
        |> Seq.choose id
        |> Seq.head

    let unknownValue =
        comparedValues
        |> Seq.map (fun v ->
            match v with
            | Reference n -> Some(v)
            | _ -> None)
        |> Seq.choose id
        |> Seq.head

    let equation =
        { Left = unknownValue
          Right = knownValue }

    // let result =
    //     match rootValue with
    //     | ActualNumber result -> result
    //     | _ -> failwith "Could not solve."

    let monkeysResolvedDict = monkeysResolved |> dict

    let unwrapEquation (equation: Equation) =
        match equation.Left with
        | Reference id ->
            { Left =
                let found, resolved = monkeysResolvedDict.TryGetValue(id)
                if found then resolved else Reference(id)
              Right = equation.Right }
        | Operation operation ->
            match operation with
            | Division (l, r) ->
                match (l, r) with
                | Reference ref, other ->
                    { Left = Reference ref
                      Right = Operation(Multiplication(other, equation.Right)) }
                | other, Reference ref ->
                    { Left = Reference ref
                      Right = Operation(Division(equation.Right, other)) }
                | _ -> equation
            | Addition (l, r) ->
                match (l, r) with
                | Reference ref, other ->
                    { Left = Reference ref
                      Right = Operation(Subtraction(equation.Right, other)) }
                | other, Reference ref ->
                    { Left = Reference ref
                      Right = Operation(Subtraction(equation.Right, other)) }
                | _ -> equation
            | Multiplication (l, r) ->
                match (l, r) with
                | Reference ref, other ->
                    { Left = Reference ref
                      Right = Operation(Division(equation.Right, other)) }
                | other, Reference ref ->
                    { Left = Reference ref
                      Right = Operation(Division(equation.Right, other)) }
                | _ -> equation
            | Subtraction (l, r) ->
                match (l, r) with
                | Reference ref, other ->
                    { Left = Reference ref
                      Right = Operation(Addition(equation.Right, other)) }
                | other, Reference ref ->
                    { Left = Reference ref
                      Right = Operation(Subtraction(other, equation.Right)) }
                | _ -> equation
            | _ -> equation
        | _ -> equation

    let equationUnwrapped =
        Seq.init 5000 id |> Seq.fold (fun acc _ -> unwrapEquation acc) equation

    let rec solve (value: Value) =
        match value with
        | ActualNumber n -> n
        | Operation operation ->
            match operation with
            | Addition (l, r) -> solve l + solve r
            | Subtraction (l, r) -> solve l - solve r
            | Multiplication (l, r) -> solve l * solve r
            | Division (l, r) -> solve l / solve r
            | _ -> failwith "Unsupported operation."
        | Reference _ -> failwith "Unsupported."

    let result = solve equationUnwrapped.Right

    result
