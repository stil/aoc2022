module Day02

type Shape =
    | Rock
    | Scissors
    | Paper

type Outcome =
    | Lose
    | Draw
    | Win

let mapWeaponABC symbol =
    match symbol with
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors
    | _ -> failwith "Unknown weapon."

let mapWeaponXYZ symbol =
    match symbol with
    | 'X' -> Rock
    | 'Y' -> Paper
    | 'Z' -> Scissors
    | _ -> failwith "Unknown weapon."

let mapOutcome symbol =
    match symbol with
    | 'Y' -> Draw
    | 'X' -> Lose
    | 'Z' -> Win
    | _ -> failwith "Unknown outcome."

let sumScore rounds =
    rounds
    |> Seq.map (fun e ->
        match e with
        // Lose
        | Paper, Rock -> 0 + 1
        | Scissors, Paper -> 0 + 2
        | Rock, Scissors -> 0 + 3
        // Draw
        | Rock, Rock -> 3 + 1
        | Paper, Paper -> 3 + 2
        | Scissors, Scissors -> 3 + 3
        // Win
        | Scissors, Rock -> 6 + 1
        | Rock, Paper -> 6 + 2
        | Paper, Scissors -> 6 + 3)
    |> Seq.sum

let input =
    (Helpers.readInput 2)
    |> Seq.map (fun e -> e.Replace(" ", ""))
    |> Seq.map (fun e -> (e[0], e[1]))

let part1 =
    let result =
        input
        |> Seq.map (fun (opp, me) -> mapWeaponABC opp, mapWeaponXYZ me)
        |> sumScore

    printfn $"Result: %i{result}."

let part2 =
    let result =
        input
        |> Seq.map (fun (opp, me) -> mapWeaponABC opp, mapOutcome me)
        |> Seq.map (fun e ->
            (fst e,
             match e with
             // Lose
             | Paper, Lose -> Rock
             | Scissors, Lose -> Paper
             | Rock, Lose -> Scissors
             // Draw
             | Rock, Draw -> Rock
             | Paper, Draw -> Paper
             | Scissors, Draw -> Scissors
             // Win
             | Scissors, Win -> Rock
             | Rock, Win -> Paper
             | Paper, Win -> Scissors))
        |> sumScore

    printfn $"Result: %i{result}."
