module Day02

type Weapon =
    | Rock
    | Scissors
    | Paper

type Outcome =
    | Lose
    | Draw
    | Win

let mapWeapon symbol =
    match symbol with
    | 'A'
    | 'X' -> Rock
    | 'B'
    | 'Y' -> Paper
    | 'C'
    | 'Z' -> Scissors
    | _ -> failwith "Unknown weapon."

let mapOutcome symbol =
    match symbol with
    | 'Y' -> Draw
    | 'X' -> Lose
    | 'Z' -> Win
    | _ -> failwith "Unknown outcome."

let scoreOutcome outcome =
    match outcome with
    | Lose -> 0
    | Draw -> 3
    | Win -> 6

let scoreWeapon weapon =
    match weapon with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let getBetterWeapon weapon =
    match weapon with
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock

let getWeaponForOutcome opp outcome =
    match outcome with
    | Lose -> opp |> getBetterWeapon |> getBetterWeapon
    | Draw -> opp
    | Win -> getBetterWeapon opp

let getOutcome opp me =
    if opp = me then Draw
    elif me = (getBetterWeapon opp) then Win
    else Lose

let sumScore rounds =
    rounds
    |> Seq.map (fun (opp, me) -> (opp, me) ||> getOutcome |> scoreOutcome, me |> scoreWeapon)
    |> Seq.map (fun (score, extraPoints) -> score + extraPoints)
    |> Seq.sum

let input =
    (Helpers.readInput 2)
    |> Seq.map (fun e -> e.Replace(" ", ""))
    |> Seq.map (fun e -> (e[0], e[1]))

let part1 =
    let result =
        input |> Seq.map (fun (opp, me) -> mapWeapon opp, mapWeapon me) |> sumScore

    printfn $"Result: %i{result}."

let part2 =
    let result =
        input
        |> Seq.map (fun (opp, me) -> mapWeapon opp, mapOutcome me)
        |> Seq.map (fun e -> (fst e, e ||> getWeaponForOutcome))
        |> sumScore

    printfn $"Result: %i{result}."
