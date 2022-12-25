module Day19

open System.Collections.Generic
open System.Diagnostics

type Cost = { Ore: int; Clay: int; Obsidian: int }

type Blueprint =
    { Id: int
      OreRobotCost: Cost
      ClayRobotCost: Cost
      ObsidianRobotCost: Cost
      GeodeRobotCost: Cost }

let pattern =
    "Blueprint (\d+):\s*Each ore robot costs (\d+) ore.\s*Each clay robot costs (\d+) ore.\s*Each obsidian robot costs (\d+) ore and (\d+) clay.\s*Each geode robot costs (\d+) ore and (\d+) obsidian."

let blueprints =
    Helpers.readInput 19
    |> Seq.map (fun line ->
        let m = System.Text.RegularExpressions.Regex.Match(line, pattern)
        let v (i: int) = int m.Groups[i].Value

        { Id = v 1
          OreRobotCost = { Ore = v 2; Clay = 0; Obsidian = 0 }
          ClayRobotCost = { Ore = v 3; Clay = 0; Obsidian = 0 }
          ObsidianRobotCost = { Ore = v 4; Clay = v 5; Obsidian = 0 }
          GeodeRobotCost = { Ore = v 6; Clay = 0; Obsidian = v 7 } })
    |> Seq.toList

type State =
    { Blueprint: Blueprint
      MinutesPassed: int
      OreRobots: int
      ClayRobots: int
      ObsidianRobots: int
      GeodeRobots: int
      Ore: int
      Clay: int
      Obsidian: int
      Geode: int
      SkippedBuyingOreRobot: bool
      SkippedBuyingClayRobot: bool
      SkippedBuyingObsidianRobot: bool
      SkippedBuyingGeodeRobot: bool }


let computeMaxGeodesPerBlueprint blueprints timeLimit =
    let advanceProductionAndTime (prevState: State) (state: State) =
        { state with
            Ore = state.Ore + prevState.OreRobots
            Clay = state.Clay + prevState.ClayRobots
            Obsidian = state.Obsidian + prevState.ObsidianRobots
            Geode = state.Geode + prevState.GeodeRobots
            MinutesPassed = state.MinutesPassed + 1 }

    let advanceTime (state: State) =
        { state with MinutesPassed = state.MinutesPassed + 1 }

    let hasFunds (state: State) (cost: Cost) =
        state.Ore >= cost.Ore
        && state.Clay >= cost.Clay
        && state.Obsidian >= cost.Obsidian

    let deductCost (state: State) (cost: Cost) =
        { state with
            Ore = state.Ore - cost.Ore
            Clay = state.Clay - cost.Clay
            Obsidian = state.Obsidian - cost.Obsidian }

    let constructGeodeRobot (state: State) =
        let stateAfterPurchase = deductCost state state.Blueprint.GeodeRobotCost
        { stateAfterPurchase with GeodeRobots = state.GeodeRobots + 1 }

    let constructObsidianRobot (state: State) =
        let stateAfterPurchase = deductCost state state.Blueprint.ObsidianRobotCost
        { stateAfterPurchase with ObsidianRobots = state.ObsidianRobots + 1 }

    let constructClayRobot (state: State) =
        let stateAfterPurchase = deductCost state state.Blueprint.ClayRobotCost
        { stateAfterPurchase with ClayRobots = state.ClayRobots + 1 }

    let constructOreRobot (state: State) =
        let stateAfterPurchase = deductCost state state.Blueprint.OreRobotCost
        { stateAfterPurchase with OreRobots = state.OreRobots + 1 }

    let nextStates (state: State) =
        let nextState constructFn =
            { state with
                SkippedBuyingClayRobot = false
                SkippedBuyingGeodeRobot = false
                SkippedBuyingObsidianRobot = false
                SkippedBuyingOreRobot = false }
            |> constructFn
            |> advanceProductionAndTime state //|> advanceTime

        if state.MinutesPassed = timeLimit then
            Seq.empty
        elif state.MinutesPassed = timeLimit - 1 then
            Seq.singleton (nextState id)
        elif state.MinutesPassed = timeLimit - 2 then
            if hasFunds state state.Blueprint.GeodeRobotCost then
                Seq.singleton (nextState constructGeodeRobot)
            else
                Seq.singleton (nextState id)
        else
            seq {
                if hasFunds state state.Blueprint.GeodeRobotCost then
                    yield nextState constructGeodeRobot
                else
                    let canBuyObsidian = hasFunds state state.Blueprint.ObsidianRobotCost
                    let canBuyClay = hasFunds state state.Blueprint.ClayRobotCost
                    let canBuyOre = hasFunds state state.Blueprint.OreRobotCost

                    if canBuyObsidian && not state.SkippedBuyingObsidianRobot then
                        yield nextState constructObsidianRobot

                    if canBuyClay && not state.SkippedBuyingClayRobot then
                        yield nextState constructClayRobot

                    if canBuyOre && not state.SkippedBuyingOreRobot then
                        yield nextState constructOreRobot

                    yield
                        { nextState id with
                            SkippedBuyingOreRobot = canBuyOre
                            SkippedBuyingClayRobot = canBuyClay
                            SkippedBuyingObsidianRobot = canBuyObsidian }
            }

    let printState (state: State) =
        printfn $"== Minute %d{state.MinutesPassed} =="
        printfn $"Ore %d{state.Ore}, Clay: %d{state.Clay}, Obsidian: %d{state.Obsidian}, Geode: %d{state.Geode}"

        printfn
            $"Ore robots %d{state.OreRobots}, Clay robots: %d{state.ClayRobots}, Obsidian robots: %d{state.ObsidianRobots}, Geode robots: %d{state.GeodeRobots}"

        printfn ""

        state

    let result =
        blueprints
        |> Seq.toArray
        |> Array.Parallel.map (fun blueprint ->
            let stopwatch = Stopwatch.StartNew()
            let mutable scannedItems = 0UL
            let toVisit = PriorityQueue<State, int>()

            toVisit.Enqueue(
                { Blueprint = blueprint
                  MinutesPassed = 0
                  OreRobots = 1
                  ClayRobots = 0
                  ObsidianRobots = 0
                  GeodeRobots = 0
                  Ore = 0
                  Clay = 0
                  Obsidian = 0
                  Geode = 0
                  SkippedBuyingOreRobot = false
                  SkippedBuyingClayRobot = false
                  SkippedBuyingObsidianRobot = false
                  SkippedBuyingGeodeRobot = false

                },
                0
            )

            let mutable maxGeodes = 0

            let potential (state: State) =
                let final =
                    Seq.init (timeLimit - state.MinutesPassed) id
                    |> Seq.fold
                        (fun acc _ ->
                            { acc with
                                GeodeRobots = acc.GeodeRobots + 1
                                Geode = acc.Geode + acc.GeodeRobots })
                        state

                final.Geode


            while toVisit.Count > 0 do
                let current = toVisit.Dequeue()
                scannedItems <- scannedItems + 1UL

                if current.MinutesPassed = timeLimit then
                    if current.Geode > maxGeodes then
                        maxGeodes <- current.Geode
                else
                    let nextStateList = nextStates current
                    let minutesLeft = timeLimit - current.MinutesPassed
                    let potentialGeodes = potential current

                    if potentialGeodes < maxGeodes then
                        ()
                    else
                        for next in nextStateList do
                            let minutesLeft = timeLimit - next.MinutesPassed
                            let heuristic = -(next.Geode + next.GeodeRobots * minutesLeft)
                            toVisit.Enqueue(next, heuristic)

                ()

            let quality = blueprint.Id * maxGeodes

            printfn
                $"[Blueprint %d{blueprint.Id}] [max geodes %d{maxGeodes}] [quality %d{quality}] [elapsed %d{stopwatch.ElapsedMilliseconds} ms] [scanned items %d{scannedItems}]"

            (blueprint, maxGeodes))

    result

let part1 =
    // let maxGeodesPerBlueprint = computeMaxGeodesPerBlueprint blueprints 24
    //
    // let qualityLevels =
    //     maxGeodesPerBlueprint
    //     |> Seq.map (fun (blueprint, maxGeodes) -> blueprint.Id * maxGeodes)
    //
    // let result = qualityLevels |> Seq.sum
    // result
    0

let part2 =
    let maxGeodesPerBlueprint =
        computeMaxGeodesPerBlueprint (blueprints |> List.take (min blueprints.Length 3)) 32

    let result =
        maxGeodesPerBlueprint |> Seq.map snd |> Seq.reduce (fun mg1 mg2 -> mg1 * mg2)

    result
