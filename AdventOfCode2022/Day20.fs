module Day20

open FSharpPlus.Math

let originalNumbers = Helpers.readInput 20 |> Seq.map int |> Seq.toArray
let numbersWithIds = originalNumbers |> Seq.mapi (fun id n -> (id, n)) |> Seq.toList

let part1 =
    let mutable numbers = numbersWithIds

    for i, n in numbersWithIds do
        let nextIndex = numbers |> Seq.findIndex (fun a -> a = (i, n))
        let nextId, nextValue = numbers[nextIndex]

        let insertIndex = Generic.remE (nextIndex + nextValue) (numbers.Length - 1)

        let newList =
            numbers
            |> List.removeAt nextIndex
            |> List.insertAt insertIndex (nextId, nextValue)

        numbers <- newList

    //printfn "%s" (System.String.Join(", ", (newList |> Seq.map snd)))


    let zeroIndex = numbers |> List.findIndex (fun (i, v) -> v = 0)

    let g = numbers[(zeroIndex + 1000) % numbers.Length]
    let p = numbers[(zeroIndex + 2000) % numbers.Length]
    let s = numbers[(zeroIndex + 3000) % numbers.Length]

    let result = [ g; p; s ] |> Seq.map snd |> Seq.sum
    result

let part2 =
    let mutable numbers =
        numbersWithIds |> List.map (fun (i, v) -> (i, int64 v * 811589153L))

    for i in [ 0..9 ] do
        printfn $"Round of mixing: %d{i + 1}"

        for i, n in numbersWithIds do
            let nextIndex = numbers |> Seq.findIndex (fun (leftI, _) -> leftI = i)
            let nextId, nextValue = numbers[nextIndex]

            let insertIndex =
                Generic.remE (int64 nextIndex + nextValue) (int64 numbers.Length - 1L)

            let newList =
                numbers
                |> List.removeAt nextIndex
                |> List.insertAt (int insertIndex) (nextId, nextValue)

            numbers <- newList

            //printfn "%s" (System.String.Join(", ", (newList |> Seq.map snd)))


    let zeroIndex = numbers |> List.findIndex (fun (i, v) -> v = 0L)

    let g = numbers[(zeroIndex + 1000) % numbers.Length]
    let p = numbers[(zeroIndex + 2000) % numbers.Length]
    let s = numbers[(zeroIndex + 3000) % numbers.Length]

    let result = [ g; p; s ] |> Seq.map snd |> Seq.sum

    printfn $"1000th: %d{snd g}"
    printfn $"2000th: %d{snd p}"
    printfn $"3000th: %d{snd s}"

    result
