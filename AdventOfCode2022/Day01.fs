module Day01

let input =
    Input.readInput 1
    |> Seq.map System.Int32.Parse
    |> Seq.toArray

let filter (vals: seq<seq<int>>) =
    vals
    |> Seq.filter (fun vals -> Seq.sum vals = 2020)

let multiply (vals: seq<int>) =
    Seq.fold (fun acc charge -> acc * charge) 1 vals

let getResult vals crossproduct =
    vals
    |> crossproduct
    |> filter
    |> Seq.head
    |> multiply

let part1 =
    let crossproduct (list: int seq) =
        seq {
            for el1 in list do
                for el2 in list do
                    seq {
                        el1
                        el2
                    }
        }

    let result = getResult input crossproduct
    printfn $"Result: %i{result}."

let part2 =
    let crossproduct (list: int seq) =
        seq {
            for el1 in list do
                for el2 in list do
                    for el3 in list do
                        seq {
                            el1
                            el2
                            el3
                        }
        }

    let result = getResult input crossproduct
    printfn $"Result: %i{result}."
