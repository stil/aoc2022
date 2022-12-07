module Day07

type FileSystemItem =
    | File of FileInfo
    | Directory of DirectoryInfo

and FileInfo =
    { name: string
      fileSize: uint64
      mutable parent: DirectoryInfo option }

and DirectoryInfo =
    { name: string
      mutable subitems: FileSystemItem list
      mutable parent: DirectoryInfo option }

let input = Helpers.readInput 7

let isCommand (line: string) = line.StartsWith('$')

let inputParsed =
    input
    |> Seq.mapi (fun i line -> (i, line))
    |> Seq.filter (fun (i, line) -> isCommand line)
    |> Seq.map (fun (i, line) ->
        (line,
         input
         |> Seq.skip (i + 1)
         |> Seq.takeWhile (fun line -> line |> isCommand |> not)
         |> Seq.toList))

let root =
    { name = "/"
      subitems = []
      parent = None }


let mutable cwd = root


let cd (destination: string) =
    printfn $"cd to %s{destination}"

    if destination = ".." then
        cwd <- cwd.parent.Value
    else
        let items =
            cwd.subitems
            |> Seq.filter (fun item ->
                match item with
                | Directory (dirinfo) -> dirinfo.name = destination
                | _ -> false)

        let item = items |> Seq.head

        match item with
        | Directory (dirinfo) -> cwd <- dirinfo



let ls (output: string list) =
    printfn "ls"

    let entries =
        output
        |> Seq.map (fun dirent ->
            let parts = dirent.Split(' ')

            match parts[0] with
            | "dir" ->
                Directory
                    { name = parts[1]
                      subitems = []
                      parent = Some(cwd) }
            | _ ->
                File
                    { name = parts[1]
                      fileSize = uint64 parts[0]
                      parent = Some(cwd) })
        |> Seq.toList


    cwd.subitems <- entries
    ()

let consumeCommand (line: string) (output: string list) =
    let parts = line.Split(' ')

    match parts[1] with
    | "cd" -> cd parts[2]
    | "ls" -> ls output
    | _ -> failwith "Unsupported command."

inputParsed |> Seq.skip 1 |> Seq.iter (fun cmd -> cmd ||> consumeCommand)

let rec traverse (current: FileSystemItem) (itemFn: FileSystemItem -> unit) =
    match current with
    | File _ -> itemFn current
    | Directory dir ->
        itemFn current
        dir.subitems |> Seq.iter (fun cmd -> traverse cmd itemFn)

let descendants (node: DirectoryInfo) =
    let mutable result: FileSystemItem list = []
    traverse (Directory node) (fun item -> result <- result |> List.append [ item ])
    result

let sumSize (node: FileSystemItem) =
    match node with
    | Directory (dir) ->
        descendants dir
        |> Seq.choose (fun item ->
            match item with
            | File file -> Some(file)
            | _ -> None)
        |> Seq.sumBy (fun file -> file.fileSize)
    | File file -> file.fileSize

let onlyDirectories =
    descendants root
    |> Seq.choose (fun item ->
        match item with
        | Directory info -> Some(info)
        | _ -> None)
    |> Seq.map (fun dir -> (dir, sumSize (Directory dir)))
    |> Seq.toList

let part1 =
    let highSize = onlyDirectories |> Seq.filter (fun (dir, size) -> size <= 100000UL)

    let result = highSize |> Seq.sumBy (fun (dir, size) -> size)

    traverse (Directory root) (fun item ->
        match item with
        | File file -> printfn $"Scan: %s{file.name}"
        | Directory dir -> printfn $"Scan: %s{dir.name}")

    result


let part2 =
    let bySize = onlyDirectories |> Seq.sortBy (fun (dir, size) -> size) |> Seq.toList

    let totalSpace = 70000000UL
    let requiredUnusedSpace = 30000000UL
    let unusedSpace = totalSpace - sumSize (Directory root)

    let smallest =
        bySize
        |> Seq.find (fun (dir, size) -> unusedSpace + size >= requiredUnusedSpace)
        
    let result = snd smallest

    result
