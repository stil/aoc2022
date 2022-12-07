module Day07

type FileSystemItem =
    | File of FileInfo
    | Directory of DirectoryInfo

and FileInfo =
    { name: string
      fileSize: uint64
      parent: DirectoryInfo option }

and DirectoryInfo =
    { name: string
      mutable children: FileSystemItem list
      parent: DirectoryInfo option }

let filterFiles items =
    items
    |> Seq.choose (fun item ->
        match item with
        | File file -> Some(file)
        | _ -> None)

let filterDirs items =
    items
    |> Seq.choose (fun item ->
        match item with
        | Directory dir -> Some(dir)
        | _ -> None)

let fileSystem =
    let cd destination (cwd: DirectoryInfo) =
        match destination with
        | ".." -> cwd.parent.Value
        | _ -> cwd.children |> filterDirs |> Seq.find (fun dir -> dir.name = destination)

    let getWord i (text: string) = text.Split(' ')[i]

    let ls (output: string list) (cwd: DirectoryInfo) =
        { cwd with
            children =
                output
                |> Seq.map (fun dirent ->
                    match getWord 0 dirent with
                    | "dir" ->
                        Directory
                            { name = getWord 1 dirent
                              children = []
                              parent = Some(cwd) }
                    | _ ->
                        File
                            { name = getWord 1 dirent
                              fileSize = uint64 (getWord 0 dirent)
                              parent = Some(cwd) })
                |> Seq.toList }

    let root =
        { name = "/"
          children = []
          parent = None }

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

    let consumeCommand (line: string) (output: string list) (currentCwd: DirectoryInfo) =
        match getWord 1 line with
        | "cd" -> cd (getWord 2 line) currentCwd
        | "ls" ->
            let newCwd = ls output currentCwd
            currentCwd.children <- newCwd.children
            currentCwd
        | _ -> failwith "Unsupported command."

    let lastCwd =
        inputParsed
        |> Seq.skip 1
        |> Seq.fold (fun state (line, output) -> consumeCommand line output state) root

    Directory root

let rec descendants (node: FileSystemItem) =
    match node with
    | File _ -> [ node ]
    | Directory dir -> (Directory dir) :: (dir.children |> List.collect descendants)

let sumSize (node: FileSystemItem) =
    descendants node |> filterFiles |> Seq.sumBy (fun file -> file.fileSize)

let directorySizes =
    descendants fileSystem
    |> filterDirs
    |> Seq.map (fun dir -> sumSize (Directory dir))

let part1 = directorySizes |> Seq.filter (fun size -> size <= 100000UL) |> Seq.sum

let part2 =
    directorySizes
    |> Seq.sort
    |> Seq.find (fun size -> 70000000UL - (sumSize fileSystem) + size >= 30000000UL)
