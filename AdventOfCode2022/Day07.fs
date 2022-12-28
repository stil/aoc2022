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
        | File file -> Some file
        | _ -> None)

let filterDirs items =
    items
    |> Seq.choose (fun item ->
        match item with
        | Directory dir -> Some dir
        | _ -> None)

let fileSystem =
    let cd destination (cwd: DirectoryInfo) =
        match destination with
        | ".." -> cwd.parent.Value
        | _ -> cwd.children |> filterDirs |> Seq.find (fun dir -> dir.name = destination)

    let word i (text: string) = text.Split(' ')[i]

    let ls (output: string seq) (cwd: DirectoryInfo) =
        cwd.children <-
            output
            |> Seq.map (fun dirent ->
                match word 0 dirent with
                | "dir" ->
                    Directory
                        { name = word 1 dirent
                          children = []
                          parent = Some cwd }
                | _ ->
                    File
                        { name = word 1 dirent
                          fileSize = uint64 (word 0 dirent)
                          parent = Some cwd })
            |> Seq.toList

    let root =
        { name = "/"
          children = []
          parent = None }

    let input = Helpers.readInput 7
    let isCommand (line: string) = line.StartsWith('$')

    let readCmdOutput cmdLineIndex =
        input |> Seq.skip (cmdLineIndex + 1) |> Seq.takeWhile (isCommand >> not)

    let consumeCommand lineIndex line currentCwd =
        match word 1 line with
        | "cd" -> cd (word 2 line) currentCwd
        | "ls" ->
            ls (readCmdOutput lineIndex) currentCwd
            currentCwd
        | _ -> failwith "Unsupported command."

    input
    |> Seq.indexed
    |> Seq.filter (fun (_, line) -> isCommand line)
    |> Seq.skip 1
    |> Seq.fold (fun state (i, line) -> consumeCommand i line state) root
    |> ignore

    Directory root

let rec descendants node =
    match node with
    | File _ -> [ node ]
    | Directory dir -> node :: (dir.children |> List.collect descendants)

let totalSize node =
    descendants node |> filterFiles |> Seq.sumBy (fun file -> file.fileSize)

let directorySizes =
    descendants fileSystem
    |> filterDirs
    |> Seq.map (fun dir -> totalSize (Directory dir))

let part1 =
    directorySizes |> Seq.filter (fun size -> size <= 100000UL) |> Seq.sum |> string

let part2 =
    directorySizes
    |> Seq.sort
    |> Seq.find (fun size -> 70000000UL - (totalSize fileSystem) + size >= 30000000UL)
    |> string

Helpers.assertEqual "2104783" part1
Helpers.assertEqual "5883165" part2
