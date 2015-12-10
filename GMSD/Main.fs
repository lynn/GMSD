open libgmsd
open System.IO

// Some settings for file output.
let scriptDirectoryName = "src"
let scriptExtension = "txt"

/// Given a script name, get the path to store it at.
let scriptPath (scriptName : string) : string =
    Path.Combine(scriptDirectoryName, Path.ChangeExtension(scriptName, scriptExtension))

/// Extract the scripts from the given data.win path. If `isToFiles` is true,
/// write each script to `src/script_name.txt`. Else, just print everything to
/// standard output.
let writeScripts (isToFiles : bool) (path : string) =
    let form = Unpack.readForm path

    if isToFiles then
        Directory.CreateDirectory(scriptDirectoryName) |> ignore

    for i, c in Array.mapi (fun i x -> (i, x)) form.Code do
        let script = Decompile.decompile c.Start c.Bytecode
        let scriptName = form.StringAddressMap.[c.NameAddress]
        if isToFiles then
            printfn "Writing (%5d/%5d) %s" i form.Code.Length scriptName
            let writer = new StreamWriter(scriptPath scriptName)

            for ps in script do
                writer.WriteLine(Decompile.showPosStatement form ps) |> ignore

            writer.Flush() |> ignore
            writer.Close() |> ignore
        else
            printfn "=================== %s ===================" scriptName
            for ps in script do
                printfn "%s" (Decompile.showPosStatement form ps)

[<EntryPoint>]
let main argv =
    match Array.toList argv with
    | ["-f"; path] -> writeScripts true path
    | [path]       -> writeScripts false path
    | _            -> eprintfn "Usage: GMSD.exe [-f] [data.win path]\n\
                                The -f flag enables file output (to %s)."
                                (scriptPath "*")
    0