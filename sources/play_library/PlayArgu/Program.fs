open System

let showCommandLineArgs() =

    let args = System.Environment.GetCommandLineArgs()
    if  args.Length = 2 && args[1] = "-hello" then
        printfn "hello world"
    else
        printfn "I don't know"


showCommandLineArgs()
exit 100