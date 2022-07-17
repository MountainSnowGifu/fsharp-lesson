open System

//課題6: オプションhelloを処理
// let showCommandLineArgs() =

//     let args = System.Environment.GetCommandLineArgs()
//     if  args.Length = 2 && args[1] = "-hello" then
//         printfn "hello world"
//     else
//         printfn "I don't know"
//showCommandLineArgs()
//exit 100

//課題6: オプションhelloを処理[<EntryPoint>]
[<EntryPoint>]
let main args =
    if  args.Length = 1 && args[0] = "-hello" then
        printfn "hello world"
    else
        printfn "I don't know"
    0