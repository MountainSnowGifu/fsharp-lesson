open System

//課題6: オプションhelloを処理[<EntryPoint>]
[<EntryPoint>]
let main args =
    if  args.Length = 1 && args[0] = "-hello" then
        printfn "hello world"
    else
        printfn "I don't know"
    0