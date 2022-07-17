open System
open Argu

[<CliPrefix(CliPrefix.Dash)>]
type Arguments =
    | Hello

    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Hello -> "print Hello World"


//課題7: -hello をArgu化しよう
[<EntryPoint>]
let main args =
    printfn "Arguments passed to function : %A" args
    let parser = ArgumentParser.Create<Arguments>(programName="PlayArgu")
    let res = parser.Parse args
    if res.Contains Hello then
        printfn "hello world"
    else
        printfn "I don't know"
    0