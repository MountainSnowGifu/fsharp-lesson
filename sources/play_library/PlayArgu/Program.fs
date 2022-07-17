open System
open Deedle
open Argu

[<CliPrefix(CliPrefix.DoubleDash)>]
type Arguments =
    | Project of List<string>

    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Project _ -> "Project"

let project (list:list<string>) (frame :Frame<int,string>) = frame.Columns.[list]

//課題8: PlayDeedleのprojectとfilterをArguでコマンドライン化
[<EntryPoint>]
let main args =
    let df = Frame.ReadCsv "sources/data/シラバス.csv"
    let parser = ArgumentParser.Create<Arguments>(programName="PlayArgu")
    let res = parser.Parse args

    if res.Contains Project then
        let list = df|> project (res.GetResult(Project))
        list.Print()
    0