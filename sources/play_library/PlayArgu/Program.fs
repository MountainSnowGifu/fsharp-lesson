open System
open Deedle
open Argu

[<CliPrefix(CliPrefix.DoubleDash)>]
type Arguments =
    | Project of string list
    | Filter of string list

    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Project _ -> "Project"
        | Filter _ -> "Filter"

let project (columnList:string list) (frame :Frame<int,string>) = frame.Columns.[columnList]
let f (list: string list) = fun (row: ObjectSeries<string>) -> row.GetAs<string>(list[0]).Contains(list[1])
let filter f frame :Frame<int,string>  = frame |> Frame.filterRowValues f

//課題8: PlayDeedleのprojectとfilterをArguでコマンドライン化
//課題8-2: filter
[<EntryPoint>]
let main args =
    let df = Frame.ReadCsv "sources/data/シラバス.csv"
    let parser = ArgumentParser.Create<Arguments>(programName="PlayArgu")
    let res = parser.Parse args

    if res.Contains Project then
        let selected_df = df |> project (res.GetResult(Project))
        selected_df.Print()

    if res.Contains Filter then
        let prediction = f (res.GetResult(Filter))
        let list = df |> filter prediction
        list.Print()

    0