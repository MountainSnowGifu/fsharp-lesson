open System
open Deedle
open Argu

[<CliPrefix(CliPrefix.DoubleDash)>]
type Arguments =
    | Project of list:string list
    | Filter of colname:string * colval: string

    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Project _ -> "Project"
        | Filter _ -> "Filter"

let project (columnList:string list) (frame :Frame<int,string>) = frame.Columns.[columnList]

//let isExistRow (colname:string,colval:string) (row: ObjectSeries<string>) = row.GetAs<string>(colname).Contains(colval)
//片方だけ部分適用ができなくなるのであまり良くない なるべくフラットに並べる方が良い

let isExistRow colname (colval:string) (row: ObjectSeries<string>) = row.GetAs<string>(colname).Contains(colval)
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
        let parsedArgs = res.GetResult(Filter)
        let selected_df = df |> filter (isExistRow "専門" "数学")
        selected_df.Print()

    0