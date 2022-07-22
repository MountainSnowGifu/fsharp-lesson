#r "nuget: FParsec"
#r "nuget: Deedle"
open FParsec
open Deedle

let project (columnList:string list) (frame :Frame<int,string>) = frame.Columns.[columnList]
let isExistRow colname (colval:string) (row: ObjectSeries<string>) = row.GetAs<string>(colname).Contains(colval)
let filter f frame :Frame<int,string>  = frame |> Frame.filterRowValues f

let df = Frame.ReadCsv "sources/data/シラバス.csv"