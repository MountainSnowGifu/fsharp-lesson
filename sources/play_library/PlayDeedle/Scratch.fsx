#r "nuget:Deedle"

open Deedle

#load "Deedle.fsx"
Frame.ReadCsv "sources/data/シラバス.csv"

let df = Frame.ReadCsv "sources/data/シラバス.csv"
df.Print()

df.Rows
df.RowsDense
let row = df.Rows.GetAt(0)

row.Get("専門")
row.GetAs("専門")
row.GetAs<string>("専門")
row.GetAs<int>("場所")
row.GetAs<string>("場所")

//課題１課題1: GetとGetAsの違いを調べよう
//getはオブジェクトで取得する
//getasは指定した型で取得する
