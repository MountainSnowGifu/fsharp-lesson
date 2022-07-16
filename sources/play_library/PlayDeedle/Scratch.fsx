#r "nuget:Deedle"

open Deedle
open System


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

//課題１課題1: GetとGetAsの違いを調べよう。
//getはオブジェクトで取得する
//getasは指定した型で取得する

//課題2: 専門が数学の行だけを残そう
df.RowsDense |> Series.filterValues(fun row -> row.GetAs<string>("専門").Contains("数学"))

//課題3: 専門が数学の行だけを持ったFrameを作ろう
let df2 = Frame.ReadCsv("sources/data/シラバス.csv")
        |> Frame.filterRowValues(fun row -> row.GetAs<string>("専門").Contains("数学"))

//課題4: 場所と学年だけのFrameを作ろう
let df3 = Frame.ReadCsv("sources/data/シラバス.csv")
let df4 = df3.Columns.[ ["場所"; "学年"] ]

