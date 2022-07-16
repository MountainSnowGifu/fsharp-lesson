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
//row.GetAs("専門")
row.GetAs<string>("専門")
row.GetAs<int>("場所")
row.GetAs<string>("場所")

//課題１課題1: GetとGetAsの違いを調べよう。
//getはオブジェクトで取得する
//getasは指定した型で取得する

//課題2: 専門が数学の行だけを残そう
df.RowsDense |> Series.filterValues(fun row -> row.GetAs<string>("専門").Contains("数学"))

//課題3: 専門が数学の行だけを持ったFrameを作ろう
df |> Frame.filterRowValues(fun row -> row.GetAs<string>("専門").Contains("数学"))

//課題4: 場所と学年だけのFrameを作ろう
let keys = ["場所"; "学年"]
df.Columns.[keys]
//パイプライン
df |> Frame.filterCols(fun col _ -> col.Contains("場所") || col.Contains("学年"))


//課題5-1: フィルタとプロジェクションを関数にしよう
//rowを引数にboolを返す関数を引数にとってフィルタしたFrameを返すfilter
let f = fun (row: ObjectSeries<string>) -> row.GetAs<string>("専門").Contains("数学")
let filter f frame :Frame<int,string>  = frame |> Frame.filterRowValues f
df |> filter f

//課題5-2: フィルタとプロジェクションを関数にしよう
//カラムの名前のリストを引数にとってそのカラムだけを含んだFrameを返すproject関数を作ろう。
let list = ["場所";"学年"]
let project (list:list<string>) (frame :Frame<int,string>) = frame.Columns.[list]
df |> project list

//test