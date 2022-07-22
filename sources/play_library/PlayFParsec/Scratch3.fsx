#r "nuget: Deedle"
open Deedle

#r "nuget: FParsec"
open FParsec

let str s = pstring s
let ws = spaces

let parseBy p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

let pColumn: Parser<string,unit> =
    let normalChar = satisfy (fun c -> c <> '[' && c <> ']')
    between (pstring "[".>> ws) (pstring "]".>> ws)                          
            (manyChars (normalChar))

let pString: Parser<string,unit> =
    let normalChar = satisfy (fun c -> c <> '"' && c <> '"')
    between (pstring "\"".>> ws) (pstring "\"".>> ws)                          
            (manyChars (normalChar))

type ProjectExpression = ColumnList of string list
type FilterArgu = { ColName: string; ColVal: string }
type FilterExpression = FilterArgu of FilterArgu

type Expression =
| Project of ProjectExpression
| Filter of FilterExpression

let pColumnList = ws >>.str "project(" .>> ws >>. sepBy pColumn (str ",".>> ws) .>> str ")" |>> ColumnList
let pFilterValue = pipe2 pColumn (str "=" >>. pString)
                        (fun x y -> {ColName = x;ColVal =y})
let pCondition = str "filter(" >>. pFilterValue .>> str ")" |>> FilterArgu

let pExpression = (pColumnList |>> Project)
                 <|> (pCondition |>> Filter)

let project (ColumnList columnList) (frame :Frame<int,string>) = frame.Columns.[columnList]
let isExistRow (FilterArgu filterArgu) (row: ObjectSeries<string>) = row.GetAs<string>(filterArgu.ColName).Contains(filterArgu.ColVal)
let filter f frame :Frame<int,string>  = frame |> Frame.filterRowValues f
let df = Frame.ReadCsv "sources/data/シラバス.csv"

let selected_df expression =
    match expression with
    | Project P-> df |> project P 
    | Filter F->  df |> filter (isExistRow F)

let columnList = "project([場所],[専門])" |> parseBy pExpression
let result1 = selected_df columnList
result1.Print()

let condition = "filter([専門]=\"物理\")" |> parseBy pExpression
let result2 = selected_df columnList
result2.Print()

