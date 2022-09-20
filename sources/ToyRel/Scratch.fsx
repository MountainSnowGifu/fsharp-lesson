//git add /Users/akira/Desktop/F#/F#Lesson/fsharp-lesson/sources/ToyRel
//git commit -m '課題3: Relationの型をつくれ csvをロードする関数(readCsvしてdistinctするなにか）'
//git push -u origin toyrel/3_relation_type
#r "nuget: Deedle"
open Deedle

#r "nuget: FParsec"
open FParsec
open System.Text.RegularExpressions

let ws = spaces
let str s = pstring s.>> ws

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parseBy p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

//課題0: identifierにマッチする正規表現を書け

let pIdentifierRegex:Parser<string,unit> = regex "([_a-zA-Z]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})([0-9_a-zA-Z]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})*"
test pIdentifierRegex "abc"     //abc
test pIdentifierRegex "_abc123" //_abc123
test pIdentifierRegex "abc_123" //abc_123
test pIdentifierRegex "専門"     //専門
test pIdentifierRegex "フロア"   //フロア

test pIdentifierRegex "123"     //マッチしない
test pIdentifierRegex "abc.def" //"abc"
test pIdentifierRegex "abc*"    //"abc"
test pIdentifierRegex "abc:def" //"abc"
test pIdentifierRegex "abc def" //"abc"
test pIdentifierRegex "(abc)"   //マッチしない
test pIdentifierRegex "abc+def" //"abc"

//課題1: pExpressionとpProjectExpressionをここまでの仕様で完成させよ
let notSBracket s = s <> '[' && s <> ']'
let pSBracketColumn :Parser<string,unit> = (str "[") >>. many1Satisfy notSBracket .>> (str "]")
let pColumn = pIdentifierRegex <|> pSBracketColumn

//test pColumn "[akira]"

type ColumnList = ColumnList of string list
let pColumnList = sepBy pColumn (str ",") |>> ColumnList

//test pColumnList "[akira],abc,_abc123"

type Expression =
| Identifier of string
| ProjectExpression of Expression * ColumnList

let pIdentifier = pIdentifierRegex |>> Identifier

let pExpression , pExpressionRef = createParserForwardedToRef() // :Parser<Expression,unit> ref 
let pProjectExpression = (str "project") >>. pExpression .>>. pColumnList |>> ProjectExpression
pExpressionRef.Value <- (str "(") >>. (pProjectExpression<|>pIdentifier) .>> (str ")")

//課題3: Relationの型をつくれ
type Relation = Relation of Frame<int,string>

//課題2: dfのrowを一意にしたdf2を返す、distinctを作れ
let distinct (df:Frame<int,string>) = df.Columns[df.ColumnKeys].Rows.Values 
                                    |> Seq.distinct
                                    |> Series.ofValues
                                    |> Frame.ofRows
                                    |> Relation

module RalationType = 
    type T = {Relation: Frame<int,string>}
    let distinct (df:Frame<int,string>) = df.Columns[df.ColumnKeys].Rows.Values 
                                        |> Seq.distinct
                                        |> Series.ofValues
                                        |> Frame.ofRows
                                        |> Relation
    let getRelation df = distinct df


let project (ColumnList columnList)(Relation relation) = distinct relation.Columns.[columnList]

let rec evalExpression expression df = 
    match expression with
    | Identifier I -> df
    | ProjectExpression(expression,columnList) -> evalExpression expression (df|> project columnList)
and evalProjectExpression projectExpression df =
    match projectExpression with
    | Identifier I -> df
    | ProjectExpression(expression,columnList) -> evalProjectExpression expression (df|> project columnList)

let df = Frame.ReadCsv "sources/data/シラバス.csv"
let expression = parseBy pProjectExpression "project(project(シラバス)専門,学年,場所)学年,場所"
let (Relation result) = df |> Relation |> evalExpression expression
result.Print()

//課題3: Relationの型をつくれ csvをロードする関数(readCsvしてdistinctするなにか）
let GetRelationFromCsv csvPath = distinct (Frame.ReadCsv csvPath)
let (Relation GetRelationFromCsvResult) = GetRelationFromCsv "sources/data/シラバス.csv"
GetRelationFromCsvResult.Print()