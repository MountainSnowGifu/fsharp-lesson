#r "nuget: Deedle"
open Deedle

#r "nuget: FParsec"
open FParsec

let str s = pstring s
let ws = spaces

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parseBy p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

//課題1: pExpressionとpProjectExpressionをここまでの仕様で完成させよ
//project(シラバス)専門,学年

let pColumn: Parser<string,unit> = 
    let normalChar = satisfy(fun c -> c <> '(' && c<> ')' )
    (many1Chars(normalChar))

let pRelationName: Parser<string,unit> =
    let normalChar = satisfy (fun c -> c <> '(' && c <> ')')
    between (pstring "(".>> ws) (pstring ")".>> ws)                          
            (manyChars (normalChar))

let pColumnList = ws>>.sepBy(anyString 2)(str "," .>> ws) .>> ws
let pRelation=  ws>>. pRelationName .>> ws 

test pColumnList "専門,学年,場所"
test pRelation "(シラバス)"

let pidentifier:Parser<string,unit> = 
    let isIdentifireChar c = isLetter c || isDigit c
    manySatisfy isIdentifireChar .>> ws

type ProjectArgu = {identifier: string; RelationName: string ; ColumnList: string list}
type ProjectExpression = ProjectArgu of ProjectArgu

type Expression =
| Project of ProjectExpression

let pConditions = pipe2 pRelation (pColumnList)
                    (fun x y -> {RelationName = x; ColumnList = y})

let pProjectArgu = ws >>. str "project" >>. pConditions .>> ws |>> ProjectArgu
let pExpression = (pProjectArgu|>>Project)

test pExpression "project (シラバス) 専門, 学年, 場所"
