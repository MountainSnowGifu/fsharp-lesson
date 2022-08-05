//git add /Users/akira/Desktop/F#/F#Lesson/fsharp-lesson/sources/ToyRel
//git commit -m '課題0'
//git push -u origin toyrel/1_pexpression

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

let pIdentifierRegex:Parser<string,unit> = regex "([_a-zA-Z\p{IsHiragana}\p{IsKatakana}\p{IsCJKUnifiedIdeographs}][0-9_a-zA-Z\p{IsHiragana}\p{IsKatakana}\p{IsCJKUnifiedIdeographs}]*)"
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
let pSBracketColumn :Parser<string,unit> = (pstring "[") >>. many1Satisfy notSBracket .>> (pstring "]")
let pColumn = pIdentifierRegex <|> pSBracketColumn

//test pColumn "[akira]"

type ColumnList = List of string list
let pColumnList = sepBy pColumn (str ",") |>> List

//test pColumnList "[akira],abc,_abc123"

type Expression =
| Identifier of string
| ProjectExpression of Expression * ColumnList

let pIdentifier = pIdentifierRegex |>> Identifier

let pExpression , pExpressionRef = createParserForwardedToRef() // :Parser<Expression,unit> ref 
let pProjectExpression = (str "project") >>. pExpression .>>. pColumnList |>> ProjectExpression
pExpressionRef.Value <- (str "(") >>. (pProjectExpression<|>pIdentifier) .>> (str ")")

//pExpressionRef.Value <- (str "(") >>. (pIdentifier<|>pProjectExpression) .>> (str ")") //こちらはエラー


test pProjectExpression "project(シラバス)専門,学年,場所"
test pProjectExpression "project(project(シラバス)専門,学年,場所)専門,学年,場所"
