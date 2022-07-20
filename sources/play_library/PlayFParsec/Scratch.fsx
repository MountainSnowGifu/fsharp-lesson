//git add /Users/akira/Desktop/F#/F#Lesson/fsharp-lesson/sources/play_library/PlayFParsec
//git commit -m '課題11 修正'
//git push -u origin play_library/3_playfparsec 

#r "nuget: FParsec"
open FParsec

let str s = pstring s
let ws = spaces
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

//課題10: カラム名のパーサーを書こう //[場所]
let pColumn: Parser<string,unit> =
    let normalChar = satisfy (fun c -> c <> '[' && c <> ']') //satisfy は与えられた述語関数を満たす任意の文字をパースします。
    between (pstring "[".>> ws) (pstring "]".>> ws)                          
            (manyChars (normalChar)) //manyChars は与えられた文字パーザで文字シーケンスをパースし、結果を文字列として返します。

test pColumn "[場所]"

//課題 10.1 pidentifierを書け
let pidentifier :Parser<string,unit>=
    let isIdentifierFirstChar c = isLetter c
    let isIdentifierChar c = isLetter c || isDigit c
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "isLetter" .>> ws

test pidentifier "test1="
test pidentifier "test@"
test pidentifier "t111*"
test pidentifier "1test"

//課題11: projectのパーサーを書こう
let pProjcet = ws >>.str "project(" .>> ws >>. sepBy pColumn (str ",".>> ws) .>> str ")"
test pProjcet " project( [ test1 ] , [ test2 ] ) "

//課題12: filterのパーサーを書こう
//[カラム名] = "文字列"
let pString: Parser<string,unit> =
    let normalChar = satisfy (fun c -> c <> '"' && c <> '"')
    between (pstring "\"".>> ws) (pstring "\"".>> ws)                          
            (manyChars (normalChar))

type Filter = ColumnName of string
            | StringSsearch of string

let pFilter = (pColumn |>> ColumnName)
           <|> (pString |>> StringSsearch)

let pexpr = pColumn  .>>. (pstring "=").>>. pString

test pFilter "\"123.4\""
test pFilter "[test1]"
test pexpr "[test1]="


let pintToF:Parser<float,unit> = pint32 |>> float
let pnumber = pintToF
            <|> pfloat

type Factor = Number of float
            | Identifier of string

let fnum = Number 3.0
let fid = Identifier "hogehoge"

let pfactor = (pnumber |>> Number)
           <|> (pidentifier |>> Identifier)

test pfactor "123.4"
test pfactor "abc"