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
//filter([専門] = "物理")
let pString: Parser<string,unit> =
    let normalChar = satisfy (fun c -> c <> '"' && c <> '"')
    between (pstring "\"".>> ws) (pstring "\"".>> ws)                          
            (manyChars (normalChar))

let pColumn_pString = pColumn.>> ws .>> str "=" .>> ws >>. pString .>> ws
let pFilter = ws >>.str "filter(" .>> ws >>. pColumn_pString.>> ws .>> str ")"
test pFilter "filter([専門] = \"物理\")"
