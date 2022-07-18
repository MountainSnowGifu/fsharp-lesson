//git add /Users/akira/Desktop/F#/F#Lesson/fsharp-lesson/sources/play_library/PlayFParsec
//git commit -m '課題10'
//git push -u origin play_library/3_playfparsec 

#r "nuget: FParsec"
open FParsec

//課題10: カラム名のパーサーを書こう //[場所]
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let pColumn: Parser<string,unit> =
    let normalChar = satisfy (fun c -> c <> '[' && c <> ']') //satisfy は与えられた述語関数を満たす任意の文字をパースします。
    between (pstring "[") (pstring "]")                          
            (manyChars (normalChar)) //manyChars は与えられた文字パーザで文字シーケンスをパースし、結果を文字列として返します。

test pColumn "[場所]"