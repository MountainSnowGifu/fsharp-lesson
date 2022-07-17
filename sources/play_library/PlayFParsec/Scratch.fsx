//git add /Users/akira/Desktop/F#/F#Lesson/fsharp-lesson/sources/play_library/PlayFParsec
//git commit -m '課題6-2'
//git push -u origin play_library/3_playfparsec 

#r "nuget: FParsec"
open FParsec
run pfloat "1.25"

let ws = spaces
let float_ws = pfloat .>> ws
run float_ws "1.25"

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25"
test pfloat "1.25E 3"

let str s = pstring s
let (floatBetweenBrackets: Parser<float,unit>) = str "[" >>. pfloat .>> str "]"
test floatBetweenBrackets "[1.0]"
test floatBetweenBrackets "[]"