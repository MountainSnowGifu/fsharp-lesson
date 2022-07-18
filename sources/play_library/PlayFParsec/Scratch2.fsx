#r "nuget: FParsec"
open FParsec
run pfloat "1.25"

let ws = spaces
let float_ws : Parser<float,unit> = pfloat .>> ws
run float_ws "1.25"

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25"
test pfloat "1.25E 3"

//チュートリアルを4.3
let str s = pstring s
let (floatBetweenBrackets: Parser<float,unit>) = str "[" >>. pfloat .>> str "]"
test floatBetweenBrackets "[1.0]"
test floatBetweenBrackets "[]"
test floatBetweenBrackets "[1.0"

//4.4 Abstracting parsers
let betweenStrings s1 s2 p = str s1 >>. p .>> str s2
let floatBetweenBrackets2 : Parser<float,unit> = pfloat |> betweenStrings "[" "]"
let floatBetweenDoubleBrackets2: Parser<float,unit> = pfloat |> betweenStrings "[[" "]]"
test floatBetweenBrackets2 "[1.0]"
test floatBetweenDoubleBrackets2 "[[1.0]]"

let between pBegin pEnd p  = pBegin >>. p .>> pEnd
let betweenStrings2 s1 s2 p = p |> between (str s1) (str s2)
let floatBetween3Brackets: Parser<float,unit> = pfloat |> betweenStrings2 "[[[" "]]]"
test floatBetween3Brackets "[[[1.0]]]"

//4.5 Parsing a list of floats
test (many floatBetweenBrackets) ""
test (many floatBetweenBrackets) "[1.0]"
test (many floatBetweenBrackets) "[2][3][4]"
test (many floatBetweenBrackets) "[1][2.0E]"
test (many1 floatBetweenBrackets) "(1)"
test (many1 (floatBetweenBrackets <?> "float between brackets")) "(1)"

//4.6 Handling whitespace
test floatBetweenBrackets "[1.0, 2.0]"
//let ws = spaces
let str_ws s = pstring s .>> ws
//let float_ws = pfloat .>> ws
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

test numberList @"[ 1 ,
                          2 ] "
test numberList @"[ 1,
                         2; 3]"
let numberListFile = ws >>. numberList .>> eof
test numberListFile " [1, 2, 3] [4]"
test numberListFile " [1, 2, 3,4]"

//4.7 Parsing string data
test (many (str "a" <|> str "b")) "abbba"