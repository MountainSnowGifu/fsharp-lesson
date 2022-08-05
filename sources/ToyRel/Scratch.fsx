//git add /Users/akira/Desktop/F#/F#Lesson/fsharp-lesson/sources/ToyRel
//git commit -m '課題0'
//git push -u origin toyrel/1_pexpression

#r "nuget: Deedle"
open Deedle

#r "nuget: FParsec"
open FParsec
open System.Text.RegularExpressions

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

//課題0: identifierにマッチする正規表現を書け

let pIdentifier:Parser<string,unit> = regex "([_a-zA-Z¥p{IsHiragana}¥p{IsKatakana}¥p{IsCJKUnifiedIdeographs}][0-9_a-zA-Z¥p{IsHiragana}¥p{IsKatakana}¥p{IsCJKUnifiedIdeographs}]*)|(^(!?.*[^0-9¥(¥)]+$).*$)"
test pIdentifier "abc"     //abc
test pIdentifier "_abc123" //_abc123
test pIdentifier "abc_123" //abc_123
test pIdentifier "専門"     //専門
test pIdentifier "フロア"   //フロア

test pIdentifier "123"     //マッチしない
test pIdentifier "abc.def" //"abc"
test pIdentifier "abc*"    //"abc"
test pIdentifier "abc:def" //"abc"
test pIdentifier "abc def" //"abc"
test pIdentifier "(abc)"   //マッチしない
test pIdentifier "abc+def" //"abc"