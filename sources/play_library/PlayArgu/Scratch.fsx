//git add /Users/akira/Desktop/F#/F#Lesson/fsharp-lesson/sources/play_library/PlayArgu/Program.fs
//git add sources/play_library/PlayArgu/Scratch.fsx
//git commit -m '課題6-2'
//git push -u origin play_library/2_helloargu   
#r "nuget: Argu"
open Argu

type Arguments =
    | Hoge
    | Foo of path:string

    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Hoge -> "print Hello World"
        | Foo _ -> "print Bar with arg"

let parser = ArgumentParser.Create<Arguments>(programName="hogehoge")

parser.PrintUsage()

parser.Parse[| "--hoge" |]
parser.Parse[| "--foo" |] //error
parser.Parse[| "--foo"; "fugafuga" |]

let res = parser.Parse[| "--foo"; "ikaika" |]
res.Contains Hoge
res.Contains Foo
res.GetResult(Foo)

let res2 = parser.Parse[| "--hoge"; "--foo"; "ikaika" |]