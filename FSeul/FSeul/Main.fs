// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open eulib91_99
let garbage f = 
    GC.Collect()
    GC.WaitForFullGCComplete() |>ignore
    GC.WaitForPendingFinalizers()

    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()

    (f 0) |> printfn "%A"
    timer.Elapsed |>printfn "%A"

let Main fuck =
    let f start =
        let s = (helper.get_web_txt "https://projecteuler.net/project/resources/p096_sudoku.txt").Split [|'\n'|]
        let s2 = [for a in s do
                    if a.Contains("Grid")=false then 
                        for achar in a do
                        yield ((int achar)-48)]
        let s3 = [for a in 1..9 do
                    for b in 1..9 do
                        yield (a,b)]
        let rec loop i d= 
            if i<=80 then loop (i+1) (Map.add s3.[i] s2.[i] d)
            else d
        loop 0 Map.empty
    garbage f
    

Main "bullshit"
System.Console.ReadKey() |>ignore