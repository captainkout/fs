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


let main =
    let f address =
        let s = helper.get_web_txt "https://projecteuler.net/project/resources/p096_sudoku.txt"
        s.ToString().Substring(0,100)
    
    
    
     
    garbage f

main
System.Console.ReadKey() |> ignore