// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System

let garbage f = 
    GC.Collect()
    GC.WaitForFullGCComplete() |>   ignore
    GC.WaitForPendingFinalizers()
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    (f 0) |> printfn "%A"
    timer.Elapsed |>printfn "%A"

let Main frig =
    garbage eulib100_109.hundredtwo

Main "bologna"
System.Console.ReadKey() |>ignore