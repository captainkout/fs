// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System

let garbage f = 
    GC.Collect()
    GC.WaitForFullGCComplete() |>   ignore
    GC.WaitForPendingFinalizers()
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    (f 0) |> printfn "%A"
    timer.Elapsed |>printfn "%A"


let Main frig =
    let f start = 
        let x = Seq.unfold (fun n->if n>0 then Some(n % 10, n/10) else None) 123456 |> Seq.toList
        x = List.sort x 

    garbage f
Main "bologna"
System.Console.ReadKey() |>ignore