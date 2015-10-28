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
    let f start = 
        let truecase = List.init 10 (fun x -> if x<>0 then 1 else 0)
        let (p:int64) = pown 10L 9
        let rec fib (a1:int64) (a2:int64) (i:int) = 
            match helper.Ldigcnt a2=truecase with
            |true ->    printfn "%A \t %A" a2 i 
                        fib a2 ((a1+a2)%p) (i+1) 
            |false->    fib a2 ((a1+a2)%p) (i+1) 
        fib 1L 1L 2 
    
    garbage f

Main "bologna"
System.Console.ReadKey() |>ignore