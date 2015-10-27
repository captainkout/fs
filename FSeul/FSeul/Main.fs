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
        let rec l109 e n =
            if n < pown 10I e then n
            else l109 e (n/10I)
        let rec fib (a1:bigint) (a2:bigint) (i:int) s= 
            match (helper.Ldigcnt (a2 % (pown 10I 9) |>int64)) = truecase, (helper.Ldigcnt ((l109 9 a2) |>int64)) = truecase ,Set.contains ((a2 % (pown 10I 9) |>int64)) s with
            |_,_,true ->    printfn "%A" (a1+a2)
                            i
            |true,true,_ -> i
            |_,_,_    ->  //printfn "%A \t %A \t %A" a1 a2 i
                            fib (a2 % (pown 10I 9) |>int64) (a1+a2) (i+1) (Set.add (a1+a2) s)
        fib 1I 1I 2 (Set.empty<int64>)
    
    garbage f

Main "bologna"
System.Console.ReadKey() |>ignore