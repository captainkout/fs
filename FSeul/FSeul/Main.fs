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
//        for dig in 1..5 do
//            let l =helper.comb dig [for a in 1..9 do
//                                    for b in 1..dig do
//                                    yield a]
//            printfn "starting dig--%A" dig
//            printfn "list\t%A" (List.length l)
//            printfn "set\t%A\n" (Set.ofList l
//                                |> Set.count)

        let factorial n = 
            let rec loop acc i=
                if i<=0 then 1I
                elif i=1 then acc
                else loop (acc*(bigint i)) (i-1)
            loop 1I n 
        factorial 3


    garbage f
Main "bologna"
System.Console.ReadKey() |>ignore