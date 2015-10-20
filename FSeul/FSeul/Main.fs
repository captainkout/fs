// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open eulib91_99
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
        let x = "blah"
        x
//    let f start =
//        let max=1000000L
//        let rec loop (x:int64) h=
//            let newh = h|> Set.add (2L*(x*x-x)) 
//            if x<max then
//                if Set.contains (x*x-x) h then 
//                    printfn "%A" (x,x*x-x,2L*(x*x-x))
//                    loop (x+1L) newh
//                else loop (x+1L) (Set.add (x*x-x) newh)
//            else ()
//        loop 1L (set[])
//    let f2 start =
//        let max=1000000000M
//        let rec loop2 (top:decimal) (bot:decimal) (ltop:decimal) =
//            match 2M*(top*top-top),bot*bot-bot with
//            |a,b when a=b && bot<=max ->    printfn "%A" (top,bot)
//                                            let n = top/ltop
//                                            loop2 (Math.Floor(n*top)) (Math.Floor(n*bot)) (top-ltop) 
//            |a,b when a=b && bot>max ->   printfn "%A" (top,bot)
//            |a,b when a<b-> loop2 (top+1M) (bot) ltop 
//            |a,b ->loop2 (top) (bot+1M) ltop 
//        loop2 1M 2M 1M
//    //garbage f

    garbage f

Main "bologna"
System.Console.ReadKey() |>ignore