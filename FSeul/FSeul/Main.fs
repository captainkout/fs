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
        let rec test2 (th:int list) (tt:int list list) a dir = 
            if a=0 then test2 th tt (a+1) (th.[a]>(List.head tt).[a])
        let rec test1 l1 cnt =
            match l1 with
            |[]->failwith "arg1"
            |h1::[] -> cnt
            |h1::t1 ->  test1 t1 (test2 h1 t1 0 true)
        let l = [1;2;3;4]
        let combs = [for a in 2..l.Length/2
                        do yield l |> helper.comb a]
                        |> List.map (fun item-> test1 item 0)


        combs
            

    garbage f

Main "bologna"
System.Console.ReadKey() |>ignore