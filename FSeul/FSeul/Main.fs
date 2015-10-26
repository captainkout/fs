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
        helper.comb 6 [11..25]
            |>List.filter (fun l -> (l |> helper.listSlice [0..4] |> List.sum) > (l |> helper.listSlice [4..5] |>List.sum) &&
                                    l.[0]>l.[5]-l.[1])
            |>List.sortBy (fun l ->List.sum l)
    garbage f

Main "bologna"
System.Console.ReadKey() |>ignore