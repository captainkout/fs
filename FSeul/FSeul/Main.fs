// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open eulib91_99

let main() =
    GC.Collect()
    GC.WaitForFullGCComplete() |>ignore
    GC.WaitForPendingFinalizers()
    let timer = new System.Diagnostics.Stopwatch()
        
    timer.Start()

    (ninetyfour)
        |> printfn "%A"
    //List.iter (fun x-> printfn "not fancy %A" x) (List.sort ros)


    timer.Elapsed
        |>printfn "not fancy %A"
main()
System.Console.ReadKey() |> ignore