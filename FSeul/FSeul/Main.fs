// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open eulib91_99

let main() =
    GC.Collect()
    GC.WaitForFullGCComplete()
    GC.WaitForPendingFinalizers()
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()



    timer.Elapsed |>printfn "%A sec"


//
//    timer.Restart()
//
//
//    timer.Elapsed |>printfn "%A sec"

main()
System.Console.ReadKey() |> ignore