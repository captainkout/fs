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

    
    let x = helper.comb 3 [0..2]
    
    timer.Elapsed |>printfn "%A sec"


//
//    timer.Restart()
//
//    let y = scratchpad.comb 3 [0..100]
//    timer.Elapsed |>printfn "%A sec"

main()
System.Console.ReadKey() |> ignore