﻿// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open eulib91_99

let main() =
    GC.Collect()
    GC.WaitForFullGCComplete()
    GC.WaitForPendingFinalizers()
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    
    eulib91_99.ninetytwo |> printfn "%O"
    //Math.Min((5/2),2.0)|> printfn "%O"
    timer.Elapsed 
        |> printfn "%O sec" 


main()
System.Console.ReadKey() |> ignore