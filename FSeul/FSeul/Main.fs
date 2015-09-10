// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open testsource

let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

let main() =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let t = Convert.ToInt64(100) + 23L
    printfn "%O" t
    timer.Stop()

    printfn "%O ms" timer.Elapsed


let run = main()
System.Console.ReadKey() |> ignore