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
    let   x = 1.23456789123456789M|>decimal
    let y =35.0
    
    Console.WriteLine(
        (x))

    timer.Stop()

    printfn "%O ms" timer.Elapsed


let run = main()
System.Console.ReadKey() |> ignore