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
    let t = slow2 100 2365135
    printfn "%O" t
    timer.Stop()
    //let elapsed : string = System.Convert.ToString(timer.ElapsedMilliseconds)
    //System.Console.WriteLine("result: {0}",result)
    printfn "%O ms" timer.ElapsedMilliseconds


let run = main()
System.Console.ReadKey() |> ignore