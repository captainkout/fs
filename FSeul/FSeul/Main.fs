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
    let result = testsource.function2 50
    timer.Stop()
    let elapsed : string = System.Convert.ToString(timer.ElapsedMilliseconds)
    System.Console.WriteLine("result: {0}",result)
    printfn "%s ms" elapsed
    

let run = main()
System.Console.ReadKey() |> ignore