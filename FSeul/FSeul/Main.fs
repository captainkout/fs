// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open testsource

let main() =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    
    let x =
        (113916,23584684) 
        ||>testsource.gcd
    Console.WriteLine(x|>string)
    timer.Stop()

    timer.Elapsed 
        |> printfn "%O ms" 


main()
System.Console.ReadKey() |> ignore