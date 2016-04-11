// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open eulib91

let main() =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    
    let l = eulib91.main (0,1) (1,0) []
    Console.WriteLine(l.Length()+1)
        
    timer.Elapsed 
        |> printfn "%O ms" 


main()
System.Console.ReadKey() |> ignore