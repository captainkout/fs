// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open eulib91_99
let garbage f = 
    GC.Collect()
    GC.WaitForFullGCComplete() |>ignore
    GC.WaitForPendingFinalizers()

    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()

    (f 0) |> printfn "%A"
    timer.Elapsed |>printfn "%A"


type Square (row,col,i,c) =
    let mutable localc = c
    let mutable locali = i
    let mutable localpos =  if i=0 then set [1;2;3;4;5;6;7;8;9]
                            else set[i] 
    member this.row = row
    member this.col = col
    member this.i
        with get() = locali
        and set(value) = locali <- value
    member this.c
        with get() = localc
        and set (value) = localc <- value
    member this.group = 3*((9*row+col)/27)+(col/3)
    member this.pos
        with get() = localpos
        and set(value)  = localpos <- value

type mode = 
    |Forward
    |Backward

let Main frig =
    garbage eulib91_99.ninetysix
    

Main "bologna"
System.Console.ReadKey() |>ignore