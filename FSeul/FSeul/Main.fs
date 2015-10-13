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


type Square (x,y,i,c) =
    let mutable localc = c
    member this.x = x
    member this.y = y
    member this.i = i
    member this.c
        with get() = localc
        and set (value) = localc <- value
 

let Main frig =
    let f start =

        let r = new System.Random()

        let rec loop2 m x y = 
            match x,y with
            |a,b when a=4 && b=4 ->(Map.add (a,b) (Square(a,b,r.Next(0,9),0)) m)
            |a,b when a<4 && b=4 ->loop2 (Map.add (a,b) (Square(a,b,r.Next(0,9),0)) m) (x+1) 0
            |a,b ->loop2 (Map.add (a,b) (Square(a,b,r.Next(0,9),0)) m) (x) (y+1)
        let newd = loop2 (Map.empty) 1 1
        printfn "%A" newd.[(1,1)].c
        newd.[(1,1)].c<-43
        let temp = (newd.[(1,1)].x, newd.[(1,1)].y, newd.[(1,1)].i,newd.[(1,1)].c)


//        let col = Map.init for x in 1..4 do for y in 1..4 do 
//                      Square(x,y,r.Next(0,9),0)
        //let (d:Map<(int*int),Square>) = Map.empty
//        printfn "%A" col.[0].c
//        let rec loop (arr:array<Square>) i= 
//            if i>= arr.Length then ()
//            else 
//                arr.[i].c<-99
//                loop arr (i+1)
//        loop col 0
//
//        col.[0].c
        let x = 1
        for y in 1..4 do newd.[(x,y)].c <-900
        newd
    garbage f
    

Main "bologna"
System.Console.ReadKey() |>ignore