// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System

let garbage f = 
    GC.Collect()
    GC.WaitForFullGCComplete() |>   ignore
    GC.WaitForPendingFinalizers()
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    (f 0) |> printfn "%A"
    timer.Elapsed |>printfn "%A"


let Main frig =
    let f start = 
        let primes = helper.primeSeq (pown 10 9) |> Seq.toArray
        for n in 2..8 do
            let mutable max = Array.init 10 (fun x->(0,[]))
            for p in Array.filter (fun i->
                                    i > pown 10 (n-1) && i<pown 10 n ) primes do
                let dc = helper.idigcnt p
                let m = List.max dc
                do List.iteri (fun i n-> 
                                if n = m && n > fst max.[i] then
                                    do max.[i] <- (n,[p])
                                elif n = m && n= fst max.[i] then
                                    do max.[i] <- (n,p::(snd max.[i])) ) dc
            let fin = Array.mapi (fun i (m,n) ->
                                    (i,m,List.length n,List.sum n)) max
                        |> Array.toList 
            do printfn "starting %A..." n
            do helper.listPrint fin 
            do printfn "sum is \t%A \n" (List.sumBy (fun (a,b,c,d)->d) fin)

    garbage f
Main "bologna"
System.Console.ReadKey() |>ignore