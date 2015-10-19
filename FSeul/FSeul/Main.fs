// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open eulib91_99
let garbage f = 
    GC.Collect()
    GC.WaitForFullGCComplete() |>   ignore
    GC.WaitForPendingFinalizers()
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    (f 0) |> printfn "%A"
    timer.Elapsed |>printfn "%A"


let Main frig =
    let f start = 
        let rec loop (b,e) =
            //let b,e = l.[0],l.[1] 
            if b >2 then loop ((helper.isqrt b),(e*2))
            else e
        let s = (helper.get_web_txt "https://projecteuler.net/project/resources/p099_base_exp.txt").Split('\n')
                |> List.ofArray |> List.map (fun st ->      let ns= st.Split(',') 
                                                            ((int ns.[0]),(int ns.[1])) )
                |> List.map loop

        let max = List.max s
        List.findIndex (fun x ->x = max)
    garbage f

Main "bologna"
System.Console.ReadKey() |>ignore