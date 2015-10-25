// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System

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
        let s = (helper.get_web_txt "https://projecteuler.net/project/resources/p102_triangles.txt").Split [|'\n'|] 
                    |> List.ofArray 
                    |> List.map (fun (s1:string) -> s1.Split [|','|] |> List.ofArray 
                                                    |>List.map (fun s2->if s2 <> "" then (int s2)
                                                                        else 0)) 
        s
    garbage f

Main "bologna"
System.Console.ReadKey() |>ignore