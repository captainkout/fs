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
        let yint (x1:float) (y1:float) (x2:float) (y2:float) (arr:int array) = 
            if x1=x2 then 
                if x1>0.0 then arr.[0] <- 1
                else arr.[1]<-1
            else
                let slope = if x1<x2 then (x1-x2)/(y1-y2) else (x2-x1)/(y2-y1)
                match slope*x1+y1>0.0 with
                |true ->    arr.[0]<-1
                |false ->   arr.[1]<-1
        let xint (x1:float) (y1:float) (x2:float) (y2:float) (arr:int array) = 
            if y1=y2 then 
                if y1>0.0 then arr.[2]<-1
                else arr.[3]<-1
            else
                let slope = if x1<x2 then (x1-x2)/(y1-y2) else (x2-x1)/(y2-y1)
                match (y1/slope)+x1>0.0 with
                |true ->arr.[2]<-1
                |false ->arr.[3]<-1
        let folder acc (tri:float List) =
            if tri.Length=6 then
                let axis=[|0;0;0;0|]
                if tri.[0]*tri.[2]<0.0 then yint tri.[0] tri.[1] tri.[2] tri.[3] axis
                if tri.[0]*tri.[4]<0.0 then yint tri.[0] tri.[1] tri.[4] tri.[5] axis
                if tri.[2]*tri.[4]<0.0 then yint tri.[2] tri.[3] tri.[4] tri.[5] axis
                if tri.[1]*tri.[3]<0.0 then xint tri.[0] tri.[1] tri.[2] tri.[3] axis
                if tri.[1]*tri.[5]<0.0 then xint tri.[0] tri.[1] tri.[4] tri.[5] axis
                if tri.[3]*tri.[5]<0.0 then xint tri.[2] tri.[3] tri.[4] tri.[5] axis
                if Array.sum axis =4 then acc+1
                else acc
            else acc
        (helper.get_web_txt "https://projecteuler.net/project/resources/p102_triangles.txt").Split [|'\n'|] 
                    |> List.ofArray 
                    |> List.map (fun (s1:string) -> s1.Split [|','|] 
                                                    |> List.ofArray 
                                                    |> List.map (fun s2->if s2 <> "" then (float s2) else 0.0))
                    |> List.fold folder 0
    garbage f

Main "bologna"
System.Console.ReadKey() |>ignore