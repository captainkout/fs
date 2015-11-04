module eulib100_109
    let hundred start = 
        let rec loop b n max =
            if n<max then
                loop (3L*b + 2L*n - 2L) (4L*b + 3L*n - 3L) max
            else b,n
        loop 15L 21L (pown 10L 12)
    let hundredone start =
        let m = [for r in 1.0..11.0 do
                   yield [for a in 0.0..11.0 do yield r**a]]
        let rec fip acc x =
            let solve xbx =
                let A = MathNet.Numerics.LinearAlgebra.MatrixExtensions.matrix (m |> helper.listSlice [0..(xbx-1)] |> List.map (helper.listSlice [0..(xbx-1)]))
                let v =[ for n in [1.0..11.0] do
                            yield (1.- n + (pown n 2) - (pown n 3) + (pown n 4) - (pown n 5) + (pown n 6) - (pown n 7) + (pown n 8) - (pown n 9) + (pown n 10))]
                            //yield (n**3.0)]
                let b = MathNet.Numerics.LinearAlgebra.VectorExtensions.vector (v |> helper.listSlice [0..(xbx-1)])
                A.Solve(b).ToArray() 
                            |> List.ofArray 
                            |> List.zip (helper.listSlice [0..(xbx-1)] m.[(xbx)]) 
                            |> List.map (fun (a,b)->(decimal b)*(decimal a))
                            |> List.sum
            if x<10 then 
                fip (acc+(solve x)) (x+1)
            else System.Math.Round(acc+(solve x))
        fip 0M 1
    let hundredtwo start = 
        let yint (x1:float) (y1:float) (x2:float) (y2:float) (arr:int array) = 
            if x1=x2 then 
                if x1>0.0 then arr.[0] <- 1
                else arr.[1]<-1
            else
                let slope = if x1<x2 then (y1-y2)/(x1-x2) else (y2-y1)/(x2-x1)
                match slope*(-x1)+y1 with
                |a when a>0.0 ->    arr.[0]<-1
                |a when a<0.0->   arr.[1]<-1
                |_ ->()
        let xint (x1:float) (y1:float) (x2:float) (y2:float) (arr:int array) = 
            if y1=y2 then 
                if y1>0.0 then arr.[2]<-1
                else arr.[3]<-1
            else
                let slope = if x1<x2 then (y1-y2)/(x1-x2) else (y2-y1)/(x2-x1)
                match (-y1/slope)+x1 with
                |a when a>0.0 ->arr.[2]<-1
                |a when a<0.0->arr.[3]<-1
                |_->()
        let folder acc (tri:float List) =
            if tri.Length=6 then
                let axis=[|0;0;0;0|]
                if tri.[0]*tri.[2]<=0.0 then yint tri.[0] tri.[1] tri.[2] tri.[3] axis
                if tri.[0]*tri.[4]<=0.0 then yint tri.[0] tri.[1] tri.[4] tri.[5] axis
                if tri.[2]*tri.[4]<=0.0 then yint tri.[2] tri.[3] tri.[4] tri.[5] axis
                if tri.[1]*tri.[3]<=0.0 then xint tri.[0] tri.[1] tri.[2] tri.[3] axis
                if tri.[1]*tri.[5]<=0.0 then xint tri.[0] tri.[1] tri.[4] tri.[5] axis
                if tri.[3]*tri.[5]<=0.0 then xint tri.[2] tri.[3] tri.[4] tri.[5] axis
                if Array.sum axis =4 then acc+1
                else acc
            else acc
        (helper.get_web_txt "https://projecteuler.net/project/resources/p102_triangles.txt").Split [|'\n'|] 
                    |> List.ofArray 
                    |> List.map (fun (s1:string) -> s1.Split [|','|] 
                                                    |> List.ofArray 
                                                    |> List.map (fun s2->float s2))
                    |> List.fold folder 0
    let hundredthree start = 
        helper.comb 7 ([20..47])
        |>List.filter (fun l -> (l.[0]+l.[1]) > l.[(List.length l)-1] && //lowest two less than top one
                                    l.[0]+l.[1]+l.[2] > l.[(List.length l)-2]+l.[(List.length l)-1] && //lowest 3 less than top two
                                    l.[0]+l.[1]+l.[2]+l.[3] > l.[(List.length l)-3]+l.[(List.length l)-2]+l.[(List.length l)-1] ) //lowest 4 less than top three
        |>List.filter (fun l -> let test =[for a in 1..(l.Length-1) do
                                            yield (helper.comb a l)] 
                                            |> List.fold (fun acc l1-> l1@acc) []
                                            |> List.map (fun l1 -> List.sum l1)
                                List.length test = Set.count (Set.ofList test ))
        |>List.sortBy (fun l -> (List.sum l))
        |>List.head 
        |>List.fold (fun acc i -> acc+(string i)) ""
    let hundredfour start = 
        let truecase = List.init 10 (fun x -> if x<>0 then 1 else 0)
        let rec fib a1 a2 i =
            let t= (i * 0.20898764024997873M-0.3494850021680094M)-System.Math.Floor(i * 0.20898764024997873M - 0.3494850021680094M)+8M |>float
            match helper.Ldigcnt a2 =truecase, System.Math.Pow(10.0,t) |> int64 |>helper.Ldigcnt =truecase with
            |true,true ->   i
            |_,_-> fib a2 ((a1+a2)%(pown 10L 9)) (i+1M)
        fib 1L 1L 2M
    let hundredfive start = 
        let rec test (l:int list) a = //verify lowest x > highest x-1
            if a>(l.Length/2) then true
            elif    (l |> helper.listSlice [0..a] |> List.sum ) > ( l |> helper.listSlice [(l.Length-a)..(l.Length-1)] |> List.sum) then
                    test l (a+1)
            else false
        (helper.get_web_txt "https://projecteuler.net/project/resources/p105_sets.txt").Split [|'\n'|]
                |> Array.map (fun (x:string)->x.Split [|','|] 
                                                |> Array.map (fun x->int x) 
                                                |> Array.sort
                                                |> List.ofArray)
                |> List.ofArray
                |> List.filter (fun l -> test l 1)
                |> List.filter (fun l ->    let test =[for a in 1..(l.Length-1) do
                                                        yield (helper.comb a l)] 
                                                        |> List.fold (fun acc l1-> l1@acc) []
                                                        |> List.map (fun l1 -> List.sum l1)
                                            List.length test = Set.count (Set.ofList test ))
                |> List.map (fun l->List.sum l)
                |> List.sum