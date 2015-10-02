module eulib91_99
    open System
    open helper

    let ninetyone = 
        let s = 50 //size in euler
        let f acc one =
            match one with
                |(x,y) when x=0 && y>0 ->
                    2*s + acc
                |(x,y) when x>0 && y>0 ->
                    let g = gcd x y
                    let a = (s-x)*g/ y
                    let b = y*g/x
                    1+2*Math.Min(a,b)+acc
                |(x,y)-> acc
        seq{for x in 0..s do
                for y in 1..s do
                    yield (x,y)}
        |> Seq.fold f 0
    let ninetytwo = 
        let rec f2 (i:int) =
            let s = i.ToString()
            let rec f  (acc:int) (s:string) =
                match s.Length with
                |a when a=0 && acc=1 ->acc
                |a when a=0 && acc=89 ->acc
                |a when a=0 -> f 0 (acc|>string) 
                |a ->f (acc+(pown (Int32.Parse((s.[0]).ToString())) 2)) s.[1..] 
            f 0 s
        let folder (acc:int) (one:int) =
            match one with
            |a when a=89 ->acc+1
            |a->acc
        Seq.map f2 [1..10000000]
            |>Seq.fold folder 0
