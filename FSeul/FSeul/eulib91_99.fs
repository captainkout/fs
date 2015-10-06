module eulib91_99
    open System
    open System.Collections.Generic
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
    let ninetytwo max= //16sec
        let rec f i =
            match sumsqdig 0 i with
            |a when a=1||a=89->a
            |a-> f a 
        let folder acc one =
            match one with
            |a when a=89 ->acc+1
            |a->acc
        Seq.map f {1..max}
            |>Seq.fold folder 0
    let ninetytwoB max= //9sec
        let d = new Dictionary<'a, 'b>()
        let rec f i l =
            let rec mem l n = 
                match l with
                |[]-> n
                |h::t-> 
                    d.Add(h,n)
                    mem t n 
            match sumsqdig 0 i with
            |a when d.ContainsKey i ->mem l d.[i]
            |a when a=1->mem (i::l) 1 
            |a when a= 89->mem (i::l) 89
            |a-> f a (i::l)
        let rec f2 (acc:int) (i:int) =
            if i=max then acc
            else
                match f i [] with
                |a when a=89-> f2 (acc+1) (i+1)
                |a ->f2 acc (i+1)
        f2 0 1 
    let ninetytwoC max= //12sec
        let rec f2 acc i =
            let rec f i =
                match (sumsqdig 0 i) with
                |a when a=1||a=89->a
                |a-> f a
            if i=max then acc
            else
                match f i with
                |a when a=89-> f2 (acc+1) (i+1)
                |a ->f2 acc (i+1)
        f2 0 1 