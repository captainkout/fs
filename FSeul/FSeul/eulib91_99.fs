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
    let ninetytwo max= 
        let rec f i =
            match (sumsqdig 0 i) with
            |a when a=1-> 1
            |a when a=89->89
            |a-> f a 
        let folder acc one =
            match one with
            |a when a=89 ->acc+1
            |a->acc
        Seq.map f [1..max]
            |>Seq.fold folder 0
    let ninetytwoB max= 
        let d = new Dictionary<'a, 'b>()
        let rec mem l n = 
            match l with
            |[]->n
            |h::t->
                d.Add(h,n) 
                mem t n
        let rec f i l =
            match (sumsqdig 0 i) with
            |a when d.ContainsKey a -> mem l d.[a] 
            |a when a=1-> mem l 1
            |a when a= 89-> mem l 89 
            |a-> f a (a::l)
        let rec f2 acc i = 
            match i,(f i []) with
            |(a,b) when a=max->acc
            |(a,b) when b =89-> f2 (acc+1) (i+1)
            |(_,_) ->f2 acc (i+1) 
        f2 0 1 

            
