﻿module eulib91_99
    open System
    open System.Collections.Generic

    let ninetyone start= 
        let s = 50 //size in euler
        let f acc one =
            match one with
                |(x,y) when x=0 && y>0 ->
                    2*s + acc
                |(x,y) when x>0 && y>0 ->
                    let g = helper.gcd x y
                    let a = (s-x)*g/ y
                    let b = y*g/x
                    1+2*Math.Min(a,b)+acc
                |(x,y)-> acc
        seq{for x in 0..s do
                for y in 1..s do
                    yield (x,y)}
            |> Seq.fold f 0
    let ninetytwo start= //16sec, seq ops are so cool
        let max = 10000000 //size in euler
        let rec f i =
            match helper.sumsqdig 0 i with
            |a when a=1||a=89->a
            |a-> f a 
        let folder acc one =
            match one with
            |a when a=89 ->acc+1
            |a->acc
        Seq.map f {1..max}
            |>Seq.fold folder 0
    let ninetytwoC start= //12sec, recursive iteration is cool too
        let max = 10000000 //size in euler
        let rec f2 acc i =
            let rec f i =
                match (helper.sumsqdig 0 i) with
                |a when a=1||a=89->a
                |a-> f a
            if i=max then 
                acc 
            else
                match f i with
                |a when a=89-> f2 (acc+1) (i+1)
                |a ->f2 acc (i+1)
        f2 0 1 
    let ninetythree start= 
        let max =8 //i know this is, where it ends
        let div a b=
            if b=0. then 0. else a/b 
        let add a b = a+b
        let sub a b = a-b
        let mult a b = a*b
        let l2 = List.map helper.perm (helper.comb 3 [0..11])
                    |> List.fold (fun acc one-> acc@one) []
                    |> List.map (fun x1-> List.map (fun x2->
                                                [( add ); ( add ); ( add ); 
                                                ( sub ); ( sub ); ( sub ); 
                                                ( mult ); ( mult ); ( mult ); 
                                                ( div ); ( div ); ( div )].[x2]) x1)
        let proc l = 
            let l1 = List.map helper.perm l 
                    |>List.fold (fun acc one-> acc @ one) []
            ([for i1 in l1 do
                for i2 in l2 do
                    yield (i2.[2] (i2.[1] (i2.[0] i1.[0] i1.[1]) i1.[2]) i1.[3])   // _ _ _ _
                    yield (i2.[1] (i2.[0] i1.[0] i1.[1]) (i2.[2] i1.[2] i1.[3])) //_ _(_ _)
                    yield (i2.[2] (i2.[0] i1.[0] (i2.[1] i1.[1]  i1.[2])) i1.[3])   //_ (_ _) _
                    yield (i2.[0] i1.[0] (i2.[2] (i2.[1] i1.[1] i1.[2]) i1.[3]))  //_ (_ _ _)
                    yield (i2.[0] i1.[0] (i2.[1] i1.[1] (i2.[2] i1.[2] i1.[3])))]  //_ (_(_ _))
                    |> Set.ofList
                    |> Set.filter (fun (a:float) -> a=Math.Floor(a) && a>0. )
                    |> Set.toList
                    |> List.fold (fun acc one -> match one with
                                                  |a when a=acc+1. ->acc+1.
                                                  |a -> acc) 0., l)
        let rec looper l acc=
            match l with 
            |[]-> acc
            |h::t-> looper t ((proc [h])::acc)
        (looper (helper.comb 4 ([1..max]|>List.map (fun x -> (float x)))) [])
            |> List.maxBy (fun (x,y)->x)
    let ninetyfour start =
        let rec test a b c max=
            if 2*(a+c)<=max||2*(b+c)<=max then true
            else false
        let folder acc x = 
            match x with
            |(a,b,c) ->
                let one = 2*a-c
                let two = 2*b-c
                if (one = 1 || one = -1 ) && ( two = 1 || two = -1) then (acc+2*(a+c)+2*(b+c))
                elif (one = 1 || one = -1 ) then (acc+2*(a+c))
                elif ( two = 1 || two = -1) then (acc+2*(b+c))
                else acc
        helper.pythag test 1000000000
            |> Seq.fold folder 0
    let ninetyfive start =
        let divs = Array.create 1000000 List.empty 
        let narr = Array.create 1000000 Set.empty
        let rec loop i j =
            if i>=divs.Length/2 then ()
            elif j>=divs.Length then loop (i+1) (2*(i+1))
            else 
                divs.[j]<- i::divs.[j]
                loop i (j+i)
        loop 1 2
        let d = Array.map (fun x->List.sum x) divs
        let rec loop i ni= 
            if i>=narr.Length then () //done
            elif ni<narr.Length     &&  narr.[i].Contains d.[ni] = false then 
                narr.[i]    <-  narr.[i].Add d.[ni]
                loop i d.[ni]
            elif ni>narr.Length then
                narr.[i]    <-  Set.empty
                loop (i+1) (i+1)
            else //find where it repeats
                narr.[i]    <-  Set.empty |> Set.add d.[ni]
                let rec loop2 ni2 = 
                    if narr.[i].Contains d.[ni2] = false then 
                        narr.[i]<-narr.[i].Add d.[ni2]
                        loop2 d.[ni2]
                    else ()
                loop2 (d.[ni])
                loop (i+1) (i+1)
        loop 2 2
        let (a,b,c) = Array.fold (fun acc (one:int Set) ->  
                                    match acc with
                                    |(a,b,c)->  if one.Count>b then 
                                                    (Set.minElement one,one.Count,one)
                                                else acc ) (0,0,Set.empty) narr
        Set.minElement narr.[(Array.findIndex (fun x->x=c) narr)]