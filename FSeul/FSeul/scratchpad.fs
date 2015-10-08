module scratchpad
    open System
    let eulib93 = 
        let l2a = List.map helper.perm (helper.comb 3 [0..11])
                    |>List.fold (fun acc one-> acc@one) []
        let div a b=
            if b=0. then 0. else a/b 
        let add a b = a+b
        let sub a b = a-b
        let mult a b = a*b
        let l2b = [( add ); ( add ); ( add ); ( sub ); ( sub ); ( sub ); ( mult ); ( mult ); ( mult ); ( div ); ( div ); ( div )]
        let l2 = List.map (fun x1-> List.map (fun x2->l2b.[x2]) x1) l2a

        let proc l = 
            let l1 = List.map helper.perm l 
                        |>List.fold (fun acc one-> acc@one) []
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
            |h::t-> let x = (proc [h])
                    looper t (x::acc)
        (looper (helper.comb 4 ([1..10]|>List.map (fun x -> (float x)))) [])
            |> List.maxBy (fun (x,y)->x)

