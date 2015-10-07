module scratchpad
    let test = 
        let l1 = (helper.comb 4 [1..4])
        let l2 = (helper.comb 3 [( + ); ( + ); ( + ); ( - ); ( - ); ( - ); ( * ); ( * ); ( * ); ( / ); ( / ); ( / )])
        [for i1 in l1 do
            for i2 in l2 do
                //this one just adds in order
                yield (i2.[2] (i2.[1] (i2.[0] i1.[0] i1.[1]) i1.[2]) i1.[3])]
            |> Set.ofList
            |> Set.iter (fun x->printfn "%A" x) 






