module eulib110_119
    let hundredEleven start = 
        let primes = helper.primeSeqL (pown 10L 6) |> Seq.toArray
        let rec isprime n i=
            if n % primes.[i] =0L then 
                false
            elif primes.[i] > helper.Lsqrt n then
                true
            else  isprime n (i+1)
        let nums =  set[for dig in 10..10 do
                        for n in 0L..9L do
                        for i in 0..(dig-1) do
                        for j in 0..(dig-1) do
                            let arr = Array.init dig (fun x -> n)
                            for inum in 0L..9L do
                            for jnum in 0L..9L do
                            arr.[i]<-inum
                            arr.[j]<-jnum
                            let x =(Array.mapi (fun i d -> 
                                                d*(pown 10L ((Array.length arr)-1-i))) arr
                                    |> Array.sum )
                            if x > pown 10L (dig-1) && x < pown 10L (dig) && isprime x 0 then
                                yield x]

        let mutable max = Array.init 10 (fun x->(0,[]))
        for p in nums do
            let dc = helper.Ldigcnt p
            let m = List.max dc
            do List.iteri (fun i n-> 
                            if n = m && n > fst max.[i] then
                                do max.[i] <- (n,[p])
                            elif n = m && n= fst max.[i] then
                                do max.[i] <- (n,p::(snd max.[i])) ) dc
        let fin = Array.mapi (fun i (m,n) ->
                                (i,m,List.length n,List.sum n)) max
                    |> Array.toList 
        do printfn "starting %A..." "fours"
        do helper.listPrint fin 
        do printfn "sum is \t%A \n" (List.sumBy (fun (a,b,c,d)->d) fin)
