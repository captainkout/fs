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
    type bouncy = Dec|Inc|Unknown
    let hundredTwelve n =
        let rec loop2 n prev b = 
            if n=0L then 
                0.0 //its technically increasing 
            else
                match n%10L, b with
                |a,Unknown when a>prev -> loop2 (n/10L) a Dec
                |a,Inc when a>prev -> 1.0  
                |a,Unknown when a<prev -> loop2 (n/10L) a Inc
                |a,Dec when a<prev ->  1.0
                |a,_ -> loop2 (n/10L) a b
        let rec loop1 n cnt =
            let newcnt = (loop2 (n/10L) (n % 10L) Unknown)
            if (cnt+newcnt)/(n-1L|>float) = 0.99 then 
                n
            else
                loop1 (n+1L) (cnt+newcnt)
        -1L+(loop1 102L 0.0) 
    let hundredTwelve_unfold start =
        let rec loop n cnt = 
            let x = Seq.unfold (fun n ->
                                    if n>0 then 
                                        Some(n % 10, n/10) 
                                    else 
                                        None) n 
                        |> Seq.toList
            if x <> List.rev (List.sort x) 
                && x <> List.sort x then
                if (float (cnt+1))/(float n) >= 0.99 then 
                    n
                else
                    loop (n+1) (cnt+1)
            else 
                loop (n+1) cnt
        loop 1 1
    let hundredThirteen start =
        let chooseNR n r =
            (helper.factorial n)/(helper.factorial (r))/ (helper.factorial (n-r))
        let n = 100
        (chooseNR (n+10) 10)+(chooseNR (n+9) 9)-10I*(bigint n)-2I
    let hundredThirteen_janky start = //from observed pattern
        let chooseNR n r =
            (helper.factorial n)/
            (helper.factorial (r))/ 
            (helper.factorial (n-r))
        let d = 100
        let l1 =List.init (d-1) (fun n -> //d-1 because we add 9I onto l1
                                    chooseNR (n+10) (n+2))
        let l2 = l1.Tail
                    |> List.mapi (fun i n->
                                    helper.listSlice [0..i] l1 |> List.sum)
        List.zip (9I::(l1 
                        |> List.map (fun x->2I*x))) (0I::0I::l2) 
            |> List.map (fun (a,b) ->
                            a+b)
            |> List.sum