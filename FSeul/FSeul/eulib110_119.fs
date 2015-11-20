module eulib110_119
    let hundredEleven start = 
        let primes = helper.primeSeqL (pown 10L 6) 
                        |> Seq.toArray
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
    type Bouncy = 
        |Dec
        |Inc
        |Unknown
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
            if (cnt+newcnt)/(n-1L |> float) = 0.99 then 
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
        let l1 = List.init (d-1) (fun n -> //d-1 because we add 9I onto l1
                                    chooseNR (n+10) (n+2))
        let l2 = l1.Tail
                    |> List.mapi (fun i n->
                                    helper.listSlice [0..i] l1 |> List.sum)
        List.zip (9I::(l1 
                        |> List.map (fun x->2I*x))) (0I::0I::l2) 
            |> List.map (fun (a,b) ->
                            a+b)
            |> List.sum
    let hundredFourteen start =
        let n =50
        let rec loop i l = //http://oeis.org/A005252
            match l with 
            |n1::n2::n3::n4::t when i>3 -> loop (i-1) ((2L*n1-n2+n4)::n1::n2::[n3])
            |h::t -> h
            |_ -> failwith "too short"
        loop n [2L;1L;1L;1L]
    let hundredFifteen start = //http://oeis.org/A005252 general solution
        let m = 50
        let rec loop l m n =
            match l with
            |n0::t when n0 >=1000000L -> (n-1)
            |n1::n2::t -> loop ((2L*n1-n2+l.[m])::l) m (n+1)
            |_ -> failwith "too short"
        loop (2L::(List.init m (fun i-> 1L))) m (m+1)
    let hundredSixteen start =
        let n = 50
        let f m n =
            let rec loop l jlst =
                match l,jlst with
                |_,[] -> List.head l
                |j::t,hh::tt -> 
                    loop ((j+l.[m-1])::(List.rev l 
                                        |> List.tail 
                                        |> List.rev)) tt
                |_,_ -> failwith "too short"
            loop (List.init m (fun x->if x=0 then 2L else 1L)) [m..n-1]
        (f 2 n)+(f 3 n)+(f 4 n)-3L
    let hundredSeventeen start =
        let m = 50L
        let rec loop l i =
            match l with
            |h::t when i<=0L -> h
            |_ -> 
                loop ((List.sum l)::(List.rev l 
                                        |> List.tail 
                                        |> List.rev)) (i-1L)
        loop [8L;4L;2L;1L] (m-4L)
    let hundredEightTeen start =
        let poss = helper.perm [1..9]
        let primes = helper.primeSeq (helper.isqrt (pown 10 9))
                        |> Seq.toList
        let maps = [for one in 0..1 do
                    for two in 0..1 do
                    for three in 0..1 do
                    for four in 0..1 do
                    for five in 0..1 do
                    for six in 0..1 do
                    for seven in 0..1 do
                    for eight in 0..1 do
                    for nine in 0..1 do
                    yield [one;two;three;four;five;six;seven;eight;nine]]
        let rec isprime i plist isq =
            match plist with
            |_::_ when i=1 -> false
            |ph::pt when i = ph || ph > isq -> true
            |ph::pt when i % ph =0 ->false
            |ph::pt -> isprime i pt isq
            |_ ->failwith "you messed up"
        let rec loop1 digits mapval acc prev =
            match digits,mapval,acc with
            |dh::dt,mh::mt,[]-> loop1 dt mt [dh] mh
            |dh::dt,mh::mt,ah::at->
                if prev = -1 || prev=mh  then loop1 dt mt ((ah*10+dh)::at) mh
                elif isprime ah primes (helper.isqrt ah) then 
                    loop1 dt mt (dh::acc) mh
                else set[]
            |_,_,_-> 
                if isprime (List.head acc) primes (helper.isqrt (List.head acc)) then 
                    Set.ofList acc
                else set[]
        let rec loop2 poslist acc =
            match poslist with
            |[] -> (Set.count acc)-1 //minus one for empty set
            |ph::pt -> 
                let ns = List.map (fun l -> loop1 ph l [] (-1)) maps |> Set.ofList
                loop2 pt (Set.union ns acc)
        loop2 poss (set[])
    let hundredNineTeen start =
        let rec test i acc targ =
            if i=1I || acc > targ then false
            elif acc = targ then true
            else test i (i*acc) targ
        let rec loop2 l s =
            match l with
            |[] -> s
            |h::t ->
                let x = helper.sumdig 0I h
                if test x x h then
                    loop2 t (Set.add h s)
                else loop2 t s
        let rec loop1 e s=
            let l = List.init 1000 (fun i-> pown (bigint i) e)
            if Set.count s < 100 then
                loop2 l s 
                    |> Set.union s
                    |> loop1 (e+1)
            else Set.filter (fun i-> i>11I) s |> Set.toList
        (loop1 2 (set[])).[29]
