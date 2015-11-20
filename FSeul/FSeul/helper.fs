module helper 
    open System.Collections.Generic
    let rec gcd a b = 
        match (a % b) with
        |0 -> b
        |r -> gcd b r
    let isqrt (i:int) =
        i |> float |> sqrt |> int 
    let Lsqrt (i:int64) = 
            let rec iter t =
                let d = i - t*t
                if (0L <= d) && (d < t+t+1L) then t
                else iter ((t+(i/t))>>>1)
            iter 1L
    let Isqrt (i:bigint) =
        let rec iter t =
            let d = i - t*t
            if (0I <= d) && (d < t+t+1I) then t
            else iter ((t+(i/t))>>>1)
        iter 1I
    let rec sumdig (acc:bigint) (x:bigint) =
        match x with
        |a when a=0I-> acc
        |a->sumdig (acc + (x % 10I) ) (x/10I) 
    let rec sumsqdig acc x =
        match x with
        |a when a=0-> acc
        |a->sumsqdig (acc+ (pown (x % 10) 2)) (x/10) 
    let rec comb m lst =
        match m, lst with
        | 0, _ -> [[]]
        | _, [] -> []
        | m, h :: t ->
            List.map (fun y -> h :: y) (comb (m-1) t) @ comb m t
    let rec perm l =
        let n = List.length l
        if n = 1 then [l] 
        else
            let rec sub e = function
                | [] -> failwith "sub"
                | h :: t -> 
                    if h = e then t 
                    else h :: sub e t
            let rec f k =
                let e = List.nth l k
                let t = List.map (fun a -> e::a) (perm (sub e l))
                if k < n-1 then t @ (f (k+1))
                else t
            f 0;; 
    let pythag stop max = 
        let num_to rule ms = seq{
            for m = 2 to isqrt (ms/2) do
            for j = 0 to (m/2) - 1 do
                let n = m-(2*j+1)
                if gcd m n =1 && stop (m*m-n*n) (2*m*n) (m*m+n*n) ms then
                    yield (m*m-n*n,2*m*n,m*m+n*n)}
        num_to stop max
    let pythag_byMin =
        let rec loop (queue:int list list) = seq {
            match queue with
            |[] -> [] |>ignore
            |h::t->
                yield h |> List.sort 
                let a,b,c = h.[0],h.[1],h.[2]
                yield!  [-a+2*b+2*c;  -2*a+b+2*c;  -2*a+2*b+3*c]::
                        [a+2*b+2*c;   2*a+b+2*c;  2*a+2*b+3*c]::
                        [a-2*b+2*c;   2*a-b+2*c;  2*a-2*b+3*c]::t
                        |> List.sortBy (fun x-> List.min x)
                        |> loop }
        loop [[3;4;5]]
    let get_web_txt (address:string) =
        let client = new System.Net.WebClient()
        let s = new System.IO.StreamReader(client.OpenRead(address))
        s.ReadToEnd().ToString().Trim()
    let fracroot root = 
        let rec loop m0 d0 a0 s l = 
            let m1= d0*a0-m0
            let d1 = (root-(m1*m1))/d0
            let a1 = ((isqrt root)+m1)/d1
            if Set.contains (m1,d1,a1) s then
                let rec loop2 l0 (l1:(int*int*int) list) = 
                    match l0 with
                    |[] ->      [l0 |> List.map (fun (a,b,c)->c) |> List.rev ; l1 |> List.map (fun (a,b,c)->c) ]
                    |h::t when h <> (m1,d1,a1)->    loop2 t (h::l1)
                    |h::t ->    [t |> List.map (fun (a,b,c)->c) |> List.rev ; h::l1 |> List.map (fun (a,b,c)->c) ]
                loop2 l []
            else
                loop m1 d1 a1 (Set.add (m1,d1,a1) s) ((m1,d1,a1)::l)
        loop 0 1 (isqrt root) (Set.add (0,1,(isqrt root)) Set.empty)  [(0,1,(isqrt root))]
    let listSlice range l =
        let rec loop nl nr (i:int) store= 
            match nl,nr with
            |hl::tl,hr::tr when hr=i-> loop (tl) (tr) (i+1) (hl::store)
            |hl::tl,hr::tr ->loop tl (nr) (i+1) store
            |_,_ -> List.rev store
        loop l range 0 []
    let fracrootConverge root conv = 
        let l = fracroot root
        let rec loop tail times= 
            if times=0 then tail
            else tail@(loop tail (times-1))
        let nl = (l.[0])@(loop (l.[1]) ((conv-l.[0].Length)/l.[1].Length)) |> listSlice [0..conv-1] |>List.rev
        let folder acc (x:int) = 
            let (n,d)= acc
            (n*(bigint x)+d,n)
        nl.Tail |> List.fold folder ((nl.Head |>bigint,1I))
    let pellLike B answer=  (* as in x^2-By^2=answer 
                            answer is hopefully near zero, 
                            and B is integer*)
        let rec loop (a,b) (root:int) conv=
            if conv <50 then
                let v1= a*a-(root|>bigint)*b*b
                if v1=answer then (a,b)
                else loop (fracrootConverge root (conv+1)) root (conv+1) 
            else failwith "nothing found"
        loop (0I,0I) B 0 
    let Ldigcnt (num:int64) =
        let arr = Array.zeroCreate 10
        let rec loop (n:int64) =
            arr.[n%10L|>int]<-arr.[n%10L|>int]+1
            if (n > 10L) then loop (n/10L)
            else arr|>List.ofArray
        loop num
    let idigcnt (num:int) =
        let arr = Array.zeroCreate 10
        let rec loop (n:int) =
            arr.[n%10|>int]<-arr.[n%10|>int]+1
            if (n > 10) then loop (n/10)
            else arr|>List.ofArray
        loop num
    let listPrint l = 
        List.iteri (fun i x -> printfn "%A\t%A" i x) l
    let primeSeq max=
        let bitarr = new System.Collections.BitArray((max/2)+1,true)
        bitarr.[0]<-false
        let newmax = (isqrt max)+1
        seq{yield 2
            for i in 1..(bitarr.Length-1) do
                if bitarr.[i] then
                    yield (2*i+1)
                    if i<newmax then
                        for j in (3*i+1)..(2*i+1)..(bitarr.Count-1) do
                            bitarr.[j]<-false }
    let primeSeqL max=
        let bitarr = new System.Collections.BitArray((max/2L)+1L|>int,true)
        bitarr.[0]<-false
        seq{yield 2L
            for i in 1..(bitarr.Length-1)do
                if bitarr.[i] then
                    yield (2*i+1 |>int64)
                    if i<(Lsqrt max|>int)+1 then
                        for j in (3*i+1)..(2*i+1)..(bitarr.Count-1) do
                        bitarr.[j]<-false }
    let pythagTrianglei i =
        let rec loop i prev acc =
            match i,prev with
            |0,_-> List.rev acc
            |_,[]-> loop (i) [1] [[1]]
            |_,_ ->
                let plen = List.length prev
                let next = (List.init (plen+1) (fun x -> 
                                                if x=0 || x >= plen then 
                                                    1
                                                else 
                                                    prev.[x-1]+prev.[x]) ) 
                loop (i-1) next (next::acc)
        loop i [] []
    let pythagTriangleL i =
        let rec loop i prev acc =
            match i,prev with
            |0,_-> List.rev acc
            |_,[]-> loop (i) [1L] [[1L]]
            |_,_ ->
                let plen = List.length prev
                let next = (List.init (plen+1) (fun x -> 
                                                        if x=0 || x >= plen then 
                                                            1L
                                                        else 
                                                            prev.[x-1]+prev.[x]) ) 
                loop (i-1) next (next::acc)
        loop i [] []
    let factorial n = 
        let rec loop acc i=
            if i<2 then acc
            else loop (acc*(bigint i)) (i-1)
        loop 1I n 
    let chooseNR n r =
        (factorial n)/(factorial (r))/ (factorial (n-r))
//    let csvWrite (s:seq<int64>) col name =
//        use streamWriter = new System.IO.StreamWriter(name+".txt")
//        s 
//            |> Seq.iteri (fun i p ->    if i % col = col-1 then streamWriter.Write(String.Format ("{0}\n",p))
//                                        else streamWriter.Write(String.Format("{0},",p))) 
//        do streamWriter.Close()
//        "complete"