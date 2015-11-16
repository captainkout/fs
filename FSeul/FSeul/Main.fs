// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System

let garbage f = 
    GC.Collect()
    GC.WaitForFullGCComplete() |>   ignore
    GC.WaitForPendingFinalizers()
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    (f 0) |> printfn "%A"
    timer.Elapsed |>printfn "%A"


let Main frig =
    let f start =
//        let primes = helper.primeSeq (98765432) 
//                        |> Seq.map (fun i -> (i,helper.idigcnt i))//idigcnt takes int-> list , ex 9609 =[1;0;0;0;0;1;0;0;2]
//                        |> Seq.filter (fun (i,l)-> List.head l = 0 && List.max l =1 )
//                        |> Seq.toList
////        let primes = [2;3;5;7;13;17;19;23;29;31;98765431;98765431;98765421] |> List.map (fun i -> (i,helper.idigcnt i))
//        let rec counter m l = //turn list of primes into a map of [digit list]
//            match l with
//            |[]-> m
//            |(hi,hl)::t ->
//                if Map.containsKey hl m then
//                    counter (Map.add hl (m.[hl]+1) m) t
//                else 
//                    counter (Map.add hl 1 m) t
//        let ladd one two =
//            let rec loop l1 l2 acc =
//                match l1,l2 with
//                |h1::t1,h2::t2 ->
//                    if h1+h2>1 then []  //digit repeated, invalid
//                    else loop t1 t2 ((h1+h2)::acc) //continue
//                |[],[] -> List.rev acc //still valid, so reverse it to put back in digital order
//                |_,_-> failwith "how did this happen?"
//            loop one two []
//        let rec loop2 testval ptail (m:Map<int list,int>) = //looping through all the combinations
//            match ptail with
//            |[] -> Map.remove testval m //if finished return a map with test value removed, because we already tried on all combos
//            |(pl,amt)::pt ->
//                match ladd testval pl with  // are combinations valid
//                |[] -> loop2 testval pt m   //if its empty then its invalid
//                |added when List.max added = 1 ->
//                    if Map.containsKey added m then
//                        Map.add added (m.[added]+(m.[testval]*amt)) m
//                            |> loop2 testval pt 
//                    else 
//                        Map.add added (m.[testval]*amt) m
//                            |> loop2 testval pt 
//                |_ -> failwith "list add failed"
//        let rec loop1 (m:Map<int list,int>) = //take the map turn it into a list
//            match m |> Map.toList with
//            |[(hl,hamt)]-> hamt
//            |(hl,hamt)::t -> 
//                loop2 hl t m //loop through all possible combinations
//                    |> loop1
//            |_->failwith "Something went wrong"
//        counter (Map.empty<int list,int>) primes
//            |> loop1 
        let primes = helper.perm [1..9]
        let rec loop i l acc=
            match i, l with
            |_,[]->0 
            |1|0,h::t -> i*10+h
        [for a in 1..8 do
            for b in 1..(8-a) do
            for c in 1..(8-a-b) do
            for d in 1..(8-a-b-c) do
            for e in 1..(8-a-b-c-d) do
            for f in 1..(8-a-b-c-d-e) do
            for g in 1..(8-a-b-c-d-e-f) do
            for h in 1..(8-a-b-c-d-e-f-g) do ]

                
    garbage f

Main "bologna"
System.Console.ReadKey() |>ignore