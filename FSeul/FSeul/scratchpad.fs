module Scratchpad
    open System

    let test r b =
        match box b with
        | :? System.Int32 -> 
            unbox b |> (+) 5 |> box |> r
        | :? System.Double -> 
            unbox b |> (+) 5. |> box |> r
        | _ -> failwith "some other type"
    //test (fun x -> System.Convert.ToInt32 x) 5 |> (+) 5


    let rec loop n= seq{
        let x = Seq.unfold (fun n ->
                                if n > 0 then 
                                    Some(n % 10, n/10) 
                                else 
                                    None) n 
                    |> Seq.toList
        if x = List.rev (List.sort x) (*&& x <> List.sort x*) then
                yield n
                yield! loop (n+1)
        else 
            yield! loop (n+1) }
    let s = loop 1

    for a in 0..4 do
        //let sn = Seq.takeWhile (fun x->x<(pown 10 a)) s |> Seq.length
        Seq.takeWhile (fun x->(x<pown 10 (a+1))) s
            |> Seq.length
            |> printfn "%A"


////////////
//        let primes = helper.primeSeq (pown 10 6 (*max is 98765431*)) //largest from prob 41
//                        |> Seq.map (fun i -> (i,helper.idigcnt i))
//                        |> Seq.filter (fun (i,l)-> List.head l = 0 && List.max l =1 )
//                        |> Seq.toList
////            |> Seq.maxBy (fun (i,l)-> i)
////            |> Seq.length
////        let primes = [2;3;5;7;13;15;17;19;21;23;98765431] 
////                        |> List.map (fun i -> (i,helper.idigcnt i))
//

//
//        let linv one =
//            let rec loop acc = function
//                |0::t-> loop (0::acc) t
//                |1::t-> loop (1::acc) t
//                |[] -> List.rev acc
//                |_ -> failwith "something went wrong"
//            loop [] one
//
//        let rec loop2 v testval ptail (m:Map<int list,int Set Set>) =
//            if List.isEmpty ptail then m
//            else
//                match ptail with
//                |[] -> m
//                |(pi,pl)::pt ->
//                    match ladd testval pl with
//                    |[] -> loop2 v testval pt m
//                    |added when List.max added = 1 ->
//                        if Map.containsKey added m then 
//                            loop2 v testval pt (Map.add added (Set.add (set[v;pi]) (m.[added])) m)
//                        else 
//                            loop2 v testval pt (Map.add added (set[set[v;pi]]) m)
//                    |_ -> failwith "list add failed"
//
//
//        let rec loop1 plist (m:Map<int list,int Set Set>) =
//            match plist with
//            |[] ->
//                //m 
//                m.[[0;1;1;1;1;1;1;1;1;1]]
//            |(i,l)::t ->
//                if Map.containsKey l m then
//                    loop2 i l t (Map.add l (Set.add (set[i]) m.[l]) m)
//                        |> loop1 t
//                else
//                    loop2 i l t (Map.add l (set[set[i]]) m)
//                        |> loop1 t
//        loop1 primes (Map.empty<int list,int Set Set>)
////            |> Map.toList
////            |> helper.listPrint