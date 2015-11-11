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