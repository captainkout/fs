module scratchpad
    open System


    let fstar sp1 sp2 = 
        let a,b = ("care").ToCharArray() |>List.ofArray  ,("race").ToCharArray() |>List.ofArray
        let zipper = List.zip (List.map (fun x1->List.findIndex (fun x2 -> x1=x2) a) a) (List.map (fun x1->List.findIndex (fun x2 -> x1=x2) a) b) |>List.sort

        let c = helper.comb a.Length [1..9] 
                |>List.map (fun (x:int list)-> helper.perm x)
                |>List.fold (fun acc l-> acc@l) []
                |> List.filter (fun x1 -> pown (List.fold (fun acc x2->acc+(string x2)) "" x1 |> int  |> helper.isqrt) 2 = (List.fold (fun acc x2->acc+(string x2)) "" x1|> int ))
        let unzipper (a1:int list) m= 
            let arr = Array.zeroCreate a1.Length
            let rec loop a2 m2=
                match a2,m2 with
                |h1::t1, h2::t2-> 
                        arr.[snd h2]<- h1
                        loop t1 t2
                |_,_-> arr |> List.ofArray
            loop a1 m
        List.filter (fun (l:int list)-> List.exists (fun elem->elem=(unzipper l zipper)) c) c
    let test r b =
        match box b with
        | :? System.Int32 -> 
            unbox b |> (+) 5 |> box |> r
        | :? System.Double -> 
            unbox b |> (+) 5. |> box |> r
        | _ -> failwith "some other type"
    //test (fun x -> System.Convert.ToInt32 x) 5 |> (+) 5