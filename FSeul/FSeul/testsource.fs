module testsource 

    let function2 x =
        for i in 1 .. 2 .. x do
            printf "%d /t" i
        printfn "whatever"
    let slow a b=
        let rec loop acc counter =
            match counter>1 with
            | true ->loop (acc+a) (counter-1)
            | _ -> acc
        loop a b
    let slow2 a b = 
        let s = [1 .. b]
        List.fold(fun acc elem ->acc+a) 0 s
    let fast a b =
        a*b
