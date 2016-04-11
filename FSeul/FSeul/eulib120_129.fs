module eulib120_129

    let hundredTwenty start = 
        List.init 1000 (fun i ->    if i>=3 then
                                        if i % 2=1 then i*i-i
                                        else i*i-2*i
                                    else 0)
            |> List.sum