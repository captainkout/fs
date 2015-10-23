module eulib100_109

    let hundred start = 
        let rec loop b n max =
            if n<max then
//                printfn "%A \t %A" b n
                loop (3L*b + 2L*n - 2L) (4L*b + 3L*n - 3L) max
            else b,n
        loop 15L 21L (pown 10L 12)