module helper 
    open System.Collections.Generic
    let rec gcd a b = 
        match (a % b) with
        |0 -> b
        |r -> gcd b r
    let isqrt n =
        let rec iter t =
            let d = n - t*t
            if (0 <= d) && (d < t+t+1) then t// t*t <= n < (t+1)*(t+1)
            else iter ((t+(n/t))/2)
        iter 1
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
        if n = 1 then 
            [l] 
        else
            let rec sub e = function
                | [] -> failwith "sub"
                | h :: t -> 
                    if h = e then 
                        t 
                    else 
                        h :: sub e t
            let rec f k =
                let e = List.nth l k
                let t = List.map (fun a -> e::a) (perm (sub e l))
                if k < n-1 then 
                    t @ (f (k+1))
                else 
                    t
            f 0;; 
    let pythag stop= 
        let rec loop work =seq {
            match work with
            |[]->()
            |h::t->
                match h with
                |(a,b,c) ->
                    if stop a b c then
                        yield (a,b,c)
                        yield! loop ((-a+2*b+2*c,-2*a+b+2*c,-2*a+2*b+3*c)::
                                    (a+2*b+2*c,2*a+b+2*c,2*a+2*b+3*c)::
                                    (a-2*b+2*c,2*a-b+2*c,2*a-2*b+3*c)::t)
                    else yield! loop t}
        loop [(3,4,5)]
    let pythag_ros stop max = 
        let num_to rule ms = seq{
            for m = 2 to isqrt (ms/2) do
            for j = 0 to (m/2) - 1 do
                let n = m-(2*j+1)
                if gcd m n =1 && stop (m*m-n*n) (2*m*n) (m*m+n*n) ms then
                    yield (m*m-n*n,2*m*n,m*m+n*n)}
        num_to stop max