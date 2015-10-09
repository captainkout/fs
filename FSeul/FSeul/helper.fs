module helper 
    open System.Collections.Generic
    let rec gcd a b = 
        match (a % b) with
        |0 -> b
        |r -> gcd b r
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
