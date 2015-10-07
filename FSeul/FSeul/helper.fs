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
    let perm arr =
        let next_perm (arr:array<int>) = 
            let mutable i= (arr.Length)-1
            while i>0 && arr.[i-1] >= arr.[i] do
                i<- i-1
            if i<=0 then
                []
            else
                let mutable j = (arr.Length)-1
                while arr.[j]<=arr.[i-1] do
                    j<-j-1
                let tmp = arr.[j]
                arr.[j]<-arr.[i-1]
                arr.[i-1]<-tmp
                while i<j do
                    let tmp2 = arr.[i]
                    arr.[i]<-arr.[j]
                    arr.[j]<-tmp2
                    i<-i+1
                    j<-j-1
                List.ofArray arr
        let rec f arr l = 
            match next_perm arr with
            |[] -> List.rev l
            |a -> f arr (a::l)
        f arr [(List.ofArray arr)]
