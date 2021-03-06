﻿module helper 

    open System.Collections.Generic
    let rec gcd a b = 
        match (a % b) with
        |0 -> b
        |r -> gcd b r
    let isqrt n =
        let rec iter t =
            let d = n - t*t
            if (0 <= d) && (d < t+t+1) then t
            else iter ((t+(n/t))/2)
        iter 1
    let lsqrt n =
        let rec iter t =
            let d = n - t*t
            if (0L <= d) && (d < t+t+1L) then t
            else iter ((t+(n/t))/2L)
        iter 1L
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
        if n = 1 then [l] 
        else
            let rec sub e = function
                | [] -> failwith "sub"
                | h :: t -> 
                    if h = e then t 
                    else h :: sub e t
            let rec f k =
                let e = List.nth l k
                let t = List.map (fun a -> e::a) (perm (sub e l))
                if k < n-1 then t @ (f (k+1))
                else t
            f 0;; 
    let pythag stop max = 
        let num_to rule ms = seq{
            for m = 2 to isqrt (ms/2) do
            for j = 0 to (m/2) - 1 do
                let n = m-(2*j+1)
                if gcd m n =1 && stop (m*m-n*n) (2*m*n) (m*m+n*n) ms then
                    yield (m*m-n*n,2*m*n,m*m+n*n)}
        num_to stop max
    let get_web_txt (address:string) =
        let client = new System.Net.WebClient()
        let s = new System.IO.StreamReader(client.OpenRead(address))
        s.ReadToEnd()
        
        System.
