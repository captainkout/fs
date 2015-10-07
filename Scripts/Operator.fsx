
let rec comb m lst = 
    match m, lst with
    |0,_->[[]]
    |_,[]->[]
    |m, x::xs -> List.map (fun y-> x::y) (comb (x-1) xs) @ (comb m xs)

comb 3 [0;1;2;3;4]|>printfn "%A"