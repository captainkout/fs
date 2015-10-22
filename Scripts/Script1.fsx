let isqrt x = 
    x|>float|>sqrt |>int
let fracroot root = 
    let rec loop m0 d0 a0 s l = 
        let m1= d0*a0-m0
        let d1 = (root-(m1*m1))/d0
        let a1 = ((isqrt root)+m1)/d1
        if Set.contains (m1,d1,a1) s then
            let rec loop2 l0 (l1:(int*int*int) list) = 
                match l0 with
                |[] ->      [l0 |> List.map (fun (a,b,c)->c) |> List.rev ; l1 |> List.map (fun (a,b,c)->c) ]
                |h::t when h <> (m1,d1,a1)->    loop2 t (h::l1)
                |h::t ->    [t |> List.map (fun (a,b,c)->c) |> List.rev ; h::l1 |> List.map (fun (a,b,c)->c) ]
            loop2 l []
        else
            loop m1 d1 a1 (Set.add (m1,d1,a1) s) ((m1,d1,a1)::l)
    loop 0 1 (isqrt root) (Set.add (0,1,(isqrt root)) Set.empty)  [(0,1,(isqrt root))]
//fracroot 94
let fracunpack l =
    List.fold (fun acc x-> x @ acc) [] l
fracunpack (fracroot 94)