open System

let timer = new System.Diagnostics.Stopwatch()
timer.Start()
 // this is the seq part .0291954
let x = seq {for a in [1..5] do
                for b in [1..5] do
                    for c in [1..5] do 
                        yield (a,b,c) }
Seq.iter( fun elem ->
    match elem with
    | (a,b,c)-> Console.WriteLine("({0},{1},{2})",a,b,c)) x
timer.Stop()
timer.Elapsed |> Console.WriteLine

 // this is the list part .043782
timer.Restart()
let x2 = [for a in 1..5 do
            for b in 1..5 do
            for c in 1..5 do
            yield (a,b,c)]

let rec show l = 
    match l with
    |h::t ->
        match h with
        | (a,b,c)->
            Console.WriteLine("({0},{1},{2})",a,b,c)
        show t
    |[]-> ()
        
show x2
timer.Stop()
timer.Elapsed |> Console.WriteLine

 // this is a purely imperative version .0297107
timer.Restart()
for a in 1..5 do 
    for b in 1..5 do 
        for c in 1..5 do

        let tup = (a,b,c)
        match tup with
        |(a,b,c)-> Console.WriteLine("({0},{1},{2})",a,b,c)
timer.Stop()
timer.Elapsed |> Console.WriteLine