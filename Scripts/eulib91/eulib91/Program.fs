// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
let main = 
    let l = [for x in 0..50 do
                for y in 0..50 do
                yield (x,y)]
    
    let rec extx one two l = 
        match one,two with
        |(x1,y1),(x2,y2) when x1=0 && x1<=50 && y1<=50 && x2<=50 && y2<=50->
            extx (x1,y1) (x2,0) ((x1,y1,x2,y2)::(
                extx (x1,y1) (x2+1,y2) ((x1,y1,x2+1,y2)::l)))
        |(_,y_), (_,_) -> l
    
    let rec f l = 
        match l with
        |h::t-> ignore 
        |[]-> ignore

    extx (0,1) (1,1) [] // return an unit exit code

let m = main
for a in m do
    Console.WriteLine(a)
Console.ReadKey()
    |> ignore