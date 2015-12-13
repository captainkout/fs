#time "on"
//let arr = [|0..1000|]
//let arrTest a = Array.head arr, Array.tail arr
//let lst = [0..1000]
//let lstTest l = List.head l, List.tail l

//let rec arrBuild a =function
//    |0-> a
//    |i->arrBuild (Array.append [|i|] a) (i-1)
//arrBuild [||] 10

//let rec lstBuild l = function
//    |0->List.rev l
//    |i->lstBuild (i::l) (i-1)

open System.
let rec time f n = function
    |0->"done"
    |i->let x = f n
        time f n (i-1) 
//time arrTest arr 100000
//time lstTest lst 100000
//time arrBuild [||] 10000
//time lstBuild [] 10000
time (fun i-> Array.init i (fun i2->i2)) 100 100
//time (fun i-> List.init i (fun i2->i2)) 1000 10000

let x = 100