// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open eulib91_99
let garbage f = 
    GC.Collect()
    GC.WaitForFullGCComplete() |>ignore
    GC.WaitForPendingFinalizers()

    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()

    (f 0) |> printfn "%A"
    timer.Elapsed |>printfn "%A"


type Square (x,y,i,c) =
    let mutable localc = c
    let mutable localpos =  if i=0 then set [1;2;3;4]
                            else set[i] 
    let mutable triedpos =  if i=0 then set []
                            else set [1;2;3;4]
    let grouper a b =   match a,b with
                        |a1,b1 when a1<=2 &&    b1<=2    ->1
                        |a1,b1 when a1>2  &&    b1<=2    ->2
                        |a1,b1 when a1<=2 &&    b1>=2    ->3
                        |a1,b1 when a1>2  &&    b1>=2    ->4
                        |a1,b1->99999
//    let grouper a b =   match a,b with
//                        |a1,b1 when a1<=3   && b1<=3                ->1
//                        |a1,b1 when a1>3    && a1<=6    && b1<=3    ->2
//                        |a1,b1 when a1>6    && a1<=9    && b1<=3    ->3
//                        |a1,b1 when a1<=3   && b1>3     && b1<=6    ->4
//                        |a1,b1 when a1>3    && a1<=6    && b1>3     && b1<=6 ->5
//                        |a1,b1 when a1>3    && a1<=6    && b1>6     ->6
//                        |a1,b1 when a1>6    && b1<=3                ->7
//                        |a1,b1 when a1>6    && b1>3    && b1<=6     ->8
//                        |a1,b1 when a1>6&& b1>6 ->9
//                        |a1,b1 ->999999
    member this.x = x
    member this.y = y
    member this.i = i
    member this.c
        with get() = localc
        and set (value) = localc <- value
    member this.g = (grouper x y)
    member this.pos
         with get() = localpos
         and set(value)  = localpos <- value
    member this.tried
        with get() = triedpos
        and set(value) = triedpos<-value
let Main frig =
    let f start =
        let lstr = "4030000100033010"
        let arr = lstr.ToCharArray() |>Array.map (fun x -> (int x)-48)
        let arr2 = [|for x in 1..4 do for y in 1..4 do yield Square(x,y,arr.[4*(x-1)+y-1],arr.[4*(x-1)+y-1])|]

        while (Array.fold (fun acc (s1:Square) ->   if s1.c = 0 then acc+1
                                                    else acc) 0 arr2) >0 do
            Array.sortInPlaceBy (fun (s1:Square)-> s1.pos.Count) arr2
            let update (s1:Square) = 
                Array.iter (fun (s2:Square) -> 
                            if (s2.x = s1.x || s2.y = s1.y || s2.g = s1.g) then 
                                s2.pos <- (Set.remove s1.c s2.pos)) arr2
                Array.iter (fun (s2:Square) ->
                            if s2.pos.Count=1 then
                                s2.c<- Set.toList s2.pos|>List.head) arr2
            Array.iter update arr2


        Array.sortInPlaceBy (fun (s1:Square)-> 4*(s1.x-1)+s1.y-1 ) arr2
        Array.iter (fun (s1:Square)->   if s1.y=4 then printfn "%A  " s1.c
                                        else printf "%A  " s1.c ) arr2

    garbage f
    

Main "bologna"
System.Console.ReadKey() |>ignore