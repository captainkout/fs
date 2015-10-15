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
    let mutable localpos =  if i=0 then set [1;2;3;4;5;6;7;8;9]
                            else set[i] 
    let mutable localorgpos =       if i=0 then set []
                                    else set [1;2;3;4;5;6;7;8;9]
    let grouper a b =   match a,b with
                        |a1,b1 when a1<=3   && b1<=3                ->1
                        |a1,b1 when a1<=3   && b1>3 && b1<=6        ->2
                        |a1,b1 when a1<=3   && b1>6                 ->3
                        |a1,b1 when a1>3    && a1<=6    && b1<=3    ->4
                        |a1,b1 when a1>3    && a1<=6    && b1>3 && b1<=6 ->5
                        |a1,b1 when a1>3    && a1<=6    && b1>6     ->6
                        |a1,b1 when a1>6    && b1<=3                ->7
                        |a1,b1 when a1>6    && b1>3     && b1<=6    ->8
                        |a1,b1 when a1>6    && b1>6                 ->9
                        |a1,b1 ->999999
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
    member this.orgpos
        with get() = localorgpos
        and set(value)= localorgpos<-value


let Main frig =
    let f start =
        let s = (helper.get_web_txt "https://projecteuler.net/project/resources/p096_sudoku.txt").Split [|'\n'|]
        let mutable lstr =""
        let thesum = ref 0
        for i in 51..70 do
            //printfn "%A" lstr
            if i % 10 <>0 then 
                lstr <- lstr + s.[i]
            else
                let arr = lstr.ToCharArray() |>Array.map (fun x -> (int x)-48)
                let arr2 = [|for x in 1..9 do for y in 1..9 do yield Square(x,y,arr.[9*(x-1)+y-1],arr.[9*(x-1)+y-1])|]
                lstr<-""
                let updated = ref true
                let mutable remaining = 81
                let arrzeros = Array.create 81 (0,Set.empty)
                let mutable attempts = (0,0)
                while (updated.Value =true) || remaining>0 do
                    //Array.sortInPlaceBy (fun (s1:Square)-> s1.pos.Count) arr2
                    if updated.Value=true then
                        updated.Value<-false

                        let upd (s1:Square) =
                            let clean (s2:Square) =
                                if s2.pos.Count=1 && s2.c=0 then
                                    s2.c<- Set.toList s2.pos |>List.head
                                    updated.Value<-true

                            let part0 (s2:Square) = 
                                if (s2.x<>s1.x ||s2.y<>s1.y) && //isn't exact same
                                    (s2.x = s1.x || s2.y = s1.y || s2.g = s1.g) && //but same row col group
                                    s2.pos.Count>1 && s2.pos.Contains s1.c then 
                                        s2.pos <- (Set.remove s1.c s2.pos)
                                        s2.c<- Set.toList s2.pos |>List.head
                                        updated.Value<-true 
                                        //Array.iter clean arr2

                            let part1 (s2:Square) =
                                let rowhidden (acc:int Set) (s3:Square) = 
                                    if (s2.x<>s3.x ||s2.y<>s3.y) && (s2.x = s3.x) then 
                                        Set.op_Addition (acc,s3.pos)
                                    else acc
                                let colhidden (acc:int Set) (s3:Square) = 
                                    if (s2.x<>s3.x ||s2.y<>s3.y) && (s2.y = s3.y) then 
                                        Set.op_Addition (acc,s3.pos)
                                    else acc
                                let grouphidden (acc:int Set) (s3:Square) = 
                                    if (s2.x<>s3.x ||s3.y<>s1.y) && (s2.g = s3.g) then 
                                        Set.op_Addition (acc,s3.pos)
                                    else acc

                                let rhset = Array.fold rowhidden (set[]) arr2 
                                let chset = Array.fold colhidden (set[]) arr2
                                let ghset = Array.fold grouphidden (set[]) arr2

                                if s2.pos.Count>1 && (Set.op_Subtraction (s2.pos,rhset)).Count = 1 then
                                    s2.pos  <-Set.op_Subtraction (s2.pos,rhset)
                                    s2.c    <- (s2.pos|>Set.toList).[0]
                                    //Array.iter clean arr2
                                    updated.Value<-true
                                if s2.pos.Count>1 && (Set.op_Subtraction (s2.pos,chset)).Count = 1  then
                                    s2.pos  <-Set.op_Subtraction (s2.pos,chset)
                                    s2.c    <-(s2.pos|>Set.toList).[0]
                                    //Array.iter clean arr2
                                    updated.Value<-true
                                if s2.pos.Count>1 && (Set.op_Subtraction (s2.pos,ghset)).Count = 1 
                                    then
                                    s2.pos  <-Set.op_Subtraction (s2.pos,ghset)
                                    s2.c    <-(s2.pos|>Set.toList).[0]
                                    //Array.iter clean arr2
                                    updated.Value<-true
                            Array.iter clean arr2
                            Array.iter part0 arr2
                            Array.iter part1 arr2
                            Array.iter clean arr2                            
                        Array.iter upd arr2
                        remaining   <-  Array.fold (fun acc (s1:Square) ->  if s1.c = 0 then acc+1
                                                                            else acc) 0 arr2
                    else //problem
                        //Array.iter update arr2
                        //Array.sortInPlaceBy (fun (s1:Square)-> 9*(s1.x-1)+s1.y-1 ) arr2

                        Array.iter (fun (s1:Square)->   if s1.y=9 then printfn "%A  " s1.c
                                                        else printf "%A  " s1.c ) arr2
                        printfn "buggah %A" remaining
                        //Array.sortInPlaceBy (fun (s1:Square)-> s1.pos.Count) arr2
                        if attempts = (0,0) then 
                            for s in arr2 do
                                s.orgpos <- s.pos
                        else 
                            if  snd attempts = arr2.[fst attempts].pos.Count then
                                attempts <- ((fst attempts)+1,0)
                            else attempts <-((fst attempts),(snd attempts)+1)

                            for s in arr2 do
                                s.c<-s.i
                                s.pos<-s.orgpos

                        if arr2.[fst attempts].c=0 then
                            arr2.[fst attempts].c <- (Set.toList arr2.[fst attempts].pos).[snd attempts]
                            arr2.[fst attempts].pos <- (Set.remove arr2.[fst attempts].c arr2.[snd attempts].pos)

                        updated.Value<-true
                  
                Array.sortInPlaceBy (fun (s1:Square)-> 9*(s1.x-1)+s1.y-1 ) arr2
                Array.iter (fun (s1:Square)->   if s1.y=9 then printfn "%A  " s1.c
                                                else printf "%A  " s1.c ) arr2
                printfn "%A" (arr2.[0].c,arr2.[1].c,arr2.[2].c) 

                thesum.Value <- thesum.Value + (int ((string arr2.[0].c) + (string arr2.[1].c) + (string arr2.[2].c)))
        thesum.Value
    garbage f
    

Main "bologna"
System.Console.ReadKey() |>ignore