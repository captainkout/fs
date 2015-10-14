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
    let mutable triedpos =  if i=0 then set []
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
    member this.tried
        with get() = triedpos
        and set(value) = triedpos<-value


let Main frig =
    let f start =
        //let lstr =        "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
        //let lstr =        "200080300060070084030500209000105408000000000402706000301007040720040060004010003"
        //let lstr =        "000000907000420180000705026100904000050000040000507009920108000034059000507000000"
        //let lstr =        "630000000000500008005674000000020000003401020000000345000007004080300902947100080"
        //let lstr =          "360020089000361000000000000803000602400603007607000108000000000000418000970030014"
        //let lstr = "004000000000030002390700080400009001209801307600200008010008053900040000000000800"
        let s = (helper.get_web_txt "https://projecteuler.net/project/resources/p096_sudoku.txt").Split [|'\n'|]
        let mutable lstr =""
        for i in 1..s.Length-1 do
            if i % 10 <>0 then 
                lstr <- lstr + s.[i]
            else
                let arr = lstr.ToCharArray() |>Array.map (fun x -> (int x)-48)
                let arr2 = [|for x in 1..9 do for y in 1..9 do yield Square(x,y,arr.[9*(x-1)+y-1],arr.[9*(x-1)+y-1])|]
                lstr<-""
                let mutable updated = true
                let mutable remaining = 81

                while (updated =true) || remaining>0 do
                    Array.sortInPlaceBy (fun (s1:Square)-> s1.pos.Count) arr2
                    if updated=true then
                        updated<-false

                        let update (s1:Square) =
                            let part0 (s2:Square) = 
                                if (s2.x<>s1.x ||s2.y<>s1.y) &&
                                    (s2.x = s1.x || s2.y = s1.y || s2.g = s1.g) &&
                                    s2.pos <> (Set.remove s1.c s2.pos) then 
                                    s2.pos <- (Set.remove s1.c s2.pos)
                                    updated<-true 

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
                                    s2.c    <-Set.op_Subtraction (s2.pos,rhset)|>Set.toList |>List.head
                                    updated<-true
                                if s2.pos.Count>1 && (Set.op_Subtraction (s2.pos,chset)).Count = 1 then
                                    s2.pos  <-Set.op_Subtraction (s2.pos,chset)
                                    s2.c    <-Set.op_Subtraction (s2.pos,chset)|>Set.toList |>List.head
                                    updated<-true
                                if s2.pos.Count>1 && (Set.op_Subtraction (s2.pos,ghset)).Count = 1 then
                                    s2.pos  <-Set.op_Subtraction (s2.pos,ghset)
                                    s2.c    <-Set.op_Subtraction (s2.pos,ghset)|>Set.toList |>List.head
                                    updated<-true

                            let part2 (s2:Square) =
                                if (Set.op_Subtraction (s2.pos,s2.tried)).Count=1 && s2.c=0 then
                                    s2.c<- Set.toList s2.pos|>List.head
                                    updated<-true

                            Array.iter part0 arr2
                            Array.iter part1 arr2
                            Array.iter part2 arr2

                        Array.iter update arr2
                        Array.sortInPlaceBy (fun (s1:Square)-> 9*(s1.x-1)+s1.y-1 ) arr2
                        Array.iter (fun (s1:Square)->   if s1.y=9 then printfn "%A  " s1.c
                                                        else printf "%A  " s1.c ) arr2
                        printfn ""

                        remaining   <-  Array.fold (fun acc (s1:Square) ->  if s1.c = 0 then acc+1
                                                                            else acc) 0 arr2
                    else //problem
                        //Array.iter update arr2
                        Array.sortInPlaceBy (fun (s1:Square)-> 9*(s1.x-1)+s1.y-1 ) arr2
                        Array.iter (fun (s1:Square)->   if s1.y=9 then printfn "%A  " s1.c
                                                        else printf "%A  " s1.c ) arr2
                        printfn "buggah"
                        Array.sortInPlaceBy (fun (s1:Square)-> Set.op_Subtraction(s1.pos,s1.tried).Count) arr2
                        let firsttry = Array.findIndex (fun (s1:Square)-> s1.pos.Count>1) arr2
                        arr2.[firsttry].c <- Set.toList (Set.op_Subtraction (arr2.[firsttry].pos,arr2.[firsttry].tried)) |>List.head
                        arr2.[firsttry].tried <- Set.add arr2.[firsttry].c arr2.[firsttry].tried

                        updated<-true

            


                Array.sortInPlaceBy (fun (s1:Square)-> 9*(s1.x-1)+s1.y-1 ) arr2
                Array.iter (fun (s1:Square)->   if s1.y=9 then printfn "%A  " s1.c
                                                else printf "%A  " s1.c ) arr2

    garbage f
    

Main "bologna"
System.Console.ReadKey() |>ignore