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


type Square (row,col,i,c) =
    let mutable localc = c
    let mutable locali = i
    let mutable localpos =  if i=0 then set [1;2;3;4;5;6;7;8;9]
                            else set[i] 
    let grouper a b =   match a,b with
                        |a1,b1 when a1<3   && b1<3                  ->1
                        |a1,b1 when a1<3   && b1>3 && b1<6          ->2
                        |a1,b1 when a1<3   && b1>6                  ->3
                        |a1,b1 when a1>=3    && a1<6    && b1<3     ->4
                        |a1,b1 when a1>=3    && a1<6    && b1>=3 && b1<6 ->5
                        |a1,b1 when a1>=3    && a1<6    && b1>=6    ->6
                        |a1,b1 when a1>=6    && b1<3                ->7
                        |a1,b1 when a1>=6    && b1>=3   && b1<6     ->8
                        |a1,b1 when a1>=6    && b1>=6               ->9
                        |a1,b1 ->999999 //something went wrong
    member this.row = row
    member this.col = col
    member this.i
        with get() = locali
        and set(value) = locali <- value
    member this.c
        with get() = localc
        and set (value) = localc <- value
    member this.group = (grouper row col)
    member this.pos
         with get() = localpos
         and set(value)  = localpos <- value



let Main frig =
    let f start =
        let s = (helper.get_web_txt "https://projecteuler.net/project/resources/p096_sudoku.txt").Split [|'\n'|]
        let mutable lstr =""
        let thesum = ref 0
        for i in 1..100 do
            //printfn "%A" lstr
            if i % 10 <>0 then 
                lstr <- lstr + s.[i]
            else
                let arr = lstr.ToCharArray() |>Array.map (fun x -> (int x)-48)
                let arr2 = [|for row in 0..8 do for col in 0..8 do yield Square(row,col,arr.[9*row+col],arr.[9*row+col])|]
                lstr<-""

                let rec initial_loop index = //when i is>0 remove i from possibilites
                    if index < arr2.Length && arr2.[index].i <0 then
                        Array.iter (fun (s:Square) -> 
                                    if  (s.row <> arr2.[index].row || s.col <> arr2.[index].col) && 
                                        (s.row = arr2.[index].row || s.col = arr2.[index].col || s.group = arr2.[index].group) then
                                        s.pos <- Set.remove (arr2.[index].i) s.pos
                        ) arr2
                        initial_loop (index+1)
                    else ()
                initial_loop 0

                let rec second_loop index upd = 
                    if index < arr2.Length && arr2.[index].pos.Count = 1 then
                        arr2.[index].i <- Set.toList arr2.[index].pos |> List.head
                second_loop 0 false
                printfn "%A" (arr2.[0].c,arr2.[1].c,arr2.[2].c) 

                thesum.Value <- thesum.Value + (int ((string arr2.[0].c) + (string arr2.[1].c) + (string arr2.[2].c)))
        thesum.Value
    garbage f
    

Main "bologna"
System.Console.ReadKey() |>ignore