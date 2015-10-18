// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open eulib91_99
let garbage f = 
    GC.Collect()
    GC.WaitForFullGCComplete() |>   ignore
    GC.WaitForPendingFinalizers()
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    (f 0) |> printfn "%A"
    timer.Elapsed |>printfn "%A"

type Word (s:string)=
    let init s =
        let arr = Array.zeroCreate 26
        String.iter (fun (c:char) -> arr.[(int c)-65]<-arr.[(int c)-65]+1) s
        List.ofArray arr
    let mutable (localanagram:Word list) = []
    member this.spelled = s
    member this.lettercnt =  (init s)
    member this.ordered = this.spelled.ToCharArray() 
                            |>Array.map (fun x-> Array.findIndex (fun x1-> x=x1) (this.spelled.ToCharArray())) 
                            |>List.ofArray
    member this.anagram
        with get() = localanagram
        and set(value) = localanagram <- value
type Long (num:int64) = 
    let init n = 
        let arr = Array.zeroCreate 10
        string n |> String.iter (fun (c:char)-> arr.[(int c)-48]<-arr.[(int c)-48]+1)
        List.ofArray arr
    let mutable localanagram = []
    member this.num = num
    member this.numcnt = (init num)
    member this.anagram 
        with get() = localanagram
        and set(value) = localanagram <- value

let Main frig =
    let f start = 
        let wordlist =  (helper.get_web_txt "https://projecteuler.net/project/resources/p098_words.txt").Replace("\"","").Split([|','|])
                        |>  Array.map (fun w->Word w )
        let wanagrams (w:Word) = 
            let (testarr:Word []) = [||]
            if Array.tryFind (fun (w1:Word) -> w.spelled <> w1.spelled && w.lettercnt=w1.lettercnt) wordlist = None then 
                w.anagram   <-  testarr |> List.ofArray
            else w.anagram  <-  Array.FindAll(wordlist,(fun (w1:Word) -> w.spelled <> w1.spelled && w.lettercnt=w1.lettercnt))
                                    |>List.ofArray
        Array.iter wanagrams wordlist
        let newwordlist = Array.filter (fun (w:Word) -> w.anagram<>[]) wordlist |> List.ofArray |> List.sortBy (fun (w:Word) -> w.ordered)
        List.iter (fun (w:Word) -> printfn "%A" (w.spelled,w.ordered,w.anagram.[0].spelled)) newwordlist



//        let numlist = Array.map (fun l -> Long l) [|    let mutable n =1L
//                                                        let mutable nn = n*n
//                                                        while (string (nn)).Length<=9 do
//                                                            yield nn
//                                                            n<-(n+1L)
//                                                            nn<- n*n|]
//        let nanagrams (l:Long) = 
//            if Array.tryFind (fun (l1:Long) -> l.num <> l1.num && l.numcnt = l1.numcnt) numlist = None then
//                l.anagram <-    false
//            else l.anagram <-   true
//        Array.iter nanagrams numlist 
//        Array.filter (fun (l:Long)-> l.anagram) numlist |> List.ofArray |>List.length

//        let rec mloop (m:Map<int List,float List>) n max =
//            let nn = (n*n).ToString()
//            if nn.Length <= max then
//                let l = nn.ToCharArray() 
//                        |>Array.map (fun x-> Array.findIndex (fun x1-> x=x1) (nn.ToCharArray())) 
//                        |>List.ofArray
//                if Map.containsKey l m then 
//                    mloop (Map.add (l) ((float nn)::(m.Item(l))) m) (n+1L) max
//                else mloop (Map.add l [(float nn)] m) (n+1L) max
//            else m
//        let m = mloop (Map.add [0] [0.0] Map.empty) (1L) (List.fold (fun acc (w:Word)->
//                                                            if w.spelled.Length>acc then w.spelled.Length 
//                                                            else acc) 0 newwordlist)
//        let fm = Map.map (fun (l:int list)-> List.map (fun (lon:int64) -> m.Item(l)
//        List.iter (fun (w:Word)->   if  m.ContainsKey(w.ordered) then printfn "%A" (w.spelled,w.ordered,m.Item(w.ordered))) newwordlist
    
    garbage f
    

Main "bologna"
System.Console.ReadKey() |>ignore