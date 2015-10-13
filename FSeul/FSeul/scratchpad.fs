module scratchpad
    open System

//        let s = (helper.get_web_txt "https://projecteuler.net/project/resources/p096_sudoku.txt").Split [|'\n'|]
//        let s2 = [for a in s do
//                    if a.Contains("Grid")=false then 
//                        for achar in a do
//                        yield ((int achar)-48)]
//        let s3 = [for a in 1..9 do
//                    for b in 1..9 do
//                        yield (a,b)]
//        let rec loop i d= 
//            if i<=80 then loop (i+1) (Map.add s3.[i] s2.[i] d)
//            else d
//        loop 0 Map.empty