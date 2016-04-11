type Agent (hist : Map<string,int*int*int>) = 
    let r = System.Random()
    let mutable lChoices = hist
    member this.choices 
        with get() = lChoices
        and set(newMap) =
            do lChoices <- newMap
    member this.expecation
        with get() = 
            let (t,w,l) = Map.fold (fun (t0,w0,l0) key (t1,w1,l1) -> (t0+t1,w0+w1,l0+l1)) (0,0,0) lChoices
            (float (w-l))/(float (t))
    member this.update(action,(win,loss)) =
        let (t,w,l) = lChoices.Item(action)
        do lChoices <- Map.add(action) (t+1,w+win,l+loss) lChoices
    member this.print = 
        printfn "%A" lChoices
    new() = Agent((Map.ofList [("rock",(1,0,0));
                                ("paper",(1,0,0));
                                ("scissor",(1,0,0))]))
    member this.getBest = 
        let l = [("rock",lChoices.Item("rock"));("paper", lChoices.Item("paper"));("scissor", lChoices.Item("scissor"))] 
        let lmax = List.fold (fun state (action,(t,w,l))->                            
                                if state < float((w-l))/(float t) then 
                                    float((w-l))/(float t)
                                else state)
                                -5.0 l
        let lFilt = List.filter (fun (action,(t,w,l)) -> float((w-l))/(float t) = lmax ) l
        fst (List.item (r.Next(0,lFilt.Length)) lFilt)
        

let rec contest (agentA:Agent) (agentB:Agent) =
    match agentA.getBest, agentB.getBest with
    |a,b when (a="rock" && b="scissor") || (a="paper" && b="rock") || (a="scissor" && b="paper") ->
        do agentA.update(a,(1,0))
        do agentB.update(b,(0,1))
        do printfn "%A" (agentA.expecation , agentB.expecation)
    |a,b when (b="rock" && a="scissor") || (b="paper" && a="rock") || (b="scissor" && a="paper") ->
        do agentA.update(a,(0,1))
        do agentB.update(b,(1,0))
        do printfn "%A" (agentA.expecation , agentB.expecation)
    |a,b ->
        do agentA.update(a,(0,0))
        do agentB.update(b,(0,0))
        contest agentA agentB


let one = new Agent()
let two = new Agent()

one.update("rock",(1,0))
two.update("scissor",(1,0))

one.getBest  |> printfn "%A"
two.getBest  |> printfn "%A"

for a in 0..25 do 
    contest one two

one.choices      |> printfn "%A"
two.choices      |> printfn "%A"

