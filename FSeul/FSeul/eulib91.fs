module eulib91
    open System

    let rec scale one two n L =
        match one, two with
        |(x1,y1),(x2,y2) when x1*n<=50 && y1*n<=50 && x2*n<=50 && y2*n<=50 ->
            scale (x1,y1) (x2,y2) (n+1) ((x1*n,y1*n,x2*n,y2*n)::L)
        |(_,_),(_,_) ->L

    let rec main one two L =
        //let l2 = [] 
        //if (x1,y1) = (y2,x2) then its a corner so no flip
        match one, two with
        |(x1,y1),(x2,y2) when x1=0 && y2=0 -> (* then its at origin, no flip required*)
            let l1 = scale (x1,y1) (x2,y2) 1 ((x1,y1,x2,y2)::L)
            (*scale 3 other rotations*)
            let l2 = scale (x1,y1) (x2,y1) 1 ((x1,y1,y2,x2)::l1)
            let l3 = scale (x2,y1) (x2,y2) 1 ((x2,y1,x2,y2)::l2)
            scale (x1,y1) (x2,y2) 1 ((x1,y1,x2,y2)::l3)
        |(_,_),(_,_) -> L