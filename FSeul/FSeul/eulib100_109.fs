module eulib100_109
    let hundred start = 
        let rec loop b n max =
            if n<max then
                loop (3L*b + 2L*n - 2L) (4L*b + 3L*n - 3L) max
            else b,n
        loop 15L 21L (pown 10L 12)
    let hundredone start =
        let m = [for r in 1.0..11.0 do
                   yield [for a in 0.0..11.0 do yield r**a]]
        let rec fip acc x =
            let solve xbx =
                let A = MathNet.Numerics.LinearAlgebra.MatrixExtensions.matrix (m |> helper.listSlice [0..(xbx-1)] |> List.map (helper.listSlice [0..(xbx-1)]))
                let v =[ for n in [1.0..11.0] do
                            yield (1.- n + n **2. - n**3. + n**4. - n**5. + n**6. - n**7. + n**8. - n**9. + n**10.)]
                            //yield (n**3.0)]
                let b = MathNet.Numerics.LinearAlgebra.VectorExtensions.vector (v |> helper.listSlice [0..(xbx-1)])
                A.Solve(b).ToArray() 
                            |> List.ofArray 
                            |> List.zip (helper.listSlice [0..(xbx-1)] m.[(xbx)]) 
                            |> List.map (fun (a,b)->(decimal b)*(decimal a))
                            |> List.sum
            if x<10 then 
                fip (acc+(solve x)) (x+1)
            else acc+(solve x)
        fip 0M 1