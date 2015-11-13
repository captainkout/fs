
let pi = 3, fun n -> ((2*n-1)*(2*n-1), 6)
let nap = 2, fun n -> (max 1 (n-1), n)
let root2 = 1, fun n -> (1, 2)
 
let eval (i,f) k =
  let rec frac n =
    let a, b = f n
    float a / (float b +
      if n >= k then 0.0 else frac (n+1)) 
  float i + frac 1
 
Printf.printf "sqrt(2)\t= %.15f\n" (eval root2 1000);
Printf.printf "e\t= %.15f\n" (eval nap 1000);
Printf.printf "pi\t= %.15f\n" (eval pi 1000);