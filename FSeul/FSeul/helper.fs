module helper 

    let rec gcd a b = 
     match (a % b) with
       |0 -> b
       |r -> gcd b r
