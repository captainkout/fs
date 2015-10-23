let typ x = 
    let nx = box x
    match nx with
    | :? int -> "int"
    | _ -> "other"

typ 100
