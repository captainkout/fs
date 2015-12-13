//Lexigraphic permutations
let builder i =
  let recurse l i=
    if l =[] then
      [for a in 0..i do yield [a]] 
    else
      [for a in 0..i do 
        yield (List.map (fun l1 -> a::l1) l)]
        |> List.fold (fun  acc l2 -> l2@acc) (List.empty<int list>)
  let rec loop cur last l =
    if cur<last then loop (cur+1) last (recurse l cur)
    else l
  loop 0 i []
let mapper l =
  let rec loop where split acc=
    match where with
    |[] ->acc
    |hm::tm ->
      let a,b = List.splitAt hm split
      loop tm (a@(List.tail b)) ((List.head b)::acc)
  loop l [1..(List.length l)] []

#time "on"
builder 3 
  |> List.map (fun l -> mapper l)
  |> Set.ofList


