
let from0by1 = Seq.initInfinite id
let from100by10 = Seq.initInfinite id |> Seq.map (fun x -> 100 + (x * 10))
let res = from0by1 |> Seq.zip from100by10 |> Seq.map (fun (c1,c2) -> c1 + c2)
