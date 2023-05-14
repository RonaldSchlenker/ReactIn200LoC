type Nat0Factory() =
    let mutable value = -1
    member _.Next() =
        do value <- value + 1
        value

let nat0Factory () =
    let mutable value = -1
    fun () ->
        do value <- value + 1
        value

let nat = Nat0Factory()
let n0 = nat.Next()
let n1 = nat.Next()
let n2 = nat.Next()

let nextNat = nat0Factory ()
let n0' = nextNat ()
let n1' = nextNat ()
let n2' = nextNat ()
