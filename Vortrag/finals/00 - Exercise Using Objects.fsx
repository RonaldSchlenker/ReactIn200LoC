
let counter (initial: int) increment =
    let mutable current = initial - increment
    fun () ->
        do current <- current + increment
        current

let counterFrom0By1Inst = counter 0 1
counterFrom0By1Inst()

let counterFrom100By10Inst = counter 100 10
counterFrom100By10Inst()


let crazyCounter () =
    let counter1 = counter 0 1
    let counter2 = counter 100 10
    fun () ->
        let c1 = counter1()
        let c2 = counter2()
        c1 + c2
    
let crazyCounterInst = crazyCounter()
crazyCounterInst()


let toSeq mkGen = seq {
    let inst = mkGen ()
    while true do
        yield inst ()
}

let resultingSeq = crazyCounter |> toSeq


(*
    Positive:
        - clear API
        - composable
        - predictable

    Negative
        - Error prone (multiple evaluations possible)
        - Destroys "play instinct"

    -> PPT: Siehe Blockschaltbild
*)








let crazyCounterLikeItShouldBe () =
    let c1 = counter 0 1
    let c2 = counter 100 10
    c1 + c2
  

