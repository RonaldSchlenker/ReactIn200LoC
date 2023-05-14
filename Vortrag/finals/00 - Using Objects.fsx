
let counter (initial: int) increment =
    let mutable current = initial
    fun () ->
        do current <- current + increment
        current

let counterFrom0By1 = counter 0 1
counterFrom0By1()

let counterFrom100By10 = counter 100 10
counterFrom100By10()


let makeAdd2Counter () =
    let counter1 = counter 0 1
    let counter2 = counter 100 10
    fun () ->
        let c1 = counter1()
        let c2 = counter2()
        c1 + c2
    
let special = makeAdd2Counter()

special()



let makeAdd2CounterLikeItShouldBe () =
    let c1 = counter 0 1
    let c2 = counter 100 10
    c1 + c2
  
let special2 = makeAdd2CounterLikeItShouldBe()
special2()