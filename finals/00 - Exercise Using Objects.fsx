
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



// let a = 10 in
//     a + 20

// let plusOne f value = f (value + 1)
// 10 |> plusOne (fun a -> a + 20)


let crazyCounterLikeItShouldBe () : StateAware<int> =
    let c1 : int = (counter 0 1) : StateAware<int>
    let c2 : int = (counter 100 10) : StateAware<int>
    (c1 + c2) : int
  


type StateAware<'value, 'state> = 'state option -> 'value * 'state

let bind (f: 'v1 -> StateAware<'v2,'s2>) (m: StateAware<'v1,'s1>) : StateAware<'v2, 's1 * 's2> = 
    fun maybeState ->
        let mMaybeState,fMaybeState =
            match maybeState with
            | None -> None,None
            | Some (ms,fs) -> Some ms, Some fs
        let mValue,mNewState = m mMaybeState
        let fValue,fNewState =
            let fStateAware = f mValue
            fStateAware fMaybeState
        fValue,(mNewState,fNewState)

let retBlock (x: 'a) : StateAware<'a,unit> =
    fun _ -> x,()

type StateAwareBuilder() =
    member _.Bind(m, f) = bind f m
    member _.Return(x) = retBlock x
let stateAware = StateAwareBuilder()

let counter (initial: int) increment : StateAware<int,_> =
    fun maybeState ->
        let current = (maybeState |> Option.defaultValue initial)
        let newState = current + increment
        current,newState

let crazyCounterLikeItShouldBe () : StateAware<int,_> =
    (counter 0 1) |> bind (fun c1 ->
    (counter 100 10) |> bind (fun c2 ->
    retBlock (c1 + c2)
    ))

let crazyCounterGoal =
    stateAware {
        let! c1 = counter 0 1
        let! c2 = counter 100 10
        return c1 + c2
    }

let v1,s1 = crazyCounterGoal None
let v2,s2 = crazyCounterGoal (Some s1)
let v3,s3 = crazyCounterGoal (Some s2)

let toSeq (saf: StateAware<_,_>) =
    seq {
        let mutable currState = None
        while true do
            let v,newState = saf currState
            do currState <- Some newState
            yield v
    }

crazyCounterGoal |> toSeq |> Seq.take 5 |> Seq.toList
