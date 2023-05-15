
type Block<'output, 'state> = option<'state> -> ('output * 'state)

type BlockBuilder() =
    member this.Bind(m: Block<'o1, 's1>, f: 'o1 -> Block<'o2, 's2>) : Block<_,_> =
        fun maybeState ->
            // wie kommen wir von option<('s1 * 's2)> nach (option<'s1> * option<'s2>) ?
            let mPrevState,fPrevState =
                match maybeState with
                | None -> None,None
                | Some (s1, s2) -> Some s1, Some s2
            
            let (mout,mNewState) = m mPrevState

            let fBlock = f mout
            let (fout,fNewState) = fBlock fPrevState

            let overallNewState = mNewState,fNewState
            fout,overallNewState
    member this.Return(x: 'a) : Block<_,_> = fun _ -> x,()
let block = BlockBuilder()

let counter (initial: int) increment : Block<int,_> =
    fun maybeState ->
        let current = maybeState |> Option.defaultValue initial
        let value = current + increment
        value,value

let delayBy1 defaultVal currVal =
    fun maybeState ->
        let state = maybeState |> Option.defaultValue defaultVal
        state,currVal

let makeAdd2CounterLikeItShouldBe =
    block {
        let! c1 = counter 0 1
        let! c2 = counter 100 10
        let! delayedC2 = delayBy1 0 c2
        return float (c1 + delayedC2)
    }

/// f_n = f_n1 + f_n2    | n >= 3
/// f_2 = f_2 = 1        | Anfangswerte
let fib =
    fun maybeState ->
        let n,x1,x2 = maybeState |> Option.defaultValue (1, 1, 1)
        let x = if n < 3 then 1 else x1 + x2
        x, (n+1, x, x1)


let o1,s1 = makeAdd2CounterLikeItShouldBe None
let o2,s2 = makeAdd2CounterLikeItShouldBe (Some s1)


let blockToIEnumerable (block: Block<'o,'s>) =
    let mutable currentState = None
    seq {
        while true do
            let value,newState = block currentState
            do currentState <- Some newState
            yield value
    }

let myStream =
    block {
        let! c1 = counter 0 1
        let! c2 = counter 100 10
        return float (c1 + c2)
    }
    |> blockToIEnumerable

myStream |> Seq.take 5

fib |> blockToIEnumerable |> Seq.take 10 |> Seq.toList

block {
    let! c = counter 0 1
    let! d = delayBy1 0 c
    return d
}
|> blockToIEnumerable |> Seq.take 10 |> Seq.toList



//let makeAdd2CounterLikeItShouldBe : Block<_,int> =
//    (makeCounter 0 1) |> bind (fun c1 ->
//    (makeCounter 100 10) |> bind (fun c2 ->
//    retBlock (c1 + c2)
//    )
//    )


//let makeAdd2CounterLikeItShouldBe : Block<_,int> =
//    let! (c1: int) = (makeCounter 0 1) : Block<int,int>
//    let! (c2: int) = (makeCounter 100 10) : Block<int,int>
//    return c1 + c2
  
