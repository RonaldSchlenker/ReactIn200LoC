
(*
    Exercise
    ---
      - count from 0 by 1
      - count from 100 by 10
      - output the sum of both counters
*)



let counter (initial: int) increment =
    let mutable current = initial - increment
    fun () ->
        do current <- current + increment
        current






















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

