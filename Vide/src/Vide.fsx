module Vide

type IEvaluationManager =
    abstract member RequestEvaluation: unit -> unit
    abstract member Suspend: unit -> unit
    abstract member Resume: unit -> unit
    abstract member IsEvaluating: bool
    abstract member HasPendingEvaluationRequests: bool
    abstract member EvaluationCount: uint64

type GlobalContext = { evaluationManager: IEvaluationManager }

// why we return 's option(!!) -> Because of else branch / zero
// -> TODO: Is that still valid, now that we explicitly use preserve/discard?
type Vide<'v,'s,'c> = 's option -> GlobalContext -> 'c -> 'v * 's option

let ensureVide f : Vide<_,_,_> = f

[<AutoOpen>]
module MutableValue =
    type MutableValue<'a when 'a: equality>(initial: 'a, evalManager: IEvaluationManager) =
        let mutable state = initial
        member _.Set(value) =
            // Not just a perf opt: prevent stack overflows (see demo case asyncHelloWorld)!
            if value <> state then
                do state <- value
                do evalManager.RequestEvaluation()
        member this.Reset() = this.Set(initial)
        member inline this.Update(op, value) =
            this.Value <- op this.Value value
        member this.Value
            with get() = state
            and set(value) = this.Set(value)

type VideApp<'v,'s,'c>(content: Vide<'v,'s,'c>, ctxCtor: unit -> 'c) =
    let mutable currValue = None
    let mutable currentState = None
    let mutable isEvaluating = false
    let mutable hasPendingEvaluationRequests = false
    let mutable evaluationCount = 0uL
    let mutable suspendEvaluation = false
    let mutable onEvaluated: (VideApp<'v,'s,'c> -> 'v -> 's option -> unit) option = None
        
    let ctx = ctxCtor()

    interface IEvaluationManager with
        member this.RequestEvaluation() =
            if suspendEvaluation then
                hasPendingEvaluationRequests <- true
            else
                // During an evaluation, requests for another evaluation can
                // occur, which have to be handled as _subsequent_ evaluations!
                let rec eval () =
                    do 
                        hasPendingEvaluationRequests <- false
                        isEvaluating <- true
                    let value,newState = 
                        let gc = { evaluationManager = this.EvaluationManager } 
                        content currentState gc this.RootContext
                    do
                        currValue <- Some value
                        currentState <- newState
                        isEvaluating <- false
                        evaluationCount <- evaluationCount + 1uL
                    do onEvaluated |> Option.iter (fun x -> x this value currentState)
                    if hasPendingEvaluationRequests then
                        eval ()
                do
                    match isEvaluating with
                    | true -> hasPendingEvaluationRequests <- true
                    | false -> eval ()
        member _.Suspend() =
            do suspendEvaluation <- true
        member this.Resume() =
            do suspendEvaluation <- false
            if hasPendingEvaluationRequests then
                (this :> IEvaluationManager).RequestEvaluation()
        member _.IsEvaluating = isEvaluating
        member _.HasPendingEvaluationRequests = hasPendingEvaluationRequests
        member _.EvaluationCount = evaluationCount

    member _.RootContext = ctx
    member this.EvaluationManager = this :> IEvaluationManager
    member _.CurrentState = currentState
    member _.OnEvaluated
        with get() = onEvaluated
        and set(value) = onEvaluated <- value

module VideApp =
    let create content ctxCtor = VideApp(content, ctxCtor)

    let createWithUntypedState (content: Vide<_,_,_>) ctxCtor =
        let content =
            ensureVide <| fun (s: obj option) gc ctx ->
                let typedS = s |> Option.map (fun s -> s :?> 's)
                let v,s = content typedS gc ctx
                let untypedS = s |> Option.map (fun s -> s :> obj)
                v,untypedS
        create content ctxCtor

    let createAndStart content ctxCtor =
        let app = VideApp(content, ctxCtor)
        do app.EvaluationManager.RequestEvaluation()
        app

    let createAndStartWithUntypedState (content: Vide<_,_,_>) ctxCtor =
        let content =
            ensureVide <| fun (s: obj option) gc ctx ->
                let typedS = s |> Option.map (fun s -> s :?> 's)
                let v,s = content typedS gc ctx
                let untypedS = s |> Option.map (fun s -> s :> obj)
                v,untypedS
        createAndStart content ctxCtor

module Vide =

    // Preserves the first value given and discards subsequent values.
    let preserveValue x =
        ensureVide <| fun s gc ctx ->
            let s = s |> Option.defaultValue x
            s, Some s
    
    let preserveWith x =
        ensureVide <| fun s gc ctx ->
            let s = s |> Option.defaultWith x
            s, Some s
    
    // TODO: Think about which function is "global" and module-bound
    let map (proj: 'v1 -> 'v2) (v: Vide<'v1,'s,'c>) : Vide<'v2,'s,'c> =
        fun s gc ctx ->
            let v,s = v s gc ctx
            proj v, s
    
    // why 's and not unit? -> see comment in "VideBuilder.Zero"
    [<GeneralizableValue>]
    let zero<'c> : Vide<unit,unit,'c> =
        fun s gc ctx -> (),None

    // this "zero", but the form where state is variable
    // -> see comment in "VideBuilder.Zero"
    [<GeneralizableValue>]
    let empty<'s,'c> : Vide<unit,'s,'c> =
        fun s gc ctx -> (),None

    [<GeneralizableValue>]
    let context<'c> : Vide<'c,unit,'c> =
        fun s gc ctx -> ctx,None

    // TODO: Move to keywords? / rename to useState?
    let ofMutable x =
        ensureVide <| fun s gc ctx ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x, gc.evaluationManager))
            s, Some s

module BuilderBricks =
    let bind<'v1,'s1,'v2,'s2,'c>
        (
            m: Vide<'v1,'s1,'c>,
            f: 'v1 -> Vide<'v2,'s2,'c>
        ) 
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        fun s gc ctx ->
            let ms,fs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let mv,ms = m ms gc ctx
            let f = f mv
            let fv,fs = f fs gc ctx
            fv, Some (ms,fs)

    let return'<'v,'c>
        (x: 'v)
        : Vide<'v,unit,'c> 
        =
        fun s gc ctx -> x,None

    let yield'<'v,'s,'c>
        (v: Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        v

    // This zero (with "unit" as state) is required for multiple returns.
    // Another zero (with 's as state) is required for "if"s without an "else".
    // We cannot have both, which means: We cannot have "if"s without "else".
    // This is ok (and not unfortunate), because the developer has to make a
    // decision about what should happen: "elseForget" or "elsePreserve".
    let zero<'c>
        ()
        : Vide<unit,unit,'c>
        = Vide.zero<'c>

    let delay<'v,'s,'c>
        (f: unit -> Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        f()

    let combine<'v1,'s1,'v2,'s2,'c>
        (
           a: Vide<'v1,'s1,'c>,
           b: Vide<'v2,'s2,'c>
        )
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        fun s gc ctx ->
            let sa,sb =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let va,sa = a sa gc ctx
            let vb,sb = b sb gc ctx
            vb, Some (sa,sb)

    let for'<'a,'v,'s,'c when 'a: comparison>
        (
            input: seq<'a>,
            body: 'a -> Vide<'v,'s,'c>
        ) 
        : Vide<'v list, Map<'a, 's option>,'c>
        = 
        fun s gc ctx ->
            let mutable currMap = s |> Option.defaultValue Map.empty
            let resValues,resStates =
                [ for x in input do
                    let matchingState = currMap |> Map.tryFind x |> Option.flatten
                    let v,s = 
                        let v = body x
                        v matchingState gc ctx
                    do currMap <- currMap |> Map.remove x
                    v, (x,s)
                ]
                |> List.unzip
            resValues, Some (resStates |> Map.ofList)

// TODO:
//    For value restriction and other resolution issues, it's better to
//    move these (remaining) builder methods "as close as possible" to the builder class that's
//    most specialized.
type VideBaseBuilder() =
    member _.Bind(m, f) = BuilderBricks.bind(m, f)
    member _.Zero() = BuilderBricks.zero()







open System.Runtime.CompilerServices

type TextNodeProxy<'n> =
    {
        node: 'n
        getText: unit -> string
        setText: string -> unit
    }

/// This must be a stateless implementation that abstracts
/// commonly used node functions. Note that from the
/// point of view of this interface, parent and child are
/// both 'of type 'n (which is the common node type).

type INodeDocument<'n> =
    abstract member EnsureChildAppended : parent: 'n * child: 'n -> unit
    abstract member RemoveChild : parent: 'n * child: 'n -> unit
    abstract member GetChildren : parent: 'n -> 'n list
    abstract member ClearChildren : parent: 'n -> unit
    // This seems to be so common and useful for all type of backends
    // that we will leave it here (for now)
    abstract member CreateTextNode : text: string -> TextNodeProxy<'n>

// We have (abstract NodeContext<'n> + inheritors & INodeDocument<'n>)
// instead of (sealed NodeContext<'n,'d> & 'd :> INodeDocument<'n>)
// because having 'd would make many things very complicated and
// it would require having a node document with an 'e - which would
// prevent having a concrete and completely specialized 'c (see comments below).

[<AbstractClass>] 
type NodeContext<'n when 'n: equality>
    (
        parent: 'n, 
        document: INodeDocument<'n>
    ) =
    let mutable keptChildren = []
    member _.NodeDocument = document
    member _.ShowChild(child) =
        // What is important here:
        // The ordering is supposed to remain unchanged!
        // So we don't need a concept of "current index"
        do keptChildren <- child :: keptChildren
        do document.EnsureChildAppended(parent, child)
    member _.RemoveObsoleteChildren() =
        let childrenForRemoval = document.GetChildren(parent) |> List.except keptChildren
        for child in childrenForRemoval do
            document.RemoveChild(parent, child)
    member _.ClearContent() =
        do document.ClearChildren(parent)

type NodeBuilderState<'e,'s> = option<'e> * option<'s>

type ChildAction = Keep | DiscardAndCreateNew

type NodeModifierContext<'e> =
    {
        node: 'e
        globalContext: GlobalContext
    }

type NodeModifier<'n> = NodeModifierContext<'n> -> unit

[<AbstractClass>]
type NodeBuilder<'e,'n,'c>
    (
        createContext: 'e -> 'c,
        createThisElement: 'c -> 'e,
        checkChildNode: 'n -> ChildAction
    ) =
    
    inherit VideBaseBuilder()

    member _.Delay(f) = BuilderBricks.delay<_,_,'c>(f)

    member _.CreateContext = createContext
    member _.CreateThisElement = createThisElement
    member _.CheckChildNode = checkChildNode

    member val InitModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get
    member val EvalModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get
    member val AfterEvalModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get

module ModifierContext =
    // TODO: This is really, really weired. I think it's necessary to distinguish
    // between 'nthis and 'nhild on a general level (see branch of 2023-02-18 and many others)
    (*
        Generic Argument names:
        ---

        'e : A concrete element type, e.g. `HTMLButtonElement`
        'n : The abstract node type that is the basis for
             tree composition (e.g. `Node`)
        ...

        Notes
        ---
        
        In order to get rid of the unsafe cast, we need a constraint in form of ('e :> 'n),
        which is not possible. The interesting thing to see here is the type of "this":
            NodeBuilder<'e,'n,'c> = NodeBuilder<'e,'n,#NodeContext<'n>>
            
        Also, it is in general required that the builders have a concrete and completely
        specialized 'c (context) in Vide. This ensures smooth composition and overload
        resolution of the builder methods in the CEs, and it makes CE builder methods
        easy to implement (e.g.: see "Combine").

        Even if NodeBuilder has now 'e _and also_ 'n, we still have to use an unsafe cast from
        'e to 'n, but having also 'n as arg in NodeBuilder, we can have `checkChildNode`
        taking a 'n instead of an 'e (which otherwise would be wrong).
    *)
    let inline apply<'v1,'v2,'s,'e,'n,'c
            when 'n: equality
            and 'c :> NodeContext<'n>
        >
        (childVide: Vide<'v1,'s,'c>)
        (createResultVal: 'e -> 'v1 -> 'v2)
        (this: NodeBuilder<'e,'n,'c>)
        : Vide<'v2, NodeBuilderState<'e,'s>, 'c>
        =
        fun s gc (parentCtx: 'c) ->
            let inline runModifiers modifiers node =
                for m in modifiers do
                    m { node = node; globalContext = gc }
            let s,cs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let thisElement,cs =
                // Can it happen that s is Some and cs is None? I don't think so.
                // But: See comment in definition of: Vide.Core.Vide
                match s with
                | None ->
                    let newElement,s = this.CreateThisElement(parentCtx), cs
                    do runModifiers this.InitModifiers newElement
                    newElement,s
                | Some maybeThisElement ->
                    let elem = (box maybeThisElement) :?> 'n
                    match this.CheckChildNode(elem) with
                    | Keep ->
                        parentCtx.ShowChild(elem)
                        maybeThisElement,cs
                    | DiscardAndCreateNew ->
                        this.CreateThisElement(parentCtx), None
            do runModifiers this.EvalModifiers thisElement
            let childCtx =
                // TODO: Why the unsafe cast everywhere in this function?
                this.CreateContext thisElement
            let cv,cs = childVide cs gc childCtx
            do childCtx.RemoveObsoleteChildren()
            do runModifiers this.AfterEvalModifiers thisElement
            let result = createResultVal thisElement cv
            let state = Some (Some thisElement, cs)
            result,state

module BuilderBricks2 =
    let inline yieldVide(v: Vide<_,_,_>) =
        v
    
    let inline yieldText<'n,'c when 'c :> NodeContext<'n>>(value: string) =
        ensureVide <| fun s gc (ctx: 'c) ->
            let textNode =
                s |> Option.defaultWith (fun () ->
                    let textNode = ctx.NodeDocument.CreateTextNode(value)
                    do ctx.ShowChild(textNode.node)
                    textNode
                )
            do
                if textNode.getText() <> value then
                    textNode.setText(value)
                ctx.ShowChild(textNode.node)
            (), Some textNode

// ---------------------------------------------------------------------------------
// The four (+1 base) basic builders for "vide { .. }" and renderers
// Used for HTML elements like
//    div, p, etc.in their forms (with content, with returns, specific
//    result value like for "input"), and the vide component builder.
// ---------------------------------------------------------------------------------

// ---------------------------------------------------------------------------------
// Builder definitions
//   + Run
//   + Return
//     - every Content builder should bind every other builder)
//     - standard yields
//   + Combine,For,Delay
// -------
// Note on 
//     "Pot" (has potential return value) and 
//     "Val" (has return value):
//     -> Transformation by "emitValue" member of Pot builders imply that
//          - derived classes have no additional state at all and
//          - "emitValue" is not part of the fluent API and shall be called
//            as the last element in the chain (from a user's perspective).
// ---------------------------------------------------------------------------------

type ComponentRetCnBaseBuilder<'n,'c
        when 'n : equality
        and 'c :> NodeContext<'n> 
    > () =
    inherit VideBaseBuilder()
    member _.Return(x: 'v) = BuilderBricks.return'<'v,'c>(x)
    member _.Delay(f) = BuilderBricks.delay<_,_,'c>(f)
    member _.Combine(a, b) = BuilderBricks.combine<_,_,_,_,'c>(a, b)
    member _.For(seq, body) = BuilderBricks.for'<_,_,_,'c>(seq, body)

type RenderRetCnBaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, checkChildNode) 
    =
    inherit NodeBuilder<'e,'n,'c>(createContext, createThisElement, checkChildNode)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = this |> ModifierContext.apply v (fun n v -> v)
    member _.Return(x) = BuilderBricks.return'(x)


// -------------------------------------------------------------------
// "Yielsd"s 
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type ComponentRetCnBaseBuilder<'n,'c
        when 'n : equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,'c>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,'c>) = b {()}
    member _.Yield(v) = BuilderBricks2.yieldVide(v)
    member _.Yield(op) = BuilderBricks2.yieldText<'n,'c>(op)

type RenderRetCnBaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(v) = BuilderBricks2.yieldVide(v)
    member _.Yield(op) = BuilderBricks2.yieldText<'n,'c>(op)


    
// ----------------------------------------------------------------------------
// "Bind"s (every Content builder can bind every builder that returns values)
// ----------------------------------------------------------------------------

type RenderRetCnBaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

type ComponentRetCnBaseBuilder<'n,'c
        when 'c :> NodeContext<'n> 
        and 'n : equality
    > with
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

[<Extension>]
type NodeBuilderExtensions =
    
    /// Called once on initialization.
    [<Extension>]
    static member onInit(this: #NodeBuilder<_,_,_>, m: NodeModifier<_>) =
        do this.InitModifiers.Add(m)
        this
    
    /// Called on every Vide evaluatiopn cycle.
    [<Extension>]
    static member onEval(this: #NodeBuilder<_,_,_>, m: NodeModifier<_>) =
        do this.EvalModifiers.Add(m)
        this
    
    /// Called after every Vide evaluatiopn cycle.
    [<Extension>]
    static member onAfterEval(this: #NodeBuilder<_,_,_>, m: NodeModifier<_>) =
        do this.AfterEvalModifiers.Add(m)
        this

module Event =
    type NodeEventArgs<'evt,'e> =
        {
            node: 'e
            evt: 'evt
            gc: GlobalContext
            mutable requestEvaluation: bool
        }
    
    let inline handle
        (node: 'e)
        (gc: GlobalContext)
        (callback: NodeEventArgs<'evt,'e> -> unit)
        =
        fun evt ->
            let args = { node = node; evt = evt; gc = gc; requestEvaluation = true }
            try
                do gc.evaluationManager.Suspend()
                do callback args
                if args.requestEvaluation then
                    gc.evaluationManager.RequestEvaluation()
            finally
                do gc.evaluationManager.Resume()



// TODO: Include "WpfisModel" again and move some MAUI things (i.e. AddElement) to WpfishContext

type IWebDocument<'n> =
    inherit INodeDocument<'n>
    abstract member CreateNodeOfName : tagName: string -> 'n

[<AbstractClass>] 
type WebContext<'n when 'n: equality>
    (
        parent: 'n, 
        document: IWebDocument<'n>
    ) =
    inherit NodeContext<'n>(parent, document)
    member _.WebDocument = document

module BuilderHelper =
    // TODO: This should not append - should be done in "apply"
    let createNode<'e,'n when 'n: equality> tagName (ctx: WebContext<'n>) =
        let n = ctx.WebDocument.CreateNodeOfName(tagName)
        do ctx.ShowChild(n)
        // TODO: Can we get rid of the unsafe cast?
        (box n) :?> 'e
    
    let checkNode (expectedNodeName: string) (actualNodeName: string) =
        // WebSharper has no Equals(.., StrComp) available, so we use this
        // which is enough for HTML element tags.
        match actualNodeName.ToUpper() = expectedNodeName.ToUpper() with
        | true -> Keep
        | false ->
            // TODO: if/else detection? Expected node name: {expectedNodeName}, but was: {actualNodeName}"
            DiscardAndCreateNew
