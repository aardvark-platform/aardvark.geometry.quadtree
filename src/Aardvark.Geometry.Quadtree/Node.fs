namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System
open System.Collections.Generic
open System.Threading
open System.Collections.Immutable

(*
    Node.
*)

/// Which data dominates in merge operations.
type Dominance = 
    | FirstDominates 
    | SecondDominates 
    | MoreDetailedOrFirst
    | MoreDetailedOrSecond
with
    member this.Flipped with get() =
        match this with 
        | FirstDominates       -> SecondDominates
        | SecondDominates      -> FirstDominates
        | MoreDetailedOrFirst  -> MoreDetailedOrSecond
        | MoreDetailedOrSecond -> MoreDetailedOrFirst

type SerializationOptions = {
    Save : Guid -> byte[] -> unit
    TryLoad : Guid -> byte[] option
    Exists : Guid -> bool
    }
with

    static member Default = {
        Save    = fun id buffer -> failwith "No store defined. Invariant b6a5c282-55c9-4a1d-8672-c600b82d3969."
        TryLoad = fun id        -> failwith "No store defined. Invariant fd1748c0-6eff-4e08-822a-3d708e54e393."
        Exists  = fun id        -> failwith "No store defined. Invariant 0f9c8cfd-21dd-4973-b88d-97629a5d2804."
        }

    (* in-memory store (for testing purposes *)

    static member private inMemoryStoreCount : int = 0

    static member NewInMemoryStore (verbose : bool) =
        let store = Dictionary<Guid, byte[]>()
        let storeNumber = Interlocked.Increment(ref SerializationOptions.inMemoryStoreCount)
        {
            Save = fun id buffer ->
                store.[id] <- buffer
                if verbose then printfn "[InMemoryStore %d] SAVE %A <- %d bytes" storeNumber id buffer.Length
            TryLoad = fun id ->
                match store.TryGetValue(id) with
                | (false, _) ->
                    if verbose then printfn "[InMemoryStore %d] TRYLOAD %A -> None" storeNumber id
                    None
                | (true, buffer) ->
                    if verbose then printfn "[InMemoryStore %d] TRYLOAD %A -> Some %d bytes" storeNumber id buffer.Length
                    Some buffer
            Exists = fun id ->
                let result = store.ContainsKey(id)
                if verbose then printfn "[InMemoryStore %d] EXISTS %A -> %A" storeNumber id result
                result
        }

    static member NewInMemoryStore () =
        SerializationOptions.NewInMemoryStore(verbose = false)

    (* Uncodium.SimpleStore *)

    static member SimpleDiskStore (path : string) =
        let store = new Uncodium.SimpleStore.SimpleDiskStore(path)
        {
            Save    = fun id buffer -> store.Add(id.ToString(), buffer, fun () -> buffer)
            TryLoad = fun id        -> match store.Get(id.ToString()) with | null -> None | buffer -> Some buffer
            Exists  = fun id        -> store.Contains(id.ToString())
        }

    static member SimpleFolderStore (path : string) =
        let store = new Uncodium.SimpleStore.SimpleFolderStore(path)
        {
            Save    = fun id buffer -> store.Add(id.ToString(), buffer, fun () -> buffer)
            TryLoad = fun id        -> match store.Get(id.ToString()) with | null -> None | buffer -> Some buffer
            Exists  = fun id        -> store.Contains(id.ToString())
        }

/// Leaf node with original data.
and QNode(uid : Guid, exactBoundingBox : Box2d, cell : Cell2d, splitLimitExp : int, layers : LayerSet) =

    do
        invariant (exactBoundingBox.IsValid) "661f5dfa-2706-4935-ae4e-1d25f02224ae"

        invariantm (cell.Exponent - splitLimitExp = layers.SampleExponent) 
            "Sample exponent does not match split limit and node cell."
            "1ec76eaa-534b-4339-b63b-8e0399562bb1"

        let w = layers.SampleWindow
        let e = layers.SampleExponent
        let bb = cell.BoundingBox
        for layer in layers.Layers do
            invariantm (layer.SampleExponent = e) "Layers exponent mismatch."   "7adc422c-effc-4a86-b493-ff1cd0f9e991"
            invariantm (layer.SampleWindow = w)   "Layers window mismatch."     "74a57d1d-6a7f-4f9e-b26c-41a1e79cb989"
            invariantm (bb.Contains(layer.Mapping.BoundingBox)) 
                (sprintf "Layer %A is outside node bounds." layer.Def.Id)       "dbe069cc-df5c-42c1-bb58-59c5a061ee15"
            

    new (exactBoundingBox : Box2d, cell : Cell2d, splitLimitExp : int, layers : LayerSet) =
        QNode(Guid.NewGuid(), exactBoundingBox, cell, splitLimitExp, layers)

    member _.Id with get() = uid

    member _.Cell with get() = cell

    member _.ExactBoundingBox with get() = exactBoundingBox

    /// The maximum tile size, given as width = height = 2^SplitLimitExponent.
    member _.SplitLimitExponent with get() = splitLimitExp

    /// Replace all occurences of 'oldSemantic' with 'newSemantic'.
    /// Returns 'Some <newUpdatedOctree>' if 'oldSemantic' exists and is replaced.
    /// Returns 'None' if 'oldSemantic' does not exist.
    /// Throws if quadtree contains both 'oldSemantic' and 'newSemantic'.
    member this.UpdateLayerSemantic (oldSemantic : Durable.Def, newSemantic : Durable.Def) : QNode option =
        match layers.UpdateLayerSemantic(oldSemantic, newSemantic) with
        | None -> None
        | Some newLayers -> QNode(exactBoundingBox, cell, splitLimitExp, newLayers) |> Some
        
    member this.Save options : Guid =
        let map = List<KeyValuePair<Durable.Def, obj>>()

        // node properties
        map.Add(kvp Defs.NodeId uid)
        map.Add(kvp Defs.CellBounds cell)
        map.Add(kvp Defs.SplitLimitExponent splitLimitExp)
        map.Add(kvp Defs.ExactBoundingBox this.ExactBoundingBox)
                
        // layers
        for layer in layers.Layers do
            let layerDef = Defs.GetLayerFromDef layer.Def
            let dm = layer.Materialize().ToDurableMap ()
            map.Add(kvp layerDef dm)

        // children
        //match subNodes with
        //| Some xs ->

        //    // recursively store subnodes (where necessary)
        //    for x in xs do 
        //        match x with 
        //        | InMemoryNode n    -> if not (options.Exists n.Id) then n.Save options |> ignore
        //        | NoNode            -> () // nothing to store
        //        | OutOfCoreNode _   -> () // already stored
        //        | InMemoryInner n       -> failwith "Implement InnerNode.Save. Todo 2f9dc484-2c48-4b8e-9b4f-a23d108f7279."

        //    // collect subnode IDs 
        //    let ids = xs |> Array.map (fun x -> 
        //        match x with 
        //        | NoNode -> Guid.Empty
        //        | InMemoryNode n -> n.Id
        //        | OutOfCoreNode (id, _) -> id
        //        | InMemoryInner n -> n.Id
        //        )
        //    map.Add(kvp Defs.SubnodeIds ids)
        //| None -> ()

        let buffer = DurableCodec.Serialize(Defs.Node, map)
        options.Save uid buffer
        uid

    member _.LayerSet with get() = layers

    member this.ContainsLayer (semantic : Durable.Def) : bool =
        this.TryGetLayer(semantic) |> Option.isSome

    member this.TryGetLayer (semantic : Durable.Def) : ILayer option =
        layers.TryGetLayer(semantic)

    member this.TryGetLayer<'a> (semantic : Durable.Def) : Layer<'a> option =
        layers.TryGetLayer<'a>(semantic)

    member this.GetLayer(semantic : Durable.Def) : ILayer =
        layers.GetLayer(semantic)

    member this.GetLayer<'a>(semantic : Durable.Def) : Layer<'a> =
        layers.GetLayer<'a>(semantic)

    member this.WithWindow (w : Box2l) : QNode option =
        match layers.WithWindow(w) with
        | None -> None
        | Some windowedLayers ->
            let ebb = windowedLayers.BoundingBox
            let n = QNode(ebb, cell, splitLimitExp, windowedLayers)
            Some n

    member this.WithLayers (newLayers : LayerSet) : QNode =
        QNode(exactBoundingBox, cell, splitLimitExp, newLayers)

    member this.GetAllSamples () : Cell2d[] =
        layers.SampleWindow.GetAllSamples(layers.SampleExponent)

    member this.GetAllSamplesInsideWindow (window : Box2l) : Cell2d[] =
        invariant (layers.SampleWindow.Contains(window)) "f244101f-975c-4a0a-8c03-20e0618834b4"
        window.GetAllSamples(layers.SampleExponent)

    member this.GetAllSamplesFromFirstMinusSecond (first : Box2l, second : Box2l) : Cell2d[] =
        first.GetAllSamplesFromFirstMinusSecond(second, layers.SampleExponent)

    member this.GetSample (p : V2d) : Cell2d =
        layers.Mapping.GetSampleCell(p)

   
    

    static member Load (options: SerializationOptions) (id : Guid) : QNodeRef =
        if id = Guid.Empty then
            NoNode
        else
            match options.TryLoad id with
            | None -> NoNode
            | Some buffer ->
                let struct (def, o) = DurableCodec.Deserialize(buffer)
                if def = Defs.Node then
                    let map = o :?> ImmutableDictionary<Durable.Def, obj>
                    let id                  = map.Get(Defs.NodeId)                  :?> Guid
                    let cell                = map.Get(Defs.CellBounds)              :?> Cell2d
                    let splitLimitExp       = map.Get(Defs.SplitLimitExponent)      :?> int
                    let exactBoundingBox    = match map.TryGetValue(Defs.ExactBoundingBox) with
                                              | true,  x -> x :?> Box2d
                                              | false, _ -> Box2d.Invalid
                
                    let layers : ILayer[] =  
                        map 
                        |> Seq.choose (fun kv ->
                            match Defs.TryGetDefFromLayer kv.Key with
                            | Some def -> 
                                let m = kv.Value :?> ImmutableDictionary<Durable.Def, obj>
                                Layer.FromDurableMap def m |> Some
                            | None -> None
                            )
                        |> Seq.toArray

                    let layerSet = LayerSet(layers)

                    invariant (layers.Length > 0) "68ca6608-921c-4868-b5f2-3c6f6dc7ab57"

                    //let subNodes =
                    //    match map.TryGetValue(Defs.SubnodeIds) with
                    //    | (false, _)   -> None
                    //    | (true, o) ->
                    //        let keys = o :?> Guid[]
                    //        let xs = keys |> Array.map (fun k ->
                    //            if k = Guid.Empty then
                    //                NoNode
                    //            else
                    //                OutOfCoreNode (k, (fun () ->
                    //                    match QNode.Load options k with
                    //                    | InMemoryNode n        -> n
                    //                    | NoNode                -> failwith "Invariant de92e0d9-0dd2-4fc1-b4c7-0ace2910ce24."
                    //                    | OutOfCoreNode (_, _)  -> failwith "Invariant 59c84c71-9043-41d4-abc0-4e1f49b8e2ba."
                    //                    | InMemoryInner _           -> failwith "Invariant 91691cf0-d728-4814-994a-96707d95be1f."
                    //                    ))
                    //            )
                    //        Some xs

                    let n = QNode(id, exactBoundingBox, cell, splitLimitExp, layerSet)
                    InMemoryNode n
                else
                    failwith "Loading quadtree failed. Invalid data. f1c2fcc6-68d2-47f3-80ff-f62b691a7b2e."

and
    QInnerNode = {
        Id : Guid
        ExactBoundingBox : Box2d
        Cell : Cell2d
        SplitLimitExponent : int
        SubNodes : QNodeRef[]
        }

and
    QMergeNode = {
        Id : Guid
        ExactBoundingBox : Box2d
        Cell : Cell2d
        SplitLimitExponent : int
        Dominance : Dominance
        First : QNodeRef
        Second : QNodeRef
        }

and

    QNodeRef =
    | NoNode
    | InMemoryNode  of QNode
    | OutOfCoreNode of Guid * (unit -> QNode)
    | InMemoryInner of QInnerNode
    | InMemoryMerge of QMergeNode
    with

        /// Forces property Id. Throws exception if NoNode.
        member this.Id with get() =
            match this with
            | NoNode -> failwith "Id does not exist for NoNode. Error c48422a9-4df6-4d15-9dad-af075a1d52ac."
            | InMemoryNode n -> n.Id
            | InMemoryInner n -> n.Id
            | InMemoryMerge n -> n.Id
            | OutOfCoreNode (_, load) -> load().Id

        /// Forces property Cell. Throws exception if NoNode.
        member this.Cell with get() =
            match this with
            | NoNode -> failwith "Cell does not exist for NoNode. Error c48422a9-4df6-4d15-9dad-af075a1d52ac."
            | InMemoryNode n -> n.Cell
            | InMemoryInner n -> n.Cell
            | InMemoryMerge n -> n.Cell
            | OutOfCoreNode (_, load) -> load().Cell

         /// Returns node's exact bounding box, or invalid box for NoNode.
        member this.ExactBoundingBox with get() =
            match this with
            | NoNode -> Box2d.Invalid
            | InMemoryNode n -> n.ExactBoundingBox
            | InMemoryInner n -> n.ExactBoundingBox
            | InMemoryMerge n -> n.ExactBoundingBox
            | OutOfCoreNode (_, load) -> load().ExactBoundingBox

        /// Forces property SplitLimitExp. Throws exception if NoNode.
        member this.SplitLimitExponent with get() =
            match this with
            | NoNode -> failwith "SplitLimitExp does not exist for NoNode. Error 4424a37e-dcd8-4ae0-975c-7ae6d926aaa8."
            | InMemoryNode n -> n.SplitLimitExponent
            | InMemoryInner n -> n.SplitLimitExponent
            | InMemoryMerge n -> n.SplitLimitExponent
            | OutOfCoreNode (_, load) -> load().SplitLimitExponent

        member this.ContainsLayer (semantic : Durable.Def) =
            match this with
            | NoNode -> false
            | InMemoryInner _ -> false
            | InMemoryMerge _ -> false
            | InMemoryNode n -> n.ContainsLayer semantic
            | OutOfCoreNode (_, load) -> load().ContainsLayer semantic

        member this.LayerSet with get() : LayerSet option =
            match this with
            | NoNode -> None
            | InMemoryInner _ -> None
            | InMemoryMerge _ -> None
            | InMemoryNode n -> Some(n.LayerSet)
            | OutOfCoreNode (_, load) -> Some(load().LayerSet)

        member this.IsLeafNode with get() =
            match this with
            | NoNode                -> false
            | InMemoryInner _       -> false
            | InMemoryMerge _       -> false
            | InMemoryNode _        -> true
            | OutOfCoreNode (_, _)  -> true

        /// Replace all occurences of 'oldSemantic' with 'newSemantic'.
        /// Returns (true, <newUpdatedOctree>) if 'oldSemantic' exists and is replaced.
        /// Returns (false, 'qtree') if 'oldSemantic' does not exist.
        /// Throws if quadtree contains both 'oldSemantic' and 'newSemantic'.
        member this.UpdateLayerSemantic (oldSemantic : Durable.Def, newSemantic : Durable.Def) : bool * QNodeRef =
            
            let unchanged = (false, this)
            let qnode (n : QNode) =
                match n.UpdateLayerSemantic(oldSemantic, newSemantic) with
                | None   -> (false, this)
                | Some x -> (true,  InMemoryNode x)

            match this with
            | NoNode
            | InMemoryInner _
            | InMemoryMerge _           -> unchanged
            | InMemoryNode n            -> n |> qnode
            | OutOfCoreNode (_, load)   -> load() |> qnode

        /// Throws if no such layer.
        member this.GetLayer def : ILayer =
            match this with
            | NoNode
            | InMemoryInner _
            | InMemoryMerge _           -> sprintf "Layer not found. %A. Error 2c634f5f-d359-4523-b87a-a96d2522c018." def |> failwith
            | InMemoryNode n            -> n.LayerSet.GetLayer def
            | OutOfCoreNode (_, load)   -> load().LayerSet.GetLayer def

        /// Throws if no such layer.
        member this.GetLayer<'a> def = 
            match this with
            | NoNode
            | InMemoryInner _
            | InMemoryMerge _           -> sprintf "Layer not found. %A. Error f33f39a9-2394-473f-8dae-76bd8baaaafb." def |> failwith
            | InMemoryNode n            -> n.LayerSet.GetLayer<'a> def
            | OutOfCoreNode (_, load)   -> load().LayerSet.GetLayer<'a> def
    
        member this.TryGetLayer def =
            match this with
            | NoNode
            | InMemoryInner _ 
            | InMemoryMerge _           -> None
            | InMemoryNode n            -> n.LayerSet.TryGetLayer def
            | OutOfCoreNode (_, load)   -> load().LayerSet.TryGetLayer def

        member this.TryGetLayer<'a> def =
            match this with
            | NoNode
            | InMemoryInner _
            | InMemoryMerge _           -> None
            | InMemoryNode n            -> n.LayerSet.TryGetLayer<'a> def
            | OutOfCoreNode (_, load)   -> load().LayerSet.TryGetLayer<'a> def

module QNode =

    let toRef (n : QNode option) =
        match n with
        | Some n -> InMemoryNode n
        | None -> NoNode

module QInnerNode =

    let ofSubNode (n : QNodeRef) : QInnerNode =
        let rc = n.Cell.Parent
        match Option.ofNullable(rc.GetQuadrant(n.Cell)) with
        | None -> failwith "Invariant 3ada0081-039d-42c6-baad-94fbf26bf3ce."
        | Some qi ->
            let ns = Array.create 4 NoNode
            ns.[qi] <- n
            { Id = Guid.NewGuid(); ExactBoundingBox = n.ExactBoundingBox; Cell = rc; SplitLimitExponent = n.SplitLimitExponent; SubNodes = ns }

    let ofSubNodes (ns : QNodeRef[]) : QInnerNode =
        invariant (ns.Length = 4) "acf0afb0-5d73-4827-acca-e1707ec6db5a"
        // non-empty subnodes
        let ns' = ns |> Array.choose(fun n -> match n with | NoNode -> None | n -> Some n)
        // no duplicate subnodes allowed
        invariant ((ns' |> Seq.map(fun n -> n.Cell) |>  Seq.distinct |> Seq.length) = ns'.Length) "3242ee2c-c4bb-47aa-91ac-9627eaa98754"

        let isCenterChildren = ns' |> Array.forall(fun n -> n.Cell.TouchesOrigin)
        let rc = if isCenterChildren then Cell2d(ns'.[0].Cell.Exponent + 1)
                 else // all subnodes must be direct children of same root cell (rc)
                      ns' |> Seq.map(fun n -> n.Cell.Parent) |> Seq.distinct |> Seq.exactlyOne

        let ebb = ns' |> Seq.map(fun n -> n.ExactBoundingBox) |> Box2d

        { Id = Guid.NewGuid(); ExactBoundingBox = ebb; Cell = rc; SplitLimitExponent = ns'.[0].SplitLimitExponent; SubNodes = ns }
            

module QMergeNode =

    let ofNodes dom (first : QNodeRef) (second : QNodeRef) : QMergeNode =
        let isSpecialOverlappingCase = first.Cell.IsCenteredAtOrigin && second.Cell.TouchesOrigin ||
                                       second.Cell.IsCenteredAtOrigin && first.Cell.TouchesOrigin
        invariant (first.Cell = second.Cell || isSpecialOverlappingCase) "42a0c7ba-cf29-4820-b420-a74d668cb159"
        let ebb = Box2d(first.ExactBoundingBox, second.ExactBoundingBox)
        {
            Id = Guid.NewGuid()
            ExactBoundingBox = ebb; Cell = first.Cell; SplitLimitExponent = first.SplitLimitExponent
            Dominance = dom; First = first; Second = second
        }

    