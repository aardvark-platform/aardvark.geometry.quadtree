﻿namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Threading

module Serialization =

    type Cache = {
        TryGet : Guid -> QNodeRef option
        Add    : Guid -> QNodeRef -> unit
        }
    with
        static member Default =
            let d = Dictionary<Guid, QNodeRef>()
            let tryGet id = match d.TryGetValue(id) with | true, n -> Some n | false, _ -> None
            let add id n = d.Add(id, n)
            { TryGet = tryGet; Add = add }
        static member None =
            { TryGet = (fun _ -> None); Add = (fun _ _ -> ()) }

    type SerializationOptions = {
        Save : Guid -> byte[] -> unit
        TryLoad : Guid -> byte[] option
        Exists : Guid -> bool
        Decoder : Decoder
        Cache : Cache
        }
    with
    
        static member Default = {
            Save    = fun _ _   -> failwith "No store defined. Invariant b6a5c282-55c9-4a1d-8672-c600b82d3969."
            TryLoad = fun _     -> failwith "No store defined. Invariant fd1748c0-6eff-4e08-822a-3d708e54e393."
            Exists  = fun _     -> failwith "No store defined. Invariant 0f9c8cfd-21dd-4973-b88d-97629a5d2804."
            Decoder = fun _ _ _ -> failwith "No decoder defined. Invariant 49284081-8eac-42b1-8ecc-3ffb71551393."
            Cache   = Cache.Default
            }
    
        static member private inMemoryStoreCount : int = 0
    
        static member NewInMemoryStore (verbose : bool) =
            let store = Dictionary<Guid, byte[]>()
            let storeNumber = Interlocked.Increment(ref SerializationOptions.inMemoryStoreCount)
            {
                SerializationOptions.Default with
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
    
        static member NewInMemoryStore () = SerializationOptions.NewInMemoryStore(verbose = false)
    
        static member SimpleDiskStore (path : string) =
            let store = new Uncodium.SimpleStore.SimpleDiskStore(path)
            {
                SerializationOptions.Default with
                    Save    = fun id buffer -> store.Add(id.ToString(), buffer)
                    TryLoad = fun id        -> match store.Get(id.ToString()) with | null -> None | buffer -> Some buffer
                    Exists  = fun id        -> store.Contains(id.ToString())
            }

        static member SimpleStore (store : Uncodium.SimpleStore.ISimpleStore) =
            {
                SerializationOptions.Default with
                    Save    = fun id buffer -> store.Add(id.ToString(), buffer)
                    TryLoad = fun id        -> match store.Get(id.ToString()) with | null -> None | buffer -> Some buffer
                    Exists  = fun id        -> store.Contains(id.ToString())
            }
    
        static member SimpleFolderStore (path : string) =
            let store = new Uncodium.SimpleStore.SimpleFolderStore(path)
            { 
                SerializationOptions.Default with
                    Save    = fun id buffer -> store.Add(id.ToString(), buffer)
                    TryLoad = fun id        -> match store.Get(id.ToString()) with | null -> None | buffer -> Some buffer
                    Exists  = fun id        -> store.Contains(id.ToString())
            }

        member this.LoadNode (id : Guid) : QNodeRef =
            match this.Cache.TryGet(id) with
            | Some n -> n
            | None   ->
                match this.TryLoad id with
                | None -> NoNode
                | Some buffer ->
                    let struct (def, o) = DurableCodec.Deserialize(buffer)
                    let n = this.Decoder this def o
                    this.Cache.Add id n
                    n
    
    and

        Decoder = SerializationOptions -> Durable.Def -> obj -> QNodeRef

    (*
        QInnerNode

        durable definition 1f7baa27-5bcc-420f-89a3-714b65d93a2d
    *)

    let private encodeQInnerNode (options : SerializationOptions) (n : QInnerNode) : byte[] =

        let map = List<KeyValuePair<Durable.Def, obj>>()
        
        // node properties
        map.Add(kvp Defs.NodeId n.Id)
        map.Add(kvp Defs.CellBounds n.Cell)
        map.Add(kvp Defs.SplitLimitExponent n.SplitLimitExponent)
        map.Add(kvp Defs.ExactBoundingBox n.ExactBoundingBox)

        // collect subnode IDs 
        let ids = n.SubNodes |> Array.map (fun x -> match x with | NoNode -> Guid.Empty | _ -> x.Id)
        map.Add(kvp Defs.SubnodeIds ids)

        DurableCodec.Serialize(Defs.NodeInner, map)


    let private decodeQInnerNode (options: SerializationOptions) (def : Durable.Def) (o : obj) : QNodeRef =
        let map  = o :?> ImmutableDictionary<Durable.Def, obj>
        let id   = map.Get(Defs.NodeId)              :?> Guid
        let cell = map.Get(Defs.CellBounds)          :?> Cell2d
        let sle  = map.Get(Defs.SplitLimitExponent)  :?> int
        let ebb  = match map.TryGetValue(Defs.ExactBoundingBox) with
                   | true,  x -> x :?> Box2d
                   | false, _ -> Box2d.Invalid

        let nsIds = map.Get(Defs.SubnodeIds) :?> Guid[]
        let ns = nsIds |> Array.map (fun k -> if k = Guid.Empty then NoNode else  OutOfCoreNode (k, (fun () -> options.LoadNode k)))
                
        
        InMemoryInner {
            Id = id; ExactBoundingBox = ebb; Cell = cell; SplitLimitExponent = sle;
            SubNodes = ns
            }



    (*
        QMergeNode

        durable definition 2d80c73e-ed3e-442a-a631-4f570ff838fd
    *)
    
    let private encodeQMergeNode (options : SerializationOptions) (n : QMergeNode) : byte[] =
        let map = List<KeyValuePair<Durable.Def, obj>>()
        
        // node properties
        map.Add(kvp Defs.NodeId n.Id)
        map.Add(kvp Defs.CellBounds n.Cell)
        map.Add(kvp Defs.SplitLimitExponent n.SplitLimitExponent)
        map.Add(kvp Defs.ExactBoundingBox n.ExactBoundingBox)
        map.Add(kvp Defs.Dominance (match n.Dominance with 
                                   | FirstDominates         -> Defs.DominanceFirst.Id
                                   | SecondDominates        -> Defs.DominanceSecond.Id
                                   | MoreDetailedOrFirst    -> Defs.DominanceMoreDetailedOrFirst.Id
                                   | MoreDetailedOrSecond   -> Defs.DominanceMoreDetailedOrSecond.Id
                                   ))
        // collect subnode IDs
        let ids = [| n.First.Id; n.Second.Id |]
        map.Add(kvp Defs.SubnodeIds ids)

        DurableCodec.Serialize(Defs.NodeMerge, map)

    let private def2dominance = Map.ofList [
        Defs.DominanceFirst.Id, FirstDominates
        Defs.DominanceSecond.Id, SecondDominates
        Defs.DominanceMoreDetailedOrFirst.Id, MoreDetailedOrFirst
        Defs.DominanceMoreDetailedOrSecond.Id, MoreDetailedOrSecond
        ]

    let private decodeQMergeNode(options: SerializationOptions) (def : Durable.Def) (o : obj) : QNodeRef =
        let map  = o :?> ImmutableDictionary<Durable.Def, obj>
        let id   = map.Get(Defs.NodeId)              :?> Guid
        let cell = map.Get(Defs.CellBounds)          :?> Cell2d
        let sle  = map.Get(Defs.SplitLimitExponent)  :?> int
        let ebb  = match map.TryGetValue(Defs.ExactBoundingBox) with
                   | true,  x -> x :?> Box2d
                   | false, _ -> Box2d.Invalid

        let domDef  = map.Get(Defs.Dominance) :?> Guid
        let dom = Map.find domDef def2dominance

        let nsIds = map.Get(Defs.SubnodeIds) :?> Guid[]
        let ns = nsIds |> Array.map (fun k -> if k = Guid.Empty then NoNode else OutOfCoreNode (k, (fun () -> options.LoadNode k)))
                
        
        InMemoryMerge {
            Id = id; ExactBoundingBox = ebb; Cell = cell; SplitLimitExponent = sle;
            Dominance = dom; First = ns.[0]; Second = ns.[1]
            }

    (*
        QLinkedNode

        durable definition 8628aab6-a416-42ab-9192-bae0d5590f4f
    *)
    
    let private encodeQLinkedNode (options : SerializationOptions) (n : QLinkedNode) : byte[] =
        let map = List<KeyValuePair<Durable.Def, obj>>()
        
        // node properties
        map.Add(kvp Defs.NodeId n.Id)
        map.Add(kvp Defs.TargetId n.Target.Id)
        DurableCodec.Serialize(Defs.NodeLinked, map)

    let private decodeQLinkedNode(options: SerializationOptions) (def : Durable.Def) (o : obj) : QNodeRef =
        let map  = o :?> ImmutableDictionary<Durable.Def, obj>
        let id   = map.Get(Defs.NodeId)              :?> Guid
        let targetId = map.Get(Defs.TargetId)        :?> Guid
        
        let target = OutOfCoreNode (targetId, (fun () -> options.LoadNode targetId))

        LinkedNode { Id = id; Target = target }

    (*
        QNode

        durable definition c74fad23-1211-4073-94e5-54b778e0d295
    *)

    let private encodeQNode (options : SerializationOptions) (n : QNode) : byte[] =

        let map = List<KeyValuePair<Durable.Def, obj>>()
        
        // node properties
        map.Add(kvp Defs.NodeId n.Id)
        map.Add(kvp Defs.CellBounds n.Cell)
        map.Add(kvp Defs.SplitLimitExponent n.SplitLimitExponent)
        map.Add(kvp Defs.ExactBoundingBox n.ExactBoundingBox)
                        
        // layers
        for layer in n.LayerSet.Layers do
            let layerDef = Defs.GetLayerFromDef layer.Def
            let dm = layer.Materialize().ToDurableMap ()
            map.Add(kvp layerDef dm)
        
        DurableCodec.Serialize(Defs.NodeLeaf, map)

    let private decodeQNode (options: SerializationOptions) (def : Durable.Def) (o : obj) =
        
        let map = o :?> ImmutableDictionary<Durable.Def, obj>
        let id   = map.Get(Defs.NodeId)              :?> Guid
        let cell = map.Get(Defs.CellBounds)          :?> Cell2d
        let sle  = map.Get(Defs.SplitLimitExponent)  :?> int
        let ebb  = match map.TryGetValue(Defs.ExactBoundingBox) with
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

        let n = QNode(id, ebb, cell, sle, layerSet)
        InMemoryNode n

    (*
        obsolete QNode
        
        durable definition e497f9c1-c903-41c4-91de-32bf76e009da
    *)

    type OldQNodeEbbMode = | ComputeExactBbWhichIsSlow | ComputeApprBbWhichIsFast
    let rec private decodeOldQNode (ebbMode : OldQNodeEbbMode) (options: SerializationOptions) (def : Durable.Def) (o : obj) : QNodeRef =
        
        let map  = o :?> ImmutableDictionary<Durable.Def, obj>
        let id   = map.Get(Defs.NodeId)              :?> Guid
        let cell = map.Get(Defs.CellBounds)          :?> Cell2d
        let sle  = map.Get(Defs.SplitLimitExponent)  :?> int
        
        let layerSet =  
            map 
            |> Seq.choose (fun kv ->
                match Defs.TryGetDefFromLayer kv.Key with
                | Some def -> 
                    let m = kv.Value :?> ImmutableDictionary<Durable.Def, obj>
                    Layer.FromDurableMap def m |> Some
                | None -> None
                )
            |> Seq.toArray
            |> LayerSet
        invariant (layerSet.Layers.Length > 0) "68ca6608-921c-4868-b5f2-3c6f6dc7ab57"
        
        // generating ebb from layer data is fast (no loading of out-of-core subnodes)
        // but overstates the actual ebb
        let ebbFromLayerSet =
            match map.TryGetValue(Defs.ExactBoundingBox) with
            | true,  x -> failwith "Old node data should not have ebb. Error bd29a126-2cdb-47ee-a8c6-342c9f4aec6c."  //x :?> Box2d
            | false, _ -> layerSet.BoundingBox //Box2d.Invalid

        // this is the exact ebb
        // (computed from the original data)
        let mutable ebbFromOutOfCore = Box2d.Invalid
        let subNodes =
            match map.TryGetValue(Defs.SubnodeIds) with
            | (false, _)   -> None
            | (true, o) ->
                let keys = o :?> Guid[]
                let loadChildren = match ebbMode with
                                   | ComputeApprBbWhichIsFast  -> keys |> Array.forall (fun x -> x <> Guid.Empty)
                                   | ComputeExactBbWhichIsSlow -> true
                let xs = keys |> Array.map (fun k ->
                    if k = Guid.Empty then
                        NoNode
                    else
                        if loadChildren then 
                            // https://github.com/aardvark-platform/aardvark.geometry.quadtree/issues/3
                            // load children for more exact bb as long as node has less than 4 children
                            // this will traverse further down the tree in border regions to better appr. the real bb
                            // but is still fast because most of the tree will not be loaded from store
                            let tmp = options.LoadNode k
                            ebbFromOutOfCore <- Box2d(ebbFromOutOfCore, tmp.ExactBoundingBox)
                        
                        OutOfCoreNode (k, (fun () -> options.LoadNode k))
                    )
                Some xs

        let ebb = if ebbFromOutOfCore.IsValid then ebbFromOutOfCore else ebbFromLayerSet

        match subNodes with
        | Some ns -> InMemoryInner { Id = id; ExactBoundingBox = ebb; Cell = cell; SplitLimitExponent = sle; SubNodes = ns }
        | None    -> InMemoryNode(QNode(id, ebb, cell, sle, layerSet))
                    

    (*
        save/encode
    *)

    let rec Save (options: SerializationOptions) (root : QNodeRef) : Guid =

        let inline saveBuffer id getbuffer children =
            invariant (id <> Guid.Empty) "d8040e37-03b4-4ec5-ba4b-cbb6f91042f1"
            if options.Exists id then
                id
            else
                let buffer = getbuffer()
                options.Save id buffer
                for child in children do Save options child |> ignore
                id

        match root with
        | NoNode                -> Guid.Empty
        | OutOfCoreNode (id, _) -> id
        | InMemoryNode n        -> saveBuffer n.Id (fun () -> encodeQNode options n) Array.empty
        | InMemoryInner n       -> saveBuffer n.Id (fun () -> encodeQInnerNode options n) n.SubNodes
        | InMemoryMerge n       -> saveBuffer n.Id (fun () -> encodeQMergeNode options n) [| n.First; n.Second |]
        | LinkedNode n          -> saveBuffer n.Id (fun () -> encodeQLinkedNode options n) [| n.Target |]

    
    (*
        load
    *)

    let private decoders : Map<Guid, Decoder> = Map.ofList <| [
        Defs.Node.Id,       decodeOldQNode ComputeApprBbWhichIsFast
        Defs.NodeInner.Id,  decodeQInnerNode
        Defs.NodeLeaf.Id,   decodeQNode
        Defs.NodeMerge.Id,  decodeQMergeNode
        Defs.NodeLinked.Id, decodeQLinkedNode
        ]

    let rec private decode (options: SerializationOptions) (def : Durable.Def) (o : obj) : QNodeRef =
        match decoders |> Map.tryFind def.Id with
        | Some decoder -> decoder { options with Decoder = decode } def o
        | None -> sprintf "Unknown node type %A. Error af2665dc-f137-4eaf-a3df-11fb7afd17cf." def |> failwith

    let Load (options: SerializationOptions) (id : Guid) : QNodeRef =

        match options.TryLoad id with
        | None -> NoNode
        | Some buffer ->
            let struct (def, o) = DurableCodec.Deserialize(buffer)
            decode options def o

    /// Enumerates node ids of given quadtree.
    let rec EnumerateKeys (outOfCore : bool) (qtree : QNodeRef) : Guid seq = seq {
        let recurse = EnumerateKeys outOfCore
        match qtree with
        | NoNode -> ()
        | InMemoryNode n          -> yield n.Id
        | InMemoryInner n         -> yield n.Id; for x in n.SubNodes do yield! recurse  x
        | InMemoryMerge n         -> yield n.Id; yield! recurse n.First; yield! recurse n.Second
        | OutOfCoreNode (_, load) -> yield! load() |> recurse
        | LinkedNode n            -> yield n.Id; yield! recurse n.Target

        }

    /// Export quadtree with given id from source to target.
    let Export 
        (source : SerializationOptions)
        (target : SerializationOptions)
        (progress : Option<int * int -> unit>)
        (ct : CancellationToken)
        (id : Guid)
        : unit =

        try
            let source = { source with Cache = Cache.None }
            let target = { target with Cache = Cache.None }

            let keys = id |> Load source |> EnumerateKeys true

            ct.ThrowIfCancellationRequested ()
            let total = if progress.IsSome then keys |> Seq.length else 0

            keys |> Seq.iteri (fun i key ->
                ct.ThrowIfCancellationRequested ()
                match source.TryLoad key with
                | Some buffer ->
                    target.Save key buffer
                    if progress.IsSome then progress.Value (i + 1, total) else ()
                | None ->
                    sprintf "Failed to load key %A from source." key |> failwith
                )

        with
        | :? OperationCanceledException -> ()

