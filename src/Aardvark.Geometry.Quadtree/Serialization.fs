namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Threading

module Serialization =

    type SerializationOptions = {
        Save : Guid -> byte[] -> unit
        TryLoad : Guid -> byte[] option
        Exists : Guid -> bool
        Decoder : Decoder
        }
    with
    
        static member Default = {
            Save    = fun _ _   -> failwith "No store defined. Invariant b6a5c282-55c9-4a1d-8672-c600b82d3969."
            TryLoad = fun _     -> failwith "No store defined. Invariant fd1748c0-6eff-4e08-822a-3d708e54e393."
            Exists  = fun _     -> failwith "No store defined. Invariant 0f9c8cfd-21dd-4973-b88d-97629a5d2804."
            Decoder = fun _ _ _ -> failwith "No decoder defined. Invariant 49284081-8eac-42b1-8ecc-3ffb71551393."
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
                    Save    = fun id buffer -> store.Add(id.ToString(), buffer, fun () -> buffer)
                    TryLoad = fun id        -> match store.Get(id.ToString()) with | null -> None | buffer -> Some buffer
                    Exists  = fun id        -> store.Contains(id.ToString())
            }
    
        static member SimpleFolderStore (path : string) =
            let store = new Uncodium.SimpleStore.SimpleFolderStore(path)
            { 
                SerializationOptions.Default with
                    Save    = fun id buffer -> store.Add(id.ToString(), buffer, fun () -> buffer)
                    TryLoad = fun id        -> match store.Get(id.ToString()) with | null -> None | buffer -> Some buffer
                    Exists  = fun id        -> store.Contains(id.ToString())
            }

        member this.LoadNode (id : Guid) : QNodeRef =
            match this.TryLoad id with
            | None -> NoNode
            | Some buffer ->
                let struct (def, o) = DurableCodec.Deserialize(buffer)
                this.Decoder this def o
    
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
        failwith "todo: decodeQInnerNode"



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
        let ns = nsIds |> Array.map (fun k -> OutOfCoreNode (k, (fun () -> options.LoadNode k)))
                
        
        InMemoryMerge {
            Id = id; ExactBoundingBox = ebb; Cell = cell; SplitLimitExponent = sle;
            Dominance = dom; First = ns.[0]; Second = ns.[1]
            }

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
        
        DurableCodec.Serialize(Defs.Node, map)

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

    let rec private decodeOldQNode (options: SerializationOptions) (def : Durable.Def) (o : obj) : QNodeRef =
        
        let map  = o :?> ImmutableDictionary<Durable.Def, obj>
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

        let subNodes =
            match map.TryGetValue(Defs.SubnodeIds) with
            | (false, _)   -> None
            | (true, o) ->
                let keys = o :?> Guid[]
                let xs = keys |> Array.map (fun k ->
                    if k = Guid.Empty then
                        NoNode
                    else
                        OutOfCoreNode (k, (fun () -> options.LoadNode k))
                    )
                Some xs

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
                                    

    
    (*
        load
    *)

    let private decoders : Map<Guid, Decoder> = Map.ofList <| [
        Defs.Node.Id,       decodeOldQNode
        Defs.NodeInner.Id,  decodeQInnerNode
        Defs.NodeLeaf.Id,   decodeQNode
        Defs.NodeMerge.Id,  decodeQMergeNode
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

