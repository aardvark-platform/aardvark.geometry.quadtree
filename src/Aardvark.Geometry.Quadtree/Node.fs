﻿namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System
open System.Collections.Generic
open System.Threading
open System.Collections.Immutable

(*
    Node.
*)

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

type QNode(id : Guid, cell : Cell2d, splitLimitExp : int, originalSampleExponent : int, layers : ILayer[], subNodes : QNodeRef[] option) =

    do
        invariantm (layers.Length > 0) "No layers." "fe0e56d7-9bc2-4f61-8b36-0ed7fcc4bc56"

        invariantm (cell.Exponent - splitLimitExp = layers.[0].SampleExponent) 
            "Sample exponent does not match split limit and node cell."
            "1ec76eaa-534b-4339-b63b-8e0399562bb1"

        let w = layers.[0].SampleWindow
        let e = layers.[0].SampleExponent
        let bb = cell.BoundingBox
        for layer in layers do
            invariantm (layer.SampleExponent = e) "Layers exponent mismatch."   "7adc422c-effc-4a86-b493-ff1cd0f9e991"
            invariantm (layer.SampleWindow = w)   "Layers window mismatch."     "74a57d1d-6a7f-4f9e-b26c-41a1e79cb989"
            invariantm (bb.Contains(layer.Mapping.BoundingBox)) 
                (sprintf "Layer %A is outside node bounds." layer.Def.Id)       "dbe069cc-df5c-42c1-bb58-59c5a061ee15"
            
        match subNodes with
        | None -> ()
        | Some subNodes ->
            invariant (subNodes.Length = 4)                                     "20baf723-cf32-46a6-9729-3b4e062ceee5"
            let children = cell.Children
            for i = 0 to 3 do
                let sn = subNodes.[i]
                match sn with 
                | NoNode -> ()
                | InMemoryNode x ->
                    invariant (x.Cell = children.[i])                           "6243dc09-fae4-47d5-8ea7-834c3265988b"
                    invariant (cell.Exponent = x.Cell.Exponent + 1)             "780d98cc-ecab-43fc-b492-229fb0e208a3"
                | OutOfCoreNode _ -> ()

    new (id : Guid, cell : Cell2d, splitLimitExp : int, originalSampleExponent : int, layers : ILayer[], subNodes : QNode option[]) =
        let subNodes = subNodes |> Array.map (fun x -> match x with | Some n -> InMemoryNode n | None -> NoNode)
        QNode(id, cell, splitLimitExp, originalSampleExponent, layers, Some subNodes)

    new (id : Guid, cell : Cell2d, splitLimitExp : int, originalSampleExponent : int, layers : ILayer[], subNodes : QNodeRef[]) =
        QNode(id, cell, splitLimitExp, originalSampleExponent, layers, Some subNodes)

    /// Create leaf node.
    new (cell : Cell2d, splitLimitExp : int, originalSampleExponent : int, layers : ILayer[]) =
        QNode(Guid.NewGuid(), cell, splitLimitExp, originalSampleExponent, layers, None)

    member _.Id with get() = id

    member _.Cell with get() = cell

    member _.SplitLimitExponent with get() = splitLimitExp

    member _.OriginalSampleExponent with get() = originalSampleExponent

    member _.Layers with get() = layers

    member _.SampleWindow with get() = layers.[0].SampleWindow

    member _.SampleWindowBoundingBox with get() = layers.[0].BoundingBox

    member _.SampleExponent with get() = layers.[0].SampleExponent

    member _.Mapping with get() = layers.[0].Mapping

    member _.SubNodes with get() = subNodes

    member _.IsInnerNode with get() = subNodes.IsSome

    member _.IsLeafNode  with get() = subNodes.IsNone

    member _.WithLayers (newLayers : ILayer[]) = QNode(Guid.NewGuid(), cell, splitLimitExp, originalSampleExponent, newLayers, subNodes)

    member this.GetLayer<'a>(def : Durable.Def) : Layer<'a> =
        layers |> Array.find (fun x -> x.Def.Id = def.Id) :?> Layer<'a>

    member this.Save options : Guid =
        let map = List<KeyValuePair<Durable.Def, obj>>()

        // node properties
        map.Add(kvp Defs.NodeId id)
        map.Add(kvp Defs.CellBounds cell)
        map.Add(kvp Defs.SplitLimitExponent splitLimitExp)
        map.Add(kvp Defs.OriginalSampleExponent originalSampleExponent)
            
        // layers
        for layer in layers do
            let layerDef = Defs.GetLayerFromDef layer.Def
            let dm = layer.Materialize().ToDurableMap ()
            map.Add(kvp layerDef dm)

        // children
        match subNodes with
        | Some xs ->

            // recursively store subnodes (where necessary)
            for x in xs do 
                match x with 
                | InMemoryNode n    -> if not (options.Exists n.Id) then n.Save options |> ignore
                | NoNode            -> () // nothing to store
                | OutOfCoreNode _   -> () // already stored

            // collect subnode IDs 
            let ids = xs |> Array.map (fun x -> 
                match x with 
                | NoNode -> Guid.Empty
                | InMemoryNode n -> n.Id
                | OutOfCoreNode (id, _) -> id
                )
            map.Add(kvp Defs.SubnodeIds ids)
        | None -> ()

        let buffer = DurableCodec.Serialize(Defs.Node, map)
        options.Save id buffer
        id

    member this.SplitCenteredNodeIntoQuadrantNodesAtSameLevel () : QNode[] =
        if this.Cell.IsCenteredAtOrigin then
            let subLayers = cell.Children |> Array.map (fun subCell ->
                let cell = subCell.Parent
                let subBox = cell.GetBoundsForExponent(this.SampleExponent)
                let subLayers = layers |> Array.choose (fun l -> l.WithWindow subBox)
                (cell, subLayers) 
                )
            subLayers |> Array.map (fun (subCell, subLayers) ->
                QNode(Guid.NewGuid(), subCell, splitLimitExp, originalSampleExponent, subLayers, None)
                )
        else
            failwith "Node must be centered at origin to split into quadrant nodes at same level. Invariant 6a4321b1-0f59-4574-bf51-fcce423fa389."

    member this.Split () : QNode[] =

        let subLayers = cell.Children |> Array.map (fun subCell ->
            let subBox = subCell.GetBoundsForExponent(this.SampleExponent)
            let subLayers = layers |> Array.choose (fun l -> l.WithWindow subBox)
            (subCell, subLayers) 
            )
        let subNodes = subLayers |> Array.map (fun (subCell, subLayers) ->
            QNode(Guid.NewGuid(), subCell, splitLimitExp, originalSampleExponent, subLayers, None)
            )

        subNodes

    member this.GetAllSamples () : Cell2d[] =
        this.SampleWindow.GetAllSamples(this.SampleExponent)

    member this.GetAllSamplesInsideWindow (window : Box2l) : Cell2d[] =
        invariant (this.SampleWindow.Contains(window)) "f244101f-975c-4a0a-8c03-20e0618834b4"
        window.GetAllSamples(this.SampleExponent)

    member this.GetAllSamplesFromFirstMinusSecond (first : Box2l) (second : Box2l) : Cell2d[] =
        first.GetAllSamplesFromFirstMinusSecond(second, this.SampleExponent)

    member this.GetSample (p : V2d) : Cell2d =
        this.Mapping.GetSampleCell(p)

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
                    let originalSampleExp   = map.Get(Defs.OriginalSampleExponent)  :?> int
                
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
                                    OutOfCoreNode (k, (fun () ->
                                        match QNode.Load options k with
                                        | InMemoryNode n        -> n
                                        | NoNode                -> failwith "Invariant de92e0d9-0dd2-4fc1-b4c7-0ace2910ce24."
                                        | OutOfCoreNode (_, _)  -> failwith "Invariant 59c84c71-9043-41d4-abc0-4e1f49b8e2ba."
                                        ))
                                )
                            Some xs

                    let n = QNode(id, cell, splitLimitExp, originalSampleExp, layers, subNodes)
                    InMemoryNode n
                else
                    failwith "Loading quadtree failed. Invalid data. f1c2fcc6-68d2-47f3-80ff-f62b691a7b2e."

and

    QNodeRef =
    | NoNode
    | InMemoryNode of QNode
    | OutOfCoreNode of Guid * (unit -> QNode)
    with
        /// Get referenced node (from memory or load out-of-core), or None if NoNode.
        member this.TryGetInMemory () : QNode option =
            match this with
            | NoNode -> None
            | InMemoryNode n -> Some n
            | OutOfCoreNode (_, load) -> load() |> Some

        /// Forces property Id. Throws exception if NoNode.
        member this.Id with get() = match this.TryGetInMemory() with | Some n -> n.Id | None -> Guid.Empty

        /// Forces property Cell. Throws exception if NoNode.
        member this.Cell with get() = this.TryGetInMemory().Value.Cell

        /// Forces property SampleWindowBoundingBox. Throws exception if NoNode.
        member this.SampleWindowBoundingBox with get() = this.TryGetInMemory().Value.SampleWindowBoundingBox

        /// Forces GetLayer. Throws exception if NoNode.
        member this.GetLayer<'a> def = this.TryGetInMemory().Value.GetLayer<'a> def

module QNode =

    let toRef (n : QNode option) =
        match n with
        | Some n -> InMemoryNode n
        | None -> NoNode

    let tryGetInMemory (n : QNodeRef) : QNode option = n.TryGetInMemory()

    let generateLodLayers (subNodes : QNodeRef[]) (rootCell : Cell2d) =

        let subNodes = subNodes |> Array.choose (fun x -> x.TryGetInMemory())
        invariant (subNodes.Length > 0) "641ef4b4-65b3-4e76-bbb6-c7046452801a"
        
        //let minSubNodeExponent = (subNodes |> Array.minBy (fun x -> x.SampleExponent)).SampleExponent
        let maxSubNodeExponent = (subNodes |> Array.maxBy (fun x -> x.SampleExponent)).SampleExponent

        let result =
            subNodes 
            |> Seq.collect (fun x -> x.Layers) 
            |> Seq.groupBy (fun x -> x.Def)
            |> Seq.choose (fun (_, xs) ->
                let maxExponent = xs |> Seq.map (fun x -> x.SampleExponent) |> Seq.max
                xs 
                |> Seq.map (fun layer ->
                    let mutable l = layer
                    while l.SampleExponent < maxExponent do l <- l.ResampleUntyped rootCell
                    l
                    )
                |> Layer.Merge
                )
            |> Seq.map (fun layer -> layer.ResampleUntyped rootCell)
            |> Seq.toArray


        let resultExponents = result |> Array.groupBy (fun x -> x.SampleExponent)
        if resultExponents.Length <> 1 then
            failwith "Invariant abbd4b37-d816-4ba4-9427-183458ff6ad1."

        let selfExponent = resultExponents.[0] |> fst
        invariant (selfExponent = maxSubNodeExponent + 1) "4a8cbec0-4e14-4eb4-a21a-0af370cc1d81"

        result

    /// Computes and attaches a parent node to given tree until given root is reached.
    /// Returns new tree starting at root.
    /// Newly attached nodes contains resampled layers (LoD) from given node.
    let rec extendUpTo (root : Cell2d) (nodeRef : QNodeRef) : QNodeRef =

           match nodeRef.TryGetInMemory() with
           | None -> NoNode
           | Some node ->
               invariant (root.Contains(node.Cell))                                                "a48ca4ab-3f20-45ff-bd3c-c08f2a8fcc15"
               invariant (root.Exponent >= node.Cell.Exponent)                                     "cda4b28d-4449-4db2-80b8-40c0617ecf22"
               invariant (root.BoundingBox.Contains(node.SampleWindowBoundingBox))                 "3eb5c9c4-a78e-4788-b1b2-2727564524ee"
               
               if root = node.Cell then
                   InMemoryNode node
               else
                   invariant (root.Exponent > node.Cell.Exponent)                                  "b652d9bc-340a-4312-8407-9fdee62ffcaa"

                   if root.IsCenteredAtOrigin && node.Cell.IsCenteredAtOrigin then

                       let roots = root.Children
                       let nodes = match node.SubNodes with
                                   | Some xs -> xs
                                   | None    -> node.SplitCenteredNodeIntoQuadrantNodesAtSameLevel() |> Array.map InMemoryNode
                       let rootSubNodes  = Array.map2 extendUpTo roots nodes
                       let rootLodLayers = generateLodLayers rootSubNodes root

                       let n = QNode(Guid.NewGuid(), root, node.SplitLimitExponent,
                                     node.OriginalSampleExponent, rootLodLayers,
                                     rootSubNodes)

                       InMemoryNode n

                   else
                       let isChildOfCenteredRoot = root.IsCenteredAtOrigin && root.Exponent = node.Cell.Exponent + 1

                       let parentCell = if isChildOfCenteredRoot then root else node.Cell.Parent
                       let qi = parentCell.GetQuadrant(node.Cell)
                   
                       invariantm qi.HasValue                                                      "09575aa7-38b3-4afa-bb63-389af3301fc0"
                           (sprintf "Parent cell: %A; node cell: %A." parentCell node.Cell)
                   
                       let parentSubNodes = Array.create 4 NoNode
                       parentSubNodes.[qi.Value] <- InMemoryNode node
                       let parentLodLayers = generateLodLayers parentSubNodes root
                   
                       invariant (node.SampleExponent + 1 = parentLodLayers.[0].SampleExponent)    "7b0fa058-4812-4332-9547-0c33ee7ea7d5"
                   
                       let parentNode = QNode(Guid.NewGuid(), parentCell, node.SplitLimitExponent,
                                              node.OriginalSampleExponent, parentLodLayers,
                                              parentSubNodes)
                   
                       invariant (parentNode.Cell.Exponent = node.Cell.Exponent + 1)               "27b263d3-7fe5-49fd-96ca-422c37f9a31a"
                       invariant (parentNode.SampleExponent = node.SampleExponent + 1)             "6076b045-9bee-4285-b583-1d3d49bb259a"
                   
                       match extendUpTo root (InMemoryNode parentNode) with
                       | InMemoryNode result -> invariant (root.Exponent = result.Cell.Exponent)   "a0d249da-ed0d-4abe-8751-191ad0ffb9f9"
                                                InMemoryNode result
                       | _ -> failwith "Invariant 991571a4-ea05-4142-bd38-a6964319ba97."

    ///// Computes and attaches a parent node to given tree.
    ///// Returns new tree with parent attached.
    ///// Parent node contains resampled layers (LoD) from given node.
    //let attachParentNode (nodeRef : QNodeRef) : QNodeRef =
        
    //    match nodeRef.TryGetInMemory() with
    //    | None      -> NoNode
    //    | Some node ->

    //        if node.Cell.IsCenteredAtOrigin then

    //            extendUpTo node.Cell.Parent (InMemoryNode node)

    //        else
            
    //            let parentCell = node.Cell.Parent
    //            let i = parentCell.GetQuadrant(node.Cell).Value
    //            let subNodes = Array.create 4 NoNode
    //            subNodes.[i] <- nodeRef
    //            let parentLayers = node.Layers |> Array.map (fun x -> x.ResampleUntyped parentCell)
    //            let n = QNode(Guid.NewGuid(), parentCell, node.SplitLimitExponent, node.OriginalSampleExponent, parentLayers, subNodes)
    //            InMemoryNode n
