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

    /// The maximum tile size, given as width = height = 2^SplitLimitExponent.
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

    /// Takes all layers of up to 4 quadrants of given root cell (subNodeRefs)
    /// and generates layers for root cell (with half the resolution). 
    let generateLodLayers (subNodeRefs : QNodeRef[]) (rootCell : Cell2d) : ILayer[] =
    
        if subNodeRefs.Length = 0 then

            // No data -> done.
            Array.empty

        else

            (*
                Get all nodes into memory (except NoNode).
                All nodes should be unique quadrants of root cell.
            *)
            let subnodes = subNodeRefs |> Array.choose (fun x -> x.TryGetInMemory())

            let noNodeCount = subNodeRefs |> Array.sumBy (fun x -> match x with | NoNode -> 1 | _ -> 0)
            invariantm (subnodes.Length = subNodeRefs.Length - noNodeCount)
                "Failed to load all nodes into memory."
                "0607fed7-91cc-4a68-ba01-2512288c607a"

            invariantm (subnodes |> Array.forall (fun n -> rootCell.Contains(n.Cell)))
                (sprintf "All subnodes must be contained in root cell %A. Subnodes are %A." rootCell (subnodes |> Array.map(fun n -> n.Cell)))
                "8c78a230-e467-4f97-b2e0-0db79e003a46"

            invariantm (subnodes |> Array.forall (fun n -> rootCell.Exponent = n.Cell.Exponent + 1))
                (sprintf "All subnodes must be quadrants of root cell %A. Subnodes are %A." rootCell (subnodes |> Array.map(fun n -> n.Cell)))
                "16a32b30-51a6-43fe-ab67-9fc854b1dde2"

            invariantm (subnodes |> Array.distinctBy (fun n -> n.Cell) |> Array.length = subnodes.Length)
                (sprintf "All subnodes must be unique quadrants of root cell %A. Subnodes are %A." rootCell (subnodes |> Array.map(fun n -> n.Cell)))
                "3688841c-fe01-4d0d-b7e3-21e0c20a9c6c"


            (*
                What is the target sample size?
            *)
            
            // ... first, check if all subnodes have the same split limit exponent,
            //     which specifies the tile size as width = height = 2^SplitLimitExponent
            let xs = subnodes |> Array.map (fun n -> n.SplitLimitExponent) |> Array.distinct
            
            invariantm (xs.Length = 1) 
                (sprintf "Can't combine nodes with different split limit exponents (%A)." xs)
                "a9bf0243-e481-49a5-908e-d5af94cec2af"

            let splitLimitExponent = xs |> Array.exactlyOne

            // ... we want to fit a tile of width = height = 2^splitLimitExponent
            //     into a root cell of size rootCell.Exponent
            let targetSampleSize = rootCell.Exponent - splitLimitExponent

            
            (*
                collect all layers from all nodes
            *)
            let allLayers = subnodes |> Array.collect (fun x -> x.Layers) 

            invariantm (allLayers |> Array.forall (fun layer -> layer.SampleExponent + 1 = targetSampleSize))
                (sprintf "All layers must have a sample size of 1 less than the target sample size (%d). Layers are (%A)." targetSampleSize allLayers)
                "8d206e1c-d54a-4841-829d-2e783bde0815"

            (*
                resample all layers to half the resolution
            *)
            let allLayersResampled = allLayers |> Array.map (fun layer -> layer.ResampleUntyped rootCell)

            invariantm (allLayersResampled |> Array.forall (fun layer -> layer.SampleExponent = targetSampleSize))
                (sprintf "All resampled layers must have target sample size (%d). Resampled layers are (%A)." targetSampleSize allLayersResampled)
                "1fc40968-1224-4eaa-b1c1-a0ccc28092af"


            (*
                finally, merge resampled layers with same definition
            *)
            let resampledByDef = allLayersResampled |> Array.groupBy (fun layer -> layer.Def)
            let merged = resampledByDef |> Array.map (fun (_, xs) -> Layer.Merge xs)

            invariantm (merged |> Array.forall (fun x -> x.IsSome))
                (sprintf "There should be a merge result for each definition. Merged layers are %A." merged)
                "1fc40968-1224-4eaa-b1c1-a0ccc28092af"

            let result = merged |> Array.map (fun x -> x.Value)
            result


    /// Returns new extended tree starting at root.
    /// Root and node must not be centered.
    /// Root must contain node.
    /// Newly created nodes contain resampled layers (LoD).
    let rec extendUpTo (root : Cell2d) (nodeRef : QNodeRef) : QNodeRef =

        invariantm (not root.IsCenteredAtOrigin) "Root must not be centered."                   "b18fe51c-22a8-4a3b-a696-d54bf2f8cda0"

        match nodeRef.TryGetInMemory() with
        | None -> NoNode
        | Some node ->
            
            invariantm (not node.Cell.IsCenteredAtOrigin) "Node must not be centered."          "c8f2a8bd-2f4f-4467-843e-1d660d6ac329"
            invariantm (root.Contains(node.Cell)) "Root must contain node."                     "a48ca4ab-3f20-45ff-bd3c-c08f2a8fcc15"
            invariant (root.Exponent >= node.Cell.Exponent)                                     "cda4b28d-4449-4db2-80b8-40c0617ecf22"
            invariant (root.BoundingBox.Contains(node.SampleWindowBoundingBox))                 "3eb5c9c4-a78e-4788-b1b2-2727564524ee"
              
            let numberOfLevelsBelowRoot = root.Exponent - node.Cell.Exponent
            match numberOfLevelsBelowRoot with
                
            | 0 -> // already at desired root -> done
                invariant (root = node.Cell)                                                    "cede9588-7ea0-408d-89a3-86ed9d916930"
                InMemoryNode node

            | 1 -> // one level below root, both non-centered

                invariant (root.Exponent = node.Cell.Exponent + 1)                              "b652d9bc-340a-4312-8407-9fdee62ffcaa"

                let qi = root.GetQuadrant(node.Cell)
                
                invariantm qi.HasValue 
                    (sprintf "Node %A must be quadrant of root %A." node.Cell root )            "09575aa7-38b3-4afa-bb63-389af3301fc0"
                    
                
                let subnodes = Array.create 4 NoNode
                subnodes.[qi.Value] <- InMemoryNode node

                let lodlayers = generateLodLayers subnodes root
                
                let result = QNode(Guid.NewGuid(), root, node.SplitLimitExponent,
                                   node.OriginalSampleExponent, lodlayers, subnodes)
                
                InMemoryNode result

            | x ->    // more than 1 level below root
                invariant (x > 1) "de49dcd3-a45a-4854-b71b-45866d6f82de"
                nodeRef
                |> extendUpTo node.Cell.Parent 
                |> extendUpTo root
               

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
