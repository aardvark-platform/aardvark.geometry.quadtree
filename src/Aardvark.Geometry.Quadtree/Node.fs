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

type IQNode =
    abstract member Id : Guid
    abstract member ExactBoundingBox : Box2d
    abstract member Cell : Cell2d
    abstract member SplitLimitExponent : int
    abstract member LayerSet : LayerSet option
    abstract member SubNodes : QNodeRef[] option

    abstract member Save : SerializationOptions -> Guid

    abstract member UpdateLayerSemantic : Durable.Def * Durable.Def -> IQNode option

    abstract member GetAllSamples : unit -> Cell2d[]
    abstract member GetAllSamplesInsideWindow : Box2l -> Cell2d[]
    abstract member GetAllSamplesFromFirstMinusSecond : Box2l * Box2l -> Cell2d[]
    abstract member GetSample : V2d -> Cell2d



and QNode(uid : Guid, exactBoundingBox : Box2d, cell : Cell2d, splitLimitExp : int, layers : LayerSet option, subNodes : QNodeRef[] option) =

    do
        match layers with
        | Some layers ->
            
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
            
        
        | None -> ()

        match subNodes with
        | None -> ()
        | Some subNodes ->
            invariant (subNodes.Length = 4)                                         "20baf723-cf32-46a6-9729-3b4e062ceee5"
            let children = cell.Children
            for i = 0 to 3 do
                let sn = subNodes.[i]
                match sn with 
                | NoNode -> ()
                | InMemoryNode x ->
                    invariant (x.Cell = children.[i])                               "6243dc09-fae4-47d5-8ea7-834c3265988b"
                    invariant (cell.Exponent = x.Cell.Exponent + 1)                 "780d98cc-ecab-43fc-b492-229fb0e208a3"
                | OutOfCoreNode _ -> ()

    member val ExactBoundingBox =
        if exactBoundingBox.IsInvalid then
            match subNodes with
            | None -> layers.Value.BoundingBox
            | Some ns ->
                let boxes : Box2d seq = ns |> Seq.map (fun n -> n.TryGetInMemory()) |> Seq.choose id |> Seq.map (fun n -> n.ExactBoundingBox)
                Box2d(boxes)
        else
            exactBoundingBox

    new (uid : Guid, exactBoundingBox : Box2d, cell : Cell2d, splitLimitExp : int, layers : LayerSet option, subNodes : QNode option[]) =
        let subNodes = subNodes |> Array.map (fun x -> match x with | Some n -> InMemoryNode n | None -> NoNode)
        QNode(uid, exactBoundingBox, cell, splitLimitExp, layers, Some subNodes)

    new (exactBoundingBox : Box2d, cell : Cell2d, splitLimitExp : int, layers : LayerSet option, subNodes : QNode option[]) =
        QNode(Guid.NewGuid(), exactBoundingBox, cell, splitLimitExp, layers, subNodes)

    new (uid : Guid, exactBoundingBox : Box2d, cell : Cell2d, splitLimitExp : int, layers : LayerSet option, subNodes : QNodeRef[]) =
        QNode(uid, exactBoundingBox, cell, splitLimitExp, layers, Some subNodes)

    /// Create leaf node.
    new (exactBoundingBox : Box2d, cell : Cell2d, splitLimitExp : int, layers : LayerSet option) =
        QNode(Guid.NewGuid(), exactBoundingBox, cell, splitLimitExp, layers, None)

    /// Create leaf node.
    new (exactBoundingBox : Box2d, cell : Cell2d, splitLimitExp : int, layers : LayerSet option, subNodes : QNodeRef[] option) =
        QNode(Guid.NewGuid(), exactBoundingBox, cell, splitLimitExp, layers, subNodes)

    interface IQNode with

        member _.Id with get() = uid

        member _.Cell with get() = cell

        member this.ExactBoundingBox with get() = this.ExactBoundingBox

        /// The maximum tile size, given as width = height = 2^SplitLimitExponent.
        member _.SplitLimitExponent with get() = splitLimitExp

        /// Replace all occurences of 'oldSemantic' with 'newSemantic'.
        /// Returns 'Some <newUpdatedOctree>' if 'oldSemantic' exists and is replaced.
        /// Returns 'None' if 'oldSemantic' does not exist.
        /// Throws if quadtree contains both 'oldSemantic' and 'newSemantic'.
        member this.UpdateLayerSemantic (oldSemantic : Durable.Def, newSemantic : Durable.Def) : IQNode option =

            match layers with
            | None -> None
            | Some layers ->

                match layers.UpdateLayerSemantic(oldSemantic, newSemantic) with
                | None -> None
                | Some newLayers ->
               
                    let id = Guid.NewGuid()

                    let newChildren = 
                        match subNodes with
                        | None -> None
                        | Some xs ->
                            xs |> Array.map (fun n ->
                                match n.TryGetInMemory() with
                                | None -> NoNode
                                | Some x -> 
                                    match x.UpdateLayerSemantic(oldSemantic, newSemantic) with
                                    | None   -> n
                                    | Some y -> InMemoryNode y
                                )
                            |> Some

                    QNode(id, exactBoundingBox, cell, splitLimitExp, Some newLayers, newChildren) :> IQNode |> Some
        
        member this.Save options : Guid =
            let map = List<KeyValuePair<Durable.Def, obj>>()

            // node properties
            map.Add(kvp Defs.NodeId uid)
            map.Add(kvp Defs.CellBounds cell)
            map.Add(kvp Defs.SplitLimitExponent splitLimitExp)
            map.Add(kvp Defs.ExactBoundingBox this.ExactBoundingBox)
                
            // layers
            match layers with
            | None -> ()
            | Some layers ->
                for layer in layers.Layers do
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
            options.Save uid buffer
            uid

        member _.LayerSet with get() = layers

        member _.SubNodes with get() = subNodes

        member this.GetAllSamples () : Cell2d[] = this.GetAllSamples ()
        member this.GetAllSamplesInsideWindow (window : Box2l) : Cell2d[] = this.GetAllSamplesInsideWindow (window)
        member this.GetAllSamplesFromFirstMinusSecond (first : Box2l, second : Box2l) : Cell2d[] = this.GetAllSamplesFromFirstMinusSecond(first, second)
        member this.GetSample (p : V2d) : Cell2d = this.GetSample(p)


    member _.Cell with get() = cell
    member _.LayerSet with get() = layers
    member _.SubNodes with get() = subNodes
    member _.IsInnerNode with get() = subNodes.IsSome
    member _.IsLeafNode  with get() = subNodes.IsNone

    member this.ContainsLayer (semantic : Durable.Def) : bool =
        this.TryGetLayer(semantic) |> Option.isSome

    member this.TryGetLayer (semantic : Durable.Def) : ILayer option =
        match layers with | Some x -> x.TryGetLayer(semantic) | None -> None

    member this.TryGetLayer<'a> (semantic : Durable.Def) : Layer<'a> option =
        match layers with | Some x -> x.TryGetLayer<'a>(semantic) | None -> None

    member this.GetLayer(semantic : Durable.Def) : ILayer =
        layers.Value.GetLayer(semantic)

    member this.GetLayer<'a>(semantic : Durable.Def) : Layer<'a> =
        layers.Value.GetLayer<'a>(semantic)

    member this.WithWindow (w : Box2l) : QNode option =

        match layers with
        | None -> None
        | Some layers ->
            match layers.WithWindow(w) with
            | None -> None
            | Some windowedLayers ->
                let ebb = windowedLayers.BoundingBox
                let n = QNode(ebb, cell, splitLimitExp, Some windowedLayers)
                Some n

    member this.WithoutChildren () : QNode =
        match subNodes with
        | Some _ -> QNode(exactBoundingBox, cell, splitLimitExp, layers)
        | None   -> this

    member this.WithLayers (newLayers : LayerSet option) : QNode =
        QNode(exactBoundingBox, cell, splitLimitExp, newLayers, subNodes)


    member this.GetAllSamples () : Cell2d[] =
        match layers with
        | Some layers -> layers.SampleWindow.GetAllSamples(layers.SampleExponent)
        | None -> Array.empty

    member this.GetAllSamplesInsideWindow (window : Box2l) : Cell2d[] =
        match layers with
        | Some layers ->
            invariant (layers.SampleWindow.Contains(window)) "f244101f-975c-4a0a-8c03-20e0618834b4"
            window.GetAllSamples(layers.SampleExponent)
        | None -> Array.empty

    member this.GetAllSamplesFromFirstMinusSecond (first : Box2l, second : Box2l) : Cell2d[] =
        match layers with
        | Some layers -> first.GetAllSamplesFromFirstMinusSecond(second, layers.SampleExponent)
        | None -> Array.empty

    member this.GetSample (p : V2d) : Cell2d =
        layers.Value.Mapping.GetSampleCell(p)

   
    

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

                    let layerSet = if layers.Length = 0 then None
                                   else Some(LayerSet(layers))

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

                    let n = QNode(id, exactBoundingBox, cell, splitLimitExp, layerSet, subNodes)
                    InMemoryNode n
                else
                    failwith "Loading quadtree failed. Invalid data. f1c2fcc6-68d2-47f3-80ff-f62b691a7b2e."

//and
//    QMergeNode = 


and

    QNodeRef =
    | NoNode
    | InMemoryNode of IQNode
    | OutOfCoreNode of Guid * (unit -> IQNode)
    with

        member this.ExactBoundingBox with get() =
            match this with
            | NoNode -> Box2d.Invalid
            | InMemoryNode n -> n.ExactBoundingBox
            | OutOfCoreNode (_, load) -> load().ExactBoundingBox

        member this.ContainsLayer (semantic : Durable.Def) =
            match this.TryGetInMemory() with
            | Some n -> match n.LayerSet with | None -> false | Some x -> x.ContainsLayer semantic
            | None   -> false

        /// Replace all occurences of 'oldSemantic' with 'newSemantic'.
        /// Returns (true, <newUpdatedOctree>) if 'oldSemantic' exists and is replaced.
        /// Returns (false, 'qtree') if 'oldSemantic' does not exist.
        /// Throws if quadtree contains both 'oldSemantic' and 'newSemantic'.
        member this.UpdateLayerSemantic (oldSemantic : Durable.Def, newSemantic : Durable.Def) : bool * QNodeRef =
            match this.TryGetInMemory() with
            | None   -> (false, NoNode)
            | Some n -> 
                match n.UpdateLayerSemantic(oldSemantic, newSemantic) with
                | None   -> (false, this)
                | Some x -> (true,  InMemoryNode x)

        /// Get referenced node (from memory or load out-of-core), or None if NoNode.
        member this.TryGetInMemory () : IQNode option =
            match this with
            | NoNode -> None
            | InMemoryNode n -> Some n
            | OutOfCoreNode (_, load) -> load() |> Some

        /// Forces property Id. Throws exception if NoNode.
        member this.Id with get() = match this.TryGetInMemory() with | Some n -> n.Id | None -> Guid.Empty

        /// Forces property Cell. Throws exception if NoNode.
        member this.Cell with get() = this.TryGetInMemory().Value.Cell


[<AutoOpen>]
module IQNodeExtensions =

    type IQNode with

        /// Throws if no such layer.
        member this.GetLayer def : ILayer = this.LayerSet.Value.GetLayer def

        /// Throws if no such layer.
        member this.GetLayer<'a> def : Layer<'a> = this.LayerSet.Value.GetLayer def :?> Layer<'a>

        member this.TryGetLayer (semantic : Durable.Def) : ILayer option =
            match this.LayerSet with | None -> None | Some x -> x.TryGetLayer(semantic)

        member this.TryGetLayer<'a> (semantic : Durable.Def) : Layer<'a> option =
            match this.LayerSet with | None -> None | Some x -> x.TryGetLayer<'a>(semantic)

        member this.IsLeafNode with get()  = this.SubNodes.IsNone
        member this.IsInnerNode with get() = this.SubNodes.IsSome

type QNodeRef with

    /// Throws if no such layer.
    member this.GetLayer def = this.TryGetInMemory().Value.GetLayer def

    /// Throws if no such layer.
    member this.GetLayer<'a> def = this.TryGetInMemory().Value.GetLayer<'a> def
    
    member this.TryGetLayer def = this.TryGetInMemory().Value.TryGetLayer def

    member this.TryGetLayer<'a> def = this.TryGetInMemory().Value.TryGetLayer<'a> def

module QNode =

    let toRef (n : QNode option) =
        match n with
        | Some n -> InMemoryNode n
        | None -> NoNode

    let tryGetInMemory (n : QNodeRef) = n.TryGetInMemory()

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
            let allLayers = subnodes |> Array.collect (fun x -> match x.LayerSet with | None -> Array.empty | Some x -> x.Layers) 

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

    