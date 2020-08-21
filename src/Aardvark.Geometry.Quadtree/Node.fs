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

    static member private inMemoryStoreCount : int = 0

    member this.WithNewInMemoryStore (verbose : bool) =
        let store = Dictionary<Guid, byte[]>()
        let storeNumber = Interlocked.Increment(ref SerializationOptions.inMemoryStoreCount)
        { this with
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

    member this.WithNewInMemoryStore () = this.WithNewInMemoryStore(verbose = false)

type INode =
    abstract member Id : Guid
    /// Cell occupied by this node.
    abstract member Cell : Cell2d
    abstract member SplitLimitExponent : int
    abstract member OriginalSampleExponent : int
    abstract member Layers : ILayer[]
    abstract member SampleWindow : Box2l
    abstract member SampleExponent : int
    abstract member SubNodes : INode option[] option
    abstract member WithLayers : ILayer[] -> INode
    abstract member GetLayer<'a> : Durable.Def -> Layer<'a>
    abstract member Save : SerializationOptions -> Guid

[<AutoOpen>]
module INodeExtensions = 
    type INode with
        member this.IsInnerNode with get() = this.SubNodes.IsSome
        member this.IsLeafNode  with get() = this.SubNodes.IsNone
        member this.SampleWindowBoundingBox with get() = this.Layers.[0].BoundingBox
        member this.AllSamples with get() =
            let e = this.Layers.[0].SampleExponent
            let w = this.Layers.[0].SampleWindow
            let xMaxIncl = int w.Size.X - 1
            let yMaxIncl = int w.Size.Y - 1

            let samples = Array.zeroCreate (int w.Size.X * int w.Size.Y)
            let mutable i = 0
            for y = 0 to yMaxIncl do
                for x = 0 to xMaxIncl do
                    samples.[i] <- Cell2d(w.Min.X + int64 x, w.Min.Y + int64 y, e)
                    i <- i + 1
            samples

type Node(id : Guid, cell : Cell2d, splitLimitExp : int, originalSampleExponent : int, layers : ILayer[], subNodes : INode option[] option) =

    do
        if layers.Length = 0 then
            failwith "No layers. Invariant fe0e56d7-9bc2-4f61-8b36-0ed7fcc4bc56."

        if cell.Exponent - splitLimitExp <> layers.[0].SampleExponent then
            failwith "Sample exponent does not match split limit and node cell. Invariant 1ec76eaa-534b-4339-b63b-8e0399562bb1."

        let w = layers.[0].SampleWindow
        let e = layers.[0].SampleExponent
        let bb = cell.BoundingBox
        for layer in layers do
            if layer.SampleExponent <> e then failwith "Layers exponent mismatch."
            if layer.SampleWindow <> w then failwith "Layers window mismatch."
            if not(bb.Contains(layer.Mapping.BoundingBox)) then 
                invalidArg "layers" (sprintf "Layer %A is outside node bounds." layer.Def.Id)
            
        match subNodes with 
        | Some subNodes ->
            invariant (subNodes.Length = 4) "20baf723-cf32-46a6-9729-3b4e062ceee5."
            let children = cell.Children
            for i = 0 to 3 do
                let sn = subNodes.[i]
                match sn with 
                | None -> () 
                | Some x ->
                    invariant (x.Cell = children.[i]) "6243dc09-fae4-47d5-8ea7-834c3265988b."
                    invariant (cell.Exponent = x.Cell.Exponent + 1) "780d98cc-ecab-43fc-b492-229fb0e208a3."
         | None -> ()

    new (cell : Cell2d, splitLimitExp : int, originalSampleExponent : int, layers : ILayer[]) =
        Node(Guid.NewGuid(), cell, splitLimitExp, originalSampleExponent, layers, None)

    interface INode with
        member _.Id with get() = id
        member _.Cell with get() = cell
        member _.SplitLimitExponent with get() = splitLimitExp
        member _.OriginalSampleExponent with get() = originalSampleExponent
        member _.Layers with get() = layers
        member _.SampleWindow with get() = layers.[0].SampleWindow
        member _.SampleExponent with get() = layers.[0].SampleExponent
        member _.SubNodes with get() = subNodes
        member _.WithLayers (newLayers : ILayer[]) = Node(Guid.NewGuid(), cell, splitLimitExp, originalSampleExponent, newLayers, subNodes) :> INode
        member this.GetLayer<'a>(def : Durable.Def) : Layer<'a> =
            layers |> Array.find (fun x -> x.Def.Id = def.Id) :?> Layer<'a>
        member this.Save options =
            let map = List<KeyValuePair<Durable.Def, obj>>()

            map.Add(kvp Defs.NodeId id)
            map.Add(kvp Defs.CellBounds cell)
            map.Add(kvp Defs.SplitLimitExponent splitLimitExp)
            map.Add(kvp Defs.OriginalSampleExponent originalSampleExponent)

            match subNodes with
            | Some xs ->
                for x in xs do 
                    match x with 
                    | Some x -> x.Save options |> ignore  
                    | None -> ()
                let ids = xs |> Array.map (fun x -> match x with | Some x -> x.Id | None -> Guid.Empty)
                map.Add(kvp Defs.SubnodeIds ids)
            | None -> ()

            for layer in layers do
                let layerDef = Defs.GetLayerFromDef layer.Def
                let dm = layer.Materialize().ToDurableMap ()
                map.Add(kvp layerDef dm)

            let buffer = DurableCodec.Serialize(Defs.Node, map)
            options.Save id buffer
            id

    static member Load (options: SerializationOptions) (id : Guid) : INode option =
        if id = Guid.Empty then
            None
        else
            match options.TryLoad id with
            | None -> None
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
                            let xs = keys |> Array.map (Node.Load options)
                            Some xs

                    let n = Node(id, cell, splitLimitExp, originalSampleExp, layers, subNodes) :> INode
                    Some n
                else
                    failwith "Loading quadtree failed. Invalid data. f1c2fcc6-68d2-47f3-80ff-f62b691a7b2e."

module Node =

    let internal GenerateLodLayers (subNodes : INode option[]) (rootCell : Cell2d) =

        let subNodes = subNodes |> Array.choose id
        invariant (subNodes.Length > 0) "Invariant 641ef4b4-65b3-4e76-bbb6-c7046452801a."
        
        let minSubNodeExponent = (subNodes |> Array.minBy (fun x -> x.SampleExponent)).SampleExponent
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
        invariant (selfExponent = maxSubNodeExponent + 1) "Invariant 4a8cbec0-4e14-4eb4-a21a-0af370cc1d81."

        result
