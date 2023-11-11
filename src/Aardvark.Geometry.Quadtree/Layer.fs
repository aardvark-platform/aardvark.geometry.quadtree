namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System
open System.Collections.Immutable
open System.Collections.Generic

#nowarn "44"

type ILayer =
    abstract member Def : Durable.Def
    abstract member Mapping : DataMapping
    abstract member WithWindow : Box2l -> ILayer option
    abstract member MapSingleCenteredSampleTo : Cell2d -> ILayer
    abstract member WithSemantic : Durable.Def -> ILayer
    abstract member Materialize : unit -> ILayer
    abstract member ToDurableMap : unit -> seq<KeyValuePair<Durable.Def, obj>>
    abstract member Mask : byte[] option

[<AutoOpen>]
module ILayerExtensions =
    type ILayer with

        member this.SampleExponent    with get() = this.Mapping.BufferOrigin.Exponent
        member this.SampleMin         with get() = Cell2d(this.Mapping.Window.Min, this.SampleExponent)
        member this.SampleMaxExcl     with get() = Cell2d(this.Mapping.Window.Max, this.SampleExponent)
        member this.SampleMaxIncl     with get() = Cell2d(this.Mapping.Window.Max - V2l.II, this.SampleExponent)
        member this.SampleWindow      with get() = this.Mapping.Window
        member this.BoundingBox       with get() = if this.Mapping.BufferOrigin.IsCenteredAtOrigin then
                                                     this.Mapping.BufferOrigin.BoundingBox
                                                   else
                                                     Box2d(this.SampleMin.BoundingBox, this.SampleMaxIncl.BoundingBox)

        member this.CellBounds        with get() = if this.Mapping.BufferOrigin.IsCenteredAtOrigin then
                                                     this.Mapping.BufferOrigin
                                                   else
                                                     Cell2d(this.BoundingBox)

        member this.SampleWindowAtChildLevel with get() =
                if this.Mapping.BufferOrigin.IsCenteredAtOrigin then
                    Box2l(-1L, -1L, +1L, +1L)
                else
                    let w = this.SampleWindow
                    Box2l(w.Min * 2L, w.Max * 2L)

type Layer<'a when 'a : equality>(def : Durable.Def, data : 'a[], mapping : DataMapping, mask : byte[] option) =
   
    do
        invariantm (data.Length >= mapping.WindowSize.X * mapping.WindowSize.Y) 
            (fun()->sprintf "Mapping window %A exceeds available data (length=%d)." mapping.Window data.Length)
            "971a69a6-cd25-43b8-b85b-7dd783456552"

    new(def : Durable.Def, data : 'a[], mapping : DataMapping) = Layer<'a>(def, data, mapping, None)

    interface ILayer with
        
        member this.Mask with get() = mask

        member this.Def with get() = def
        
        member this.Mapping with get() = mapping
        
        member this.WithWindow (w : Box2l) =
            mapping.WithWindow(w) 
            |> Option.map (fun m -> Layer(def, data, m, mask) :> ILayer)
        
        member this.MapSingleCenteredSampleTo (c : Cell2d) =
            invariant (mapping.BufferOrigin.IsCenteredAtOrigin) "1a276e80-0404-4e69-9777-acbac1a4ed6a"
            invariant (c.TouchesOrigin) "e2326309-7171-4b9b-841e-4f0a5ef028d7"
            invariant (data.Length = 1) "bf2b4330-67fd-4283-880f-f9c96dafee61"
            Layer(def, data, DataMapping(c, V2i.II), mask) :> ILayer
        
        member this.WithSemantic (newSemantic : Durable.Def) =
            Layer(newSemantic, data, mapping, mask) :> ILayer

        member this.Materialize () =
            if mapping.Window.Min = V2l.OO && mapping.WindowSize = mapping.BufferSize then
                this :> ILayer
            else
                let size = V2i(mapping.WindowSize)
                let newArrayLength = int mapping.Window.Area
                let newdata = Array.zeroCreate<'a>   newArrayLength
                let mutable i = 0
                for y = 0 to size.Y - 1 do
                    for x = 0 to size.X - 1 do
                        let c = Cell2d(mapping.Window.Min.X + int64 x, mapping.Window.Min.Y + int64 y, mapping.BufferOrigin.Exponent)
                        let s = this.GetSample(Fail, c)
                        newdata[i] <- s
                        i <- i + 1

                let newmask = 
                    match mask with
                    | None -> None
                    | Some mask ->
                        let mutable i = 0
                        let newmask = Array.zeroCreate<byte> newArrayLength
                        for y = 0 to size.Y - 1 do
                            for x = 0 to size.X - 1 do
                                newmask[i] <- mask[mapping.GetBufferIndex(x, y)]
                                i <- i + 1
                        Some newmask

                let m = DataMapping(Cell2d(mapping.Window.Min, mapping.BufferOrigin.Exponent), size)
                Layer<'a>(def, newdata, m, newmask) :> ILayer

        member this.ToDurableMap () = seq {
            yield kvp Defs.Layer.DefId def.Id
            yield kvp Defs.Layer.BufferOrigin mapping.BufferOrigin
            yield kvp Defs.Layer.BufferSize mapping.BufferSize
            yield kvp Defs.Layer.Window mapping.Window
            yield kvp def data
            match mask with | Some x ->yield kvp Defs.Mask1b x | None -> ()
            }

    member this.Def with get() = def
    member this.Mapping with get() = mapping
    member this.Data with get() = data
    member this.WithWindow = (this :> ILayer).WithWindow
    member this.CellBounds with get() = (this :> ILayer).CellBounds

    member this.GetSample (mode : BorderMode<'a>, c : Cell2d) : 'a =
    
        let min = this.SampleMin
        let maxIncl = this.SampleMaxIncl

        if c.Exponent > min.Exponent then failwith "Invariant 4270bb56-29a0-4267-82d3-aacffd7c5e88."
        let mutable s = c
        while s.Exponent < min.Exponent do s <- s.Parent

        let inline inside () = s.X >= min.X && s.Y >= min.Y && s.X <= maxIncl.X && s.Y <= maxIncl.Y

        match mode with
        | Fail ->
            if inside () then
                let i = mapping.GetBufferIndex s
                data.[i]
            else
                failwith <| sprintf "Position %A outside of available data [%A-%A]." s this.SampleMin this.SampleMaxIncl
        | ClampToBorder value ->
            if inside () then
                let i = mapping.GetBufferIndex s
                data.[i]
            else
                value
        | ClampToEdge ->
            let x = if s.X < min.X then min.X elif s.X > maxIncl.X then maxIncl.X else s.X
            let y = if s.Y < min.Y then min.Y elif s.Y > maxIncl.Y then maxIncl.Y else s.Y
            let i = mapping.GetBufferIndex(x, y)
            data.[i]

    member this.GetSample (mode : BorderMode<'a>, globalPos : V2d) : 'a =
        let s = mapping.GetSampleCell globalPos
        this.GetSample(mode, s)

module Layer =

    let private verbose = false

    type private M = ImmutableDictionary<Durable.Def, obj>

    let defineBuilder<'a when 'a : equality> def = (def, fun mapping (data : obj) mask -> Layer<'a>(def, data :?> 'a[], mapping, mask) :> ILayer)
    let private builders = Map.ofList [
        defineBuilder<float32>  Defs.Heights1f
        defineBuilder<float>    Defs.Heights1d
        defineBuilder<V4f>      Defs.HeightsBilinear4f
        defineBuilder<V4d>      Defs.HeightsBilinear4d

        defineBuilder<V3f>      Defs.Normals3f
        defineBuilder<V3d>      Defs.Normals3d

        defineBuilder<float32>  Defs.HeightStdDevs1f
        defineBuilder<float>    Defs.HeightStdDevs1d

        defineBuilder<C3b>      Defs.Colors3b
        defineBuilder<C4b>      Defs.Colors4b
        defineBuilder<C3f>      Defs.Colors3f
        defineBuilder<C4f>      Defs.Colors4f

        defineBuilder<int>      Defs.Intensities1i
        defineBuilder<int64>    Defs.Intensities1l
        defineBuilder<float32>  Defs.Intensities1f
        defineBuilder<float>    Defs.Intensities1d

        defineBuilder<V4f>      Defs.BilinearParams4f
        defineBuilder<V4d>      Defs.BilinearParams4d
        
        defineBuilder<float32>  Defs.Volumes1f
        defineBuilder<float>    Defs.Volumes1d
        defineBuilder<V4f>      Defs.VolumesBilinear4f
        defineBuilder<V4d>      Defs.VolumesBilinear4d
        ]
    
    let FromDurableMap (def : Durable.Def) (map : ImmutableDictionary<Durable.Def, obj>) : ILayer =

        let mapping = DataMapping(
                        map.[Defs.Layer.BufferOrigin] :?> Cell2d,
                        map.[Defs.Layer.BufferSize]   :?> V2i,
                        map.[Defs.Layer.Window]       :?> Box2l
                        )

        let mask =
            match map.TryGetValue(Defs.Mask1b) with
            | (false, _) -> None
            | (true, :? (byte[])) as (_, x) -> Some (x :?> byte[])
            | _ -> failwith "Internal error 30ae3cd8-c06a-4698-9db3-a6ef69706e6a."

        match builders |> Map.tryFind def with
        | Some builder -> builder mapping (map.[def]) mask
        | None -> sprintf "Unknown layer type %A. 8c90faa2-de10-4938-b8ee-3034bd9bdea0." def |> failwith

    let Deserialize (buffer : byte[]) : ILayer =
        
        let foo = Map.ofList [
            (Defs.Heights1d, fun (map : M) -> map.[Defs.Heights1d])
            ]

        let struct (def, o) = DurableCodec.Deserialize(buffer)
        let map = o :?> ImmutableDictionary<Durable.Def, obj>
        let mapping = DataMapping(
                        map.[Defs.Layer.BufferOrigin] :?> Cell2d,
                        map.[Defs.Layer.BufferSize]   :?> V2i,
                        map.[Defs.Layer.Window]       :?> Box2l
                        )

        let mask =
            match map.TryGetValue(Defs.Mask1b) with
            | (false, _) -> None
            | (true, :? (byte[])) as (_, x) -> Some (x :?> byte[])
            | _ -> failwith "Internal error 0be664f2-2bb5-4023-8821-7bfd371d1ac6."

        let data = map |> foo.[def]
        match data with
        | :? (int[])        -> Layer<int>    (def, data :?> int[]    , mapping, mask) :> ILayer
        | :? (int64[])      -> Layer<int64>  (def, data :?> int64[]  , mapping, mask) :> ILayer
        | :? (float[])      -> Layer<float>  (def, data :?> float[]  , mapping, mask) :> ILayer
        | :? (float32[])    -> Layer<float32>(def, data :?> float32[], mapping, mask) :> ILayer
        | :? (V2f[])        -> Layer<V2f>    (def, data :?> V2f[]    , mapping, mask) :> ILayer
        | :? (V2d[])        -> Layer<V2d>    (def, data :?> V2d[]    , mapping, mask) :> ILayer
        | :? (V3f[])        -> Layer<V3f>    (def, data :?> V3f[]    , mapping, mask) :> ILayer
        | :? (V3d[])        -> Layer<V3d>    (def, data :?> V3d[]    , mapping, mask) :> ILayer
        | :? (V4f[])        -> Layer<V4f>    (def, data :?> V4f[]    , mapping, mask) :> ILayer
        | :? (V4d[])        -> Layer<V4d>    (def, data :?> V4d[]    , mapping, mask) :> ILayer
        | :? (C3b[])        -> Layer<C3b>    (def, data :?> C3b[]    , mapping, mask) :> ILayer
        | :? (C4b[])        -> Layer<C4b>    (def, data :?> C4b[]    , mapping, mask) :> ILayer
        | :? (C3f[])        -> Layer<C3f>    (def, data :?> C3f[]    , mapping, mask) :> ILayer
        | :? (C4f[])        -> Layer<C4f>    (def, data :?> C4f[]    , mapping, mask) :> ILayer
        | _ -> failwith <| sprintf "Unknown type %A. Invariant 4e797062-04a2-445f-9725-79f66823aff8." (data.GetType())


    let BoundingBox (layer : ILayer) = layer.Mapping.BoundingBox

    let Window (layer : ILayer) = layer.Mapping.Window

    let private ensureSameDef (layers : Layer<_>[]) : Durable.Def =
        match layers.Length with
        | 0 -> failwith "Invariant 44926079-4dcf-48c5-b5ff-e8e214636f79."
        | 1 -> layers.[0].Def
        | _ -> 
            let def = layers.[0].Def
            invariant (layers |> Array.forall (fun l -> l.Def = def)) "49514f12-b4f9-4708-9108-94a6dcc5d217"
            def

    let private mergeTyped<'a when 'a : equality> (undefinedValue : 'a) (layers : Layer<'a>[]) : Layer<'a> =
        let def = ensureSameDef layers
        if verbose then
            printfn "[Layer.Merge] .... def = %s" def.Name
        
        let e = layers.[0].SampleExponent
        if not (layers |> Array.forall (fun l -> l.SampleExponent = e)) then 
            failwith "Cannot merge layers with different resolutions."
        if verbose then
            printfn "[Layer.Merge] .... e = %d" e
        
        let finalWindow = layers |> Seq.map (fun l -> l.SampleWindow) |> Box2l
        let finalOrigin = Cell2d(finalWindow.Min, e)
        let finalMapping = DataMapping(finalOrigin, V2i finalWindow.Size, finalWindow)
        let finalData = Array.zeroCreate<'a> (int finalWindow.Size.X * int finalWindow.Size.Y)
        let finalMask = Array.create<byte> (int finalWindow.Size.X * int finalWindow.Size.Y) 255uy

        if verbose then
            for l in layers do printfn "[Layer.Merge] .... [%A-%A]" l.SampleMin l.SampleMaxIncl
            printfn "[Layer.Merge] .... final mapping"
            printfn "[Layer.Merge] .... buffer origin: %A" finalOrigin
            printfn "[Layer.Merge] .... buffer size  : %A" finalWindow.Size
            printfn "[Layer.Merge] .... window: %A" finalWindow
        
        let mutable debugCountValues = 0
        let mutable debugCountCollisions = 0
        let debugCollisionSamples = HashSet<int>()

        let mutable layerIndex = 0uy
        for layer in layers do
            let w = layer.Mapping.Window
            let xMaxIncl = int w.SizeX - 1
            let yMaxIncl = int w.SizeY - 1
            for y = 0 to yMaxIncl do
                for x = 0 to xMaxIncl do
                    let c = Cell2d(w.Min.X + int64 x, w.Min.Y + int64 y, e)
                    let i = finalMapping.GetBufferIndex c
                    let v = layer.GetSample(Fail, c)
                    
                    //finalData.[i] <- v

                    match finalMask[i] with
                    | 255uy ->
                        finalData[i] <- v
                        finalMask[i] <- layerIndex
                        debugCountValues <- debugCountValues + 1
                    | _     ->
                        if v <> finalData[i] then
                            if finalData[i] = undefinedValue then
                                if v <> undefinedValue then
                                    finalData[i] <- v
                                    finalMask[i] <- layerIndex
                                else
                                    ()
                            else
                                if v <> undefinedValue then
                                    debugCollisionSamples.Add(i) |> ignore
                                    debugCountCollisions <- debugCountCollisions + 1
                                    
                                    if verbose then
                                        printfn "[DEBUG][Layer.mergeTyped] COLLISION overwriting value %A from layer %d with value %A from layer %d" finalData[i] finalMask[i] v layerIndex

                                else
                                    ()

                        


            layerIndex <- layerIndex + 1uy

        
        // count occupied samples
        let coundOccupiedSamples = finalMask |> Array.sumBy (fun x -> if x = 255uy then 1 else 0)

        if verbose && debugCountCollisions > 0 then
            printfn "[DEBUG][Layer.mergeTyped] debugCountValues = %d" debugCountValues
            printfn "[DEBUG][Layer.mergeTyped] debugCountCollisions = %d" debugCountCollisions
            printfn "[DEBUG][Layer.mergeTyped] debugCollisionSamples.Count = %d" debugCollisionSamples.Count
            printfn "[DEBUG][Layer.mergeTyped] debugCountOccupied.Count = %d / %d ... %5.2f" coundOccupiedSamples finalMask.Length (float coundOccupiedSamples / float finalMask.Length)

        // rewrite mask (1 ... occupied, 0 ... undefined)
        for i = 0 to finalMask.Length-1 do
            finalMask[i] <- if finalMask[i] = 255uy then 0uy else 1uy

        Layer(def, finalData, finalMapping, if coundOccupiedSamples > 0 then Some finalMask else None)

    let private toTyped<'a when 'a : equality> (layers : ILayer[]) : Layer<'a>[] =
        layers |> Array.map (fun x -> x :?> Layer<'a>)

    let private mergeUntyped_<'a when 'a : equality> (undefinedValue : 'a) xs : ILayer =
        xs |> toTyped<'a> |> (mergeTyped undefinedValue) :> ILayer
    
    /// Merge layers of same type (def).
    let Merge (layers : ILayer seq) =
        let ls = layers |> Array.ofSeq
        match ls.Length with
        | 0 ->
            if verbose then printfn "[Layer.Merge] 0 layers -> None"
            None
        | 1 ->
            if verbose then printfn "[Layer.Merge] 1 layer  -> Some ls.[0]"
            Some (ls |> Array.exactlyOne)
        | n ->
            let distinctDefCount = ls |> Seq.distinctBy (fun l -> l.Def) |> Seq.length
            if distinctDefCount <> 1 then failwith "Can only merge layers of same type (def). Error 68bf8529-e9c5-4b7b-a100-3e4850fc9c33."
            if verbose then printfn "[Layer.Merge] %d layers" n
            let mergedLayers =
                match ls.[0] with
                | :? Layer<int>     -> ls |> mergeUntyped_<int>     Int32.MinValue
                | :? Layer<int64>   -> ls |> mergeUntyped_<int64>   Int64.MinValue    
                | :? Layer<float>   -> ls |> mergeUntyped_<float>   nan
                | :? Layer<float32> -> ls |> mergeUntyped_<float32> nanf
                | :? Layer<V2f>     -> ls |> mergeUntyped_<V2f>     V2f.NaN
                | :? Layer<V2d>     -> ls |> mergeUntyped_<V2d>     V2d.NaN
                | :? Layer<V3f>     -> ls |> mergeUntyped_<V3f>     V3f.NaN
                | :? Layer<V3d>     -> ls |> mergeUntyped_<V3d>     V3d.NaN
                | :? Layer<V4f>     -> ls |> mergeUntyped_<V4f>     V4f.NaN
                | :? Layer<V4d>     -> ls |> mergeUntyped_<V4d>     V4d.NaN
                | :? Layer<C3b>     -> ls |> mergeUntyped_<C3b>     (C3b(0, 0, 0))
                | :? Layer<C4b>     -> ls |> mergeUntyped_<C4b>     (C4b(0, 0, 0, 0))
                | :? Layer<C3f>     -> ls |> mergeUntyped_<C3f>     (C3f(0, 0, 0))
                | :? Layer<C4f>     -> ls |> mergeUntyped_<C4f>     (C4f(0, 0, 0, 0))
                | _ -> failwith <| sprintf "Unsupported layer type %A. Invariant bfb8d2ec-666d-4878-b612-f46f59dd5e82." ls.[0]

            Some mergedLayers

type LayerSet(layers : ILayer[]) =

    do

        invariantm (layers.Length > 0) (fun()->"No layers.") "3532f669-8321-4f47-a39d-8c9920cb5f67"
        
        let sampleExponent = layers |> Seq.map (fun x -> x.SampleExponent) |> Seq.distinct |> Seq.tryExactlyOne
        invariantm (sampleExponent.IsSome) (fun()->"Layers have different sample exponents.") "975ddfbd-ae0d-4b54-bad6-cd054faeadf9"
        
        ()

    member this.Layers                   with get() = layers

    member this.BoundingBox              with get() = layers.[0].BoundingBox
    member this.Mapping                  with get() = layers.[0].Mapping
    member this.SampleExponent           with get() = layers.[0].Mapping.BufferOrigin.Exponent
    member this.SampleWindow             with get() = layers.[0].Mapping.Window
    member this.SampleWindowAtChildLevel with get() = layers.[0].SampleWindowAtChildLevel

    /// Returns 2.0 ^ SampleExponent.
    member this.SampleSize      with get() = 2.0 ** float(layers.[0].Mapping.BufferOrigin.Exponent)

    member this.TryGetLayer (semantic : Durable.Def) : ILayer option =
        layers |> Array.tryFind (fun x -> x.Def.Id = semantic.Id)

    member this.TryGetLayer<'a when 'a : equality> (semantic : Durable.Def) : Layer<'a> option =
        this.TryGetLayer(semantic) |> Option.map (fun x -> x :?> Layer<'a>)

    member this.GetLayer(semantic : Durable.Def) : ILayer =
        try
            layers |> Array.find    (fun x -> x.Def.Id = semantic.Id)
        with
        | _ -> sprintf "Failed to retrieve layer %A. Available layers are %A. Error 091474a1-d06b-408a-8609-85e0b272e645." semantic.Name (layers |> Array.map(fun x->x.Def.Name)) |> failwith

    member this.GetLayer<'a when 'a : equality>(semantic : Durable.Def) : Layer<'a> =
        this.GetLayer(semantic) :?> Layer<'a>

    member this.WithWindow (w : Box2l) : LayerSet option =
        let ols = layers |> Array.map(fun l -> l.WithWindow(w))
        if ols |> Array.forall(Option.isNone) then
            None
        else
            invariant (ols |> Array.forall(Option.isSome)) "4ea6ff83-03ed-4294-9f37-2833e6f949eb"
            let ls = ols |> Array.map Option.get
            Some(LayerSet(ls))

    member this.ContainsLayer (semantic : Durable.Def) : bool =
        this.TryGetLayer(semantic) |> Option.isSome

    /// Replace all occurences of 'oldSemantic' with 'newSemantic'.
    /// Returns 'Some <newUpdatedOctree>' if 'oldSemantic' exists and is replaced.
    /// Returns 'None' if 'oldSemantic' does not exist.
    /// Throws if quadtree contains both 'oldSemantic' and 'newSemantic'.
    member this.UpdateLayerSemantic (oldSemantic : Durable.Def, newSemantic : Durable.Def) : LayerSet option =
        match this.TryGetLayer(oldSemantic) with
        | None          -> None
        | Some oldLayer ->

            if this.ContainsLayer(newSemantic) then
                sprintf "Can't update layer semantic from %A to %A. This node already contains the target semantic." oldSemantic newSemantic
                |> failwith

            let id = Guid.NewGuid()

            let newLayers =
                let l = oldLayer.WithSemantic(newSemantic)
                layers |> Seq.filter (fun x -> x.Def.Id <> oldSemantic.Id) |> Seq.append [l] |> Seq.toArray

            LayerSet(newLayers) |> Some

    /// True, if at least one layer has a mask.
    member this.HasMask with get () = this.Layers |> Array.exists (fun x -> x.Mask.IsSome)

module LayerSet =

    /// Merges multiple layer sets.
    /// All layer sets must have identical layers (same number, same semantics, same order).
    let Merge (layerSets : LayerSet seq) : LayerSet =

        let layerSets = layerSets |> Array.ofSeq
        let layersPerLayerSet = layerSets[0].Layers.Length

        let indices = [0..layersPerLayerSet-1]

        let layers = 
            indices
            |> List.choose (fun i ->
                // merge the i-th layers from all layer sets
                layerSets |> Array.map (fun x -> x.Layers[i]) |> Layer.Merge
                )
            |> Array.ofList

        let result = LayerSet layers
        result