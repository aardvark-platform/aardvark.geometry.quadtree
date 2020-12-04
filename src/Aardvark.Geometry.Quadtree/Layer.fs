namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System.Collections.Immutable
open System.Collections.Generic

#nowarn "44"

type ILayer =
    abstract member Def : Durable.Def
    abstract member Mapping : DataMapping
    abstract member WithWindow : Box2l -> ILayer option
    abstract member MapSingleCenteredSampleTo : Cell2d -> ILayer
    abstract member WithSemantic : Durable.Def -> ILayer
    abstract member ResampleUntyped : Cell2d -> ILayer
    abstract member SupersampleUntyped : unit -> ILayer
    abstract member Materialize : unit -> ILayer
    abstract member ToDurableMap : unit -> seq<KeyValuePair<Durable.Def, obj>>

[<AutoOpen>]
module ILayerExtensions =
    type ILayer with
        /// Resolution (=size of a sample).
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

type Layer<'a>(def : Durable.Def, data : 'a[], mapping : DataMapping) =
   
    do
        invariantm (data.Length >= mapping.WindowSize.X * mapping.WindowSize.Y) 
            (sprintf "Mapping window %A exceeds available data (length=%d)." mapping.Window data.Length)
            "971a69a6-cd25-43b8-b85b-7dd783456552"

    interface ILayer with
        member this.Def with get() = def
        member this.Mapping with get() = mapping
        member this.WithWindow (w : Box2l) =
            mapping.WithWindow(w) 
            |> Option.map (fun m -> Layer(def, data, m) :> ILayer)
        member this.MapSingleCenteredSampleTo (c : Cell2d) =
            invariant (mapping.BufferOrigin.IsCenteredAtOrigin) "1a276e80-0404-4e69-9777-acbac1a4ed6a"
            invariant (c.TouchesOrigin) "e2326309-7171-4b9b-841e-4f0a5ef028d7"
            invariant (data.Length = 1) "bf2b4330-67fd-4283-880f-f9c96dafee61"
            Layer(def, data, DataMapping(c, V2i.II)) :> ILayer
        member this.WithSemantic (newSemantic : Durable.Def) =
            Layer(newSemantic, data, mapping) :> ILayer
        member this.ResampleUntyped (resampleRoot : Cell2d) =
            let f = Resamplers.getResamplerFor def 
            let r = this.Resample ClampToEdge (f :?> ('a*'a*'a*'a->'a)) resampleRoot
            r :> ILayer
        member this.SupersampleUntyped () = this.Supersample () :> ILayer

        member this.Materialize () =
            if mapping.Window.Min = V2l.OO && mapping.WindowSize = mapping.BufferSize then
                this :> ILayer
            else
                let size = V2i(mapping.WindowSize)
                let newdata = Array.zeroCreate<'a> (int mapping.Window.Area)
                let mutable i = 0
                for y = 0 to size.Y - 1 do
                    for x = 0 to size.X - 1 do
                        let c = Cell2d(mapping.Window.Min.X + int64 x, mapping.Window.Min.Y + int64 y, mapping.BufferOrigin.Exponent)
                        let s = this.GetSample(Fail, c)
                        newdata.[i] <- s
                        i <- i + 1

                let m = DataMapping(Cell2d(mapping.Window.Min, mapping.BufferOrigin.Exponent), size)
                Layer<'a>(def, newdata, m) :> ILayer

        member this.ToDurableMap () = seq {
            kvp Defs.Layer.DefId def.Id
            kvp Defs.Layer.BufferOrigin mapping.BufferOrigin
            kvp Defs.Layer.BufferSize mapping.BufferSize
            kvp Defs.Layer.Window mapping.Window
            kvp def data
            }

    member this.Def with get() = def
    member this.Mapping with get() = mapping
    member this.Data with get() = data
    member this.WithWindow = (this :> ILayer).WithWindow
    member this.CellBounds with get() = (this :> ILayer).CellBounds

    member this.GetSample (mode : BorderMode<'a>, c : Cell2d) : 'a =
    
        let min = this.SampleMin
        let maxIncl = this.SampleMaxIncl

        let s = if c.Exponent = min.Exponent then c
                elif c.Exponent + 1 = min.Exponent then c.Parent
                else failwith "Invariant 4d59a076-37ee-42d4-8326-2341be922a6c."

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

    member this.Resample (mode : BorderMode<'a>) (f : 'a*'a*'a*'a -> 'a) (resampleRoot : Cell2d) : Layer<'a> =

        if resampleRoot.IsCenteredAtOrigin && this.SampleExponent + 1 = resampleRoot.Exponent then

            let win = this.SampleWindow
            if win.Min.X >= -1L && win.Min.Y >= -1L && win.Max.X <= +1L && win.Max.Y <= +1L then
                let inline getSample x y = this.GetSample(mode, (Cell2d(int64 x, int64 y, this.SampleExponent)))
                let v = f (getSample -1 -1, getSample 0 -1, getSample -1 0, getSample 0 0)
                let buffer = Array.create 1 v
                let newMapping = DataMapping(resampleRoot) 
                Layer(def, buffer, newMapping)
            else
                failwith "Invariant 43144338-8f31-4336-be48-001cf4075ab0."

        else

            let min = this.SampleMin.Parent
            let maxIncl = this.SampleMaxIncl.Parent
            let inline getSample x y = Cell2d(min.X + int64 x, min.Y + int64 y, min.Exponent)

            let w = int(maxIncl.X - min.X)
            let h = int(maxIncl.Y - min.Y)

            let buffer = Array.zeroCreate ((w + 1) * (h + 1))

            let getSample (x : Cell2d) = this.GetSample(mode, x)
            let mutable i = 0
            for y = 0 to h do
                for x = 0 to w do
                    let s = Cell2d(min.X + int64 x, min.Y + int64 y, min.Exponent)
                    let xs = s.Children |> Array.map getSample
                    let v = f (xs.[0], xs.[1], xs.[2], xs.[3])
                    buffer.[i] <- v
                    i <- i + 1
                
            let newMapping = DataMapping(min, maxIncl)
            Layer(def, buffer, newMapping)

    member this.Supersample () : Layer<'a> =

        if mapping.BufferOrigin.IsCenteredAtOrigin then

            let s = data.[mapping.GetBufferIndex(mapping.BufferOrigin)]
            let newMapping = DataMapping(origin = V2l(-1,-1), size = V2i(2,2), exponent = mapping.BufferOrigin.Exponent - 1)
            Layer(def, [|s;s;s;s|], newMapping)

        else

            let size = mapping.WindowSize
            let w = size.X
            let h = size.Y
            let w2 = w * 2

            let buffer = Array.zeroCreate (w * h * 4)
            let newMapping = DataMapping(origin = mapping.Window.Min * 2L, size = size * 2, exponent = mapping.BufferOrigin.Exponent - 1)

            let o0 = mapping.Window.Min
            let o1 = newMapping.Window.Min
            for x = 0 to w-1 do
                for y = 0 to h-1 do
                    let i = mapping.GetBufferIndex(o0.X + int64 x, o0.Y + int64 y)
                    let s = data.[i]
                    let j = newMapping.GetBufferIndex(o1.X + int64(2*x), o1.Y + int64(2*y))
                    buffer.[j] <- s; buffer.[j+1] <- s
                    let j = newMapping.GetBufferIndex(o1.X + int64(2*x), o1.Y + int64(2*y+1))
                    buffer.[j] <- s; buffer.[j+1] <- s

            Layer(def, buffer, newMapping)
    


module Layer =

    let private verbose = false

    type private M = ImmutableDictionary<Durable.Def, obj>
    let private foo = Map.ofList [
        (Defs.Heights1d, fun (map : M) -> map.[Defs.Heights1d])
        ]

    let Deserialize (buffer : byte[]) : ILayer =
        let struct (def, o) = DurableCodec.Deserialize(buffer)
        let map = o :?> ImmutableDictionary<Durable.Def, obj>
        let mapping = DataMapping(
                        map.[Defs.Layer.BufferOrigin] :?> Cell2d,
                        map.[Defs.Layer.BufferSize]   :?> V2i,
                        map.[Defs.Layer.Window]       :?> Box2l
                        )
        let data = map |> foo.[def]
        match data with
        | :? (int[])        -> Layer<int>(def, data :?> int[], mapping) :> ILayer
        | :? (int64[])      -> Layer<int64>(def, data :?> int64[], mapping) :> ILayer
        | :? (float[])      -> Layer<float>(def, data :?> float[], mapping) :> ILayer
        | :? (float32[])    -> Layer<float32>(def, data :?> float32[], mapping) :> ILayer
        | :? (V2f[])        -> Layer<V2f>(def, data :?> V2f[], mapping) :> ILayer
        | :? (V2d[])        -> Layer<V2d>(def, data :?> V2d[], mapping) :> ILayer
        | :? (V3f[])        -> Layer<V3f>(def, data :?> V3f[], mapping) :> ILayer
        | :? (V3d[])        -> Layer<V3d>(def, data :?> V3d[], mapping) :> ILayer
        | :? (V4f[])        -> Layer<V4f>(def, data :?> V4f[], mapping) :> ILayer
        | :? (V4d[])        -> Layer<V4d>(def, data :?> V4d[], mapping) :> ILayer
        | :? (C3b[])        -> Layer<C3b>(def, data :?> C3b[], mapping) :> ILayer
        | :? (C4b[])        -> Layer<C4b>(def, data :?> C4b[], mapping) :> ILayer
        | :? (C3f[])        -> Layer<C3f>(def, data :?> C3f[], mapping) :> ILayer
        | :? (C4f[])        -> Layer<C4f>(def, data :?> C4f[], mapping) :> ILayer
        | _ -> failwith <| sprintf "Unknown type %A. Invariant 4e797062-04a2-445f-9725-79f66823aff8." (data.GetType())

    let defineBuilder<'a> def = (def, fun mapping (data : obj) -> Layer<'a>(def, data :?> 'a[], mapping) :> ILayer)
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

        match builders |> Map.tryFind def with
        | Some builder -> builder mapping (map.[def])
        | None -> sprintf "Unknown layer type %A. 8c90faa2-de10-4938-b8ee-3034bd9bdea0." def |> failwith

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

    let private mergeTyped (layers : Layer<'a>[]) : Layer<'a> =
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
        
        if verbose then
            for l in layers do printfn "[Layer.Merge] .... [%A-%A]" l.SampleMin l.SampleMaxIncl
            printfn "[Layer.Merge] .... final mapping"
            printfn "[Layer.Merge] .... buffer origin: %A" finalOrigin
            printfn "[Layer.Merge] .... buffer size  : %A" finalWindow.Size
            printfn "[Layer.Merge] .... window: %A" finalWindow
        
        
        for layer in layers do
            let w = layer.Mapping.Window
            let xMaxIncl = int w.SizeX - 1
            let yMaxIncl = int w.SizeY - 1
            for y = 0 to yMaxIncl do
                for x = 0 to xMaxIncl do
                    let c = Cell2d(w.Min.X + int64 x, w.Min.Y + int64 y, e)
                    let i = finalMapping.GetBufferIndex c
                    let v = layer.GetSample(Fail, c)
                    finalData.[i] <- v
        
        Layer(def, finalData, finalMapping)

    let private toTyped<'a> (layers : ILayer[]) : Layer<'a>[] =
        layers |> Array.map (fun x -> x :?> Layer<'a>)

    let private mergeUntyped_<'a> xs = xs |> toTyped<'a> |> mergeTyped :> ILayer
    
    /// Merge layers of same type (def).
    let Merge (layers : ILayer seq) : ILayer option =
        let ls = layers |> Array.ofSeq
        match ls.Length with
        | 0 -> 
            if verbose then printfn "[Layer.Merge] 0 layers -> None"
            None
        | 1 ->
            if verbose then printfn "[Layer.Merge] 1 layer  -> Some ls.[0]"
            Some ls.[0]
        | n ->
            let distinctDefCount = ls |> Seq.distinctBy (fun l -> l.Def) |> Seq.length
            if distinctDefCount <> 1 then failwith "Can only merge layers of same type (def). Error 68bf8529-e9c5-4b7b-a100-3e4850fc9c33."
            if verbose then printfn "[Layer.Merge] %d layers" n
            match ls.[0] with
            | :? Layer<int>     -> ls |> mergeUntyped_<int>
            | :? Layer<int64>   -> ls |> mergeUntyped_<int64>
            | :? Layer<float>   -> ls |> mergeUntyped_<float>
            | :? Layer<float32> -> ls |> mergeUntyped_<float32>
            | :? Layer<V2f>     -> ls |> mergeUntyped_<V2f>
            | :? Layer<V2d>     -> ls |> mergeUntyped_<V2d>
            | :? Layer<V3f>     -> ls |> mergeUntyped_<V3f>
            | :? Layer<V3d>     -> ls |> mergeUntyped_<V3d>
            | :? Layer<V4f>     -> ls |> mergeUntyped_<V4f>
            | :? Layer<V4d>     -> ls |> mergeUntyped_<V4d>
            | :? Layer<C3b>     -> ls |> mergeUntyped_<C3b>
            | :? Layer<C4b>     -> ls |> mergeUntyped_<C4b>
            | :? Layer<C3f>     -> ls |> mergeUntyped_<C3f>
            | :? Layer<C4f>     -> ls |> mergeUntyped_<C4f>
            | _ -> failwith <| sprintf "Unsupported layer type %A. Invariant bfb8d2ec-666d-4878-b612-f46f59dd5e82." ls.[0]

            |> Some
