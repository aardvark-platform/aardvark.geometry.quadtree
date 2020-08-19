namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System.Collections.Immutable
open System.Collections.Generic

type ILayer =
    abstract member Def : Durable.Def
    abstract member Mapping : DataMapping
    abstract member WithWindow : Box2l -> ILayer option
    abstract member ResampleUntyped : Cell2d -> ILayer
    abstract member Materialize : unit -> ILayer
    abstract member ToDurableMap : unit -> seq<KeyValuePair<Durable.Def, obj>>

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

type Layer<'a>(def : Durable.Def, data : 'a[], mapping : DataMapping) =
   
    interface ILayer with
        member this.Def with get() = def
        member this.Mapping with get() = mapping
        member this.WithWindow (w : Box2l) =
            mapping.WithWindow(w) 
            |> Option.map (fun m -> Layer(def, data, m) :> ILayer)
        member this.ResampleUntyped (resampleRoot : Cell2d) =
            let f = Resamplers.getResamplerFor def 
            let r = this.Resample ClampToEdge (f :?> ('a*'a*'a*'a->'a)) resampleRoot
            r :> ILayer

        member this.Materialize () =
            if mapping.Window.Min = V2l.OO && mapping.WindowSize = V2l(mapping.BufferSize) then
                this :> ILayer
            else
                let size = V2i(mapping.WindowSize)
                let newdata = Array.zeroCreate<'a> (int mapping.Window.Area)
                let mutable i = 0
                for y = 0 to size.Y - 1 do
                    for x = 0 to size.X - 1 do
                        let c = Cell2d(mapping.Window.Min.X + int64 x, mapping.Window.Min.Y + int64 y, mapping.BufferOrigin.Exponent)
                        let s = this.GetSample Fail c
                        newdata.[i] <- s
                        i <- i + 1

                let m = DataMapping(Cell2d(mapping.Window.Min, mapping.BufferOrigin.Exponent), size)
                Layer<'a>(def, newdata, m) :> ILayer

        member this.ToDurableMap () = seq {
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

    member this.GetSample (mode : BorderMode<'a>) (s : Cell2d) : 'a =
        let min = this.SampleMin
        let maxIncl = this.SampleMaxIncl
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

    member this.Resample (mode : BorderMode<'a>) (f : 'a*'a*'a*'a -> 'a) (resampleRoot : Cell2d) : Layer<'a> =

        if resampleRoot.IsCenteredAtOrigin && this.SampleExponent + 1 = resampleRoot.Exponent then

            failwith "Resample to centered cell not implemented."

        else

            let min = this.SampleMin.Parent
            let maxIncl = this.SampleMaxIncl.Parent
            let inline getSample x y = Cell2d(min.X + int64 x, min.Y + int64 y, min.Exponent)

            let w = int(maxIncl.X - min.X)
            let h = int(maxIncl.Y - min.Y)

            let buffer = Array.zeroCreate ((w + 1) * (h + 1))

            let getSample = this.GetSample mode
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


    let BoundingBox (layer : ILayer) = layer.Mapping.BoundingBox

    let Window (layer : ILayer) = layer.Mapping.Window

    let private ensureSameDef (layers : Layer<_>[]) : Durable.Def =
        match layers.Length with
        | 0 -> failwith "Invariant 44926079-4dcf-48c5-b5ff-e8e214636f79."
        | 1 -> layers.[0].Def
        | _ -> 
            let def = layers.[0].Def
            if not (layers |> Array.forall (fun l -> l.Def = def)) then 
                failwith "Invariant 49514f12-b4f9-4708-9108-94a6dcc5d217."
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
                    let v = layer.GetSample Fail c
                    finalData.[i] <- v
        
        Layer(def, finalData, finalMapping)

    let private toTyped<'a> (layers : ILayer[]) : Layer<'a>[] =
        layers |> Array.map (fun x -> x :?> Layer<'a>)

    let private mergeUntyped_<'a> xs = xs |> toTyped<'a> |> mergeTyped :> ILayer
    
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
