open System
open Aardvark.Base
open Aardvark.Geometry.Quadtree
open System.Diagnostics
open Aardvark.Data
open System.IO

let example () =

    // raw height data (4x3 samples stored in a flat array)
    let heights = [| 
        1.0; 1.0; 2.0; 2.0
        1.5; 1.6; 1.7; 1.8
        1.6; 1.7; 2.0; 2.2
        |]

    // raw color data
    let colors = [| 
        C3b(255,0,0); C3b(255,1,0); C3b(255,2,0); C3b(255,3,0)
        C3b(0,255,1); C3b(1,255,1); C3b(2,255,1); C3b(3,255,1)
        C3b(0,2,255); C3b(1,2,255); C3b(2,2,255); C3b(3,2,255)
        |]

    // define mapping of raw data to raster space
    let mapping = DataMapping(origin = Cell2d(500000L, 2000L, 0), size = V2i(4, 3))
    
    // a layer gives meaning to raw data
    let heightsLayer = Layer(Defs.Heights1d, heights, mapping)
    let colorLayer   = Layer(Defs.Colors3b , colors,  mapping)

    // build the quadtree (incl. levels-of-detail)
    let qtree = Quadtree.Build BuildConfig.Default [| heightsLayer; colorLayer |]
    printfn "%A" qtree.Cell

    // query
    let config = Query.Config.Default
    let line = Ray2d(origin = V2d(500_000, 2_000), direction = V2d(1,1))
    let withinDistance = 0.5
    let chunks = qtree |> Query.NearLine config line withinDistance

    let positions = // : V3d[]
        chunks 
        |> Seq.collect (fun chunk -> chunk.GetSamples<float> Defs.Heights1d)
        |> Seq.map (fun (cell, h) -> V3d(cell.GetCenter(), h))
        |> Array.ofSeq
    // positions = [|[500000.5, 2000.5, 1]; [500001.5, 2001.5, 1.6]; [500002.5, 2002.5, 2]|]
    printfn "%A" positions

    let colors = // : C3b[]
        chunks
        |> Seq.collect (fun chunk -> chunk.GetSamples<C3b> Defs.Colors3b)
        |> Seq.map snd
        |> Array.ofSeq
    // colors = [|[255, 0, 0]; [1, 255, 1]; [2, 2, 255]|]
    printfn "%A" colors

    ()

let buildQuadtree () =

    let size = V2i(15000,10000)
    printfn "building quadtree for %i x %i raster" size.X size.Y
    let sw = Stopwatch()
    sw.Start()

    let data = Array.zeroCreate<float32> (size.X * size.Y)
    let mapping = DataMapping(origin = V2l(500_000, 2_000), size = size, exponent = 0)
    let layer = Layer(Defs.Heights1f, data, mapping)
    let q = Quadtree.Build BuildConfig.Default [| layer |]

    sw.Stop()
    printfn "elapsed time: %A" sw.Elapsed
    printfn "%i nodes (%i leafs, %i inner)" (q |> Quadtree.CountNodes) (q |> Quadtree.CountLeafs) (q |> Quadtree.CountInner)

let test () =

    let samples =
        File.ReadLines(@"T:\Vgm\Data\Raster\kiga_002_ground_raster_1m.pts")
        |> Seq.skip 1
        |> Seq.map (fun line ->
            let ts = line.Split(' ')
            let xy = V2l(int64 (float ts.[0]), int64 (float ts.[1]))
            let h = float32 ts.[2]
            let n = V3f(float32 ts.[7], float32 ts.[8], float32 ts.[9])
            (xy, h, n)
            )
        |> Seq.toArray

    printfn "count:  %d" (samples |> Seq.length)

    let bb = Box2l(samples |> Seq.map (fun (xy, _, _) -> xy));
    let size = V2i(bb.Size) + V2i.II
    printfn "bb    : %A" bb
    printfn "origin: %A" bb.Min
    printfn "size  : %A" size
    printfn "area  : %d" (size.X * size.Y)

    let poly = Polygon2d([ V2d(1.5, 2.5); V2d(6.0, 2.5); V2d(6.0, -1.0); V2d(1.5, -1.0) ])
    let polyCcw = poly.Reversed

    let ps = samples |> Array.map (fun (xy, _, _) -> V2d(xy) + V2d(0.5, 0.5))
    let psInside = ps |> Array.filter poly.Contains
    printfn "points inside polygon  : %d" psInside.Length
    printfn "%A" psInside

    (* 
        
            mapping         {Aardvark.Geometry.Quadtree.DataMapping}    Aardvark.Geometry.Quadtree.DataMapping
        +   BoundingBox     {[[-119, 129], [89, 322]]}                  Aardvark.Base.Box2d
            Height          193                                         long
        +   Origin          {[-119, 129, 0]}                            Aardvark.Base.Cell2d
        +   Size            {[208, 193]}                                Aardvark.Base.V2l
            Width           208                                         long
        +   Window          {[[-119, 129], [89, 322]]}                  Aardvark.Base.Box2l
        +   bufferOrigin    {[-119, 129, 0]}                            Aardvark.Base.Cell2d
        +   bufferSize      {[208, 193]}                                Aardvark.Base.V2i
        +   window          {[[-119, 129], [89, 322]]}                  Aardvark.Base.Box2l
        
     *)

    
    let bufferOrigin = Cell2d(bb.Min, 0)
    let bufferSize = size
    let window = Box2l(bb.Min, bb.Min + V2l(size))

    let heights = Array.create (bufferSize.X * bufferSize.Y) nanf
    let normals = Array.create (bufferSize.X * bufferSize.Y) V3f.Zero
    for (xy,h,n) in samples do
        let xy = V2i(xy - bufferOrigin.XY)
        let i = xy.Y * bufferSize.X + xy.X
        heights.[i] <- h
        normals.[i] <- n

    // define mapping of raw data to raster
    let mapping = DataMapping(bufferOrigin, bufferSize, window)
    
    // a layer gives meaning to raw data
    let heightsLayer = Layer(Defs.Heights1f, heights, mapping)
    let normalsLayer = Layer(Defs.Normals3f, normals, mapping)

    // build the quadtree (incl. levels-of-detail)
    let qtree = Quadtree.Build BuildConfig.Default [| heightsLayer; normalsLayer |]
    printfn "qtree root cell: %A" qtree.Cell

    // query
    let config = Query.Config.Default
    let chunks = qtree |> Query.InsidePolygon config poly   // currently poly must be ccw (should not matter?)

    let positions = // : V3f[]
        chunks 
        |> Seq.collect (fun chunk -> chunk.GetSamples<float32> Defs.Heights1f)
        |> Seq.map (fun (cell, h) -> V3d(cell.GetCenter(), float h))
        |> Array.ofSeq

    printfn "query:"
    printfn "  count    : %A" positions.Length
    printfn "  positions: %A" positions

    ()

[<EntryPoint>]
let main argv =

    //example ()

    //buildQuadtree ()

    test ()

    0
