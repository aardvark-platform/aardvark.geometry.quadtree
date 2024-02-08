open System
open Aardvark.Base
open Aardvark.Geometry.Quadtree
open System.Diagnostics
open System.IO
open System.Globalization
open System.Collections.Generic

#nowarn "3560"
#nowarn "0044"

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
    printfn "%A" (qtree.Cell)

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
    printfn "%i nodes (%i leafs, %i inner)" (q |> Quadtree.CountNodes true) (q |> Quadtree.CountLeafs true) (q |> Quadtree.CountInner true)

let parsePts filename (e : int) =
    let f = 1.0 / Math.Pow(2.0, float e)
    File.ReadLines(filename)
    |> Seq.skip 1
    |> Seq.map (fun line ->
        let ts = line.Split(' ')
        let xy = V2d(float ts.[0], float ts.[1])
        let xy' = V2l(f * xy)
        let h = float32 ts.[2]
        let n = V3f(float32 ts.[7], float32 ts.[8], float32 ts.[9])
        (xy, xy', h, n)
        )
    |> Seq.toArray

let parsePolygon filename =
    File.ReadLines(filename)
    |> Seq.filter (fun line -> not (String.IsNullOrWhiteSpace(line)))
    |> Seq.map (fun line ->
        let ts = line.Split(',')
        let x = Double.Parse(ts.[0])
        let y = Double.Parse(ts.[1])
        V2d(x, y)
        )
    |> Polygon2d

let import filename (e : int) (splitLimitPowerOfTwo : int) (verbose : bool) =

    if verbose then
        Report.BeginTimed "importing"
        printfn "  filename: %s" filename
        printfn "  exponent: %d" e

    let samples = parsePts filename e
    if verbose then printfn "  samples  : %d" samples.Length

    let cells = samples |> Array.map (fun (_, xy, _, _) -> Cell2d(xy, e))

    let ps = samples |> Array.map (fun (p, _, _, _) -> p)
    let psBounds = Box2d(ps)
    if verbose then
        printfn "  psBounds : %A" psBounds
        printfn "             %A" psBounds.Size

    let ps' = samples |> Array.map (fun (_, p, _, _) -> p)
    let bb  = Box2l(ps')
    if verbose then
        printfn "  psBounds': %A" bb
        printfn "             %A" bb.Size

    let size = V2i(bb.Size) + V2i.II
    let bufferOrigin = Cell2d(bb.Min,-1)
    let bufferSize = size
    let window = Box2l(bb.Min, bb.Min + V2l(size))

    let heights = Array.create (bufferSize.X * bufferSize.Y) nanf
    let normals = Array.create (bufferSize.X * bufferSize.Y) V3f.NaN

    for (_, xy,h,n) in samples do
        let xy = V2i(xy - bufferOrigin.XY)
        let i = xy.Y * bufferSize.X + xy.X
        heights.[i] <- h
        normals.[i] <- n

    // define mapping of raw data to raster
    let mapping = DataMapping(bufferOrigin, bufferSize)
    
    // a layer gives meaning to raw data
    let heightsLayer = Layer(Defs.Heights1f, heights, mapping)
    let normalsLayer = Layer(Defs.Normals3f, normals, mapping)

    // build the quadtree (incl. levels-of-detail)
    let config = { BuildConfig.Default with SplitLimitPowerOfTwo = splitLimitPowerOfTwo }
    let qtree = Quadtree.Build config [| heightsLayer; normalsLayer |]
    
    if verbose then
        printfn "  created quadtree: %A" qtree.Cell
        Report.EndTimed () |> ignore

    (qtree, cells)

let test () =

    // parse pts file
    let samples = 
        //parsePts @"T:\Vgm\Data\Raster\kiga_002_ground_raster_1m.pts"
        parsePts @"T:\Vgm\Data\Raster\kiga_0_5.pts" 0

    let bb = Box2l(samples |> Seq.map (fun (_, xy, _, _) -> xy));
    let size = V2i(bb.Size) + V2i.II
    printfn "pts data: %A" bb
    printfn "  count : %d" (samples |> Seq.length)
    printfn "  bb    : %A" bb
    printfn "  origin: %A" bb.Min
    printfn "  size  : %A" size
    printfn "  area  : %d" (size.X * size.Y)

    let poly = Polygon2d([ V2d(1.5, 2.5); V2d(6.0, 2.5); V2d(6.0, -1.0); V2d(1.5, -1.0) ])

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

    
    let bufferOrigin = Cell2d(bb.Min,-1)
    let bufferOrigin = Cell2d(-237L, -127L, -1)
    let bufferSize = size
    let bufferSize = V2i(414, 384)
    let window = Box2l(bb.Min, bb.Min + V2l(size))

    let heights = Array.create (bufferSize.X * bufferSize.Y) nanf
    let normals = Array.create (bufferSize.X * bufferSize.Y) V3f.Zero
    for (_,xy,h,n) in samples do
        let xy = V2i(xy - bufferOrigin.XY)
        let i = xy.Y * bufferSize.X + xy.X
        heights.[i] <- h
        normals.[i] <- n

    // define mapping of raw data to raster
    let mapping = DataMapping(bufferOrigin, bufferSize)
    
    // a layer gives meaning to raw data
    let heightsLayer = Layer(Defs.Heights1f, heights, mapping)
    let normalsLayer = Layer(Defs.Normals3f, normals, mapping)

    // build the quadtree (incl. levels-of-detail)
    let qtree = Quadtree.Build BuildConfig.Default [| heightsLayer; normalsLayer |]
    printfn "qtree root cell: %A" (qtree.Cell)

    // query
    let config = { Query.Config.Default with Query.SampleMode = Query.Center }
    let chunks = qtree |> Query.InsidePolygon config poly   // currently poly must be ccw (should not matter?)

    let positions =
        chunks 
        |> Seq.collect (fun chunk -> chunk.GetSamples<float32> Defs.Heights1f)
        |> Seq.map (fun (cell, h) -> V3d(cell.GetCenter(), float h))
        |> Array.ofSeq

    let normals =
        chunks 
        |> Seq.collect (fun chunk -> chunk.GetSamples<V3f> Defs.Normals3f)
        |> Seq.map (fun (cell, n) -> n)
        |> Array.ofSeq

    printfn "query:"
    printfn "  count    : %A" positions.Length
    printfn "  positions: %A" positions
    printfn "  normals  : %A" normals

    ()

let performanceTest () =

    let createQuadtreeWithRandomValues (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int) =
        let r = Random()
        let size = V2i(w, h)
        let xs = Array.zeroCreate<float32> (w * h)
        for y = 0 to size.Y - 1 do
            for x = 0 to size.X - 1 do
                let i = y * size.X + x
                xs.[i] <- -100.0f + float32(r.NextDouble()) * 200.0f
    
        let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))
    
        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = int splitLimit }
        Quadtree.Build config [| a |]

    let merge_Random_Centered_SplitLimit1 dominance =

        let mutable quadtree = createQuadtreeWithRandomValues 0 0 1 1 0 0

        let r = Random(0)
        for i = 1 to 50 do
            let e = r.Next(20) - 10
            let ox = if e >= 0 then (r.Next(2000) - 1000) >>> e else (r.Next(2000) - 1000) <<< -e
            let oy = if e >= 0 then (r.Next(2000) - 1000) >>> e else (r.Next(2000) - 1000) <<< -e
            let w  = r.Next(50) + 1
            let h  = r.Next(50) + 1

            let other = createQuadtreeWithRandomValues ox oy w h e 0
            let merged = Quadtree.Merge dominance quadtree other
            quadtree <- merged

        ()

    merge_Random_Centered_SplitLimit1 FirstDominates

    ()

let cpunz20200829 () =

    // define mapping of raw data to raster space
    let hor1 = V4f(1.0, 0.0,0.0,0.0)
    let hor2 = V4f(2.0, 0.0,0.0,0.0)
    let oblique12 = V4f(1.5, 1.0,0.0,0.0)
    
    let parameters = [|hor1; oblique12; hor2; 
                       hor1; oblique12; hor2;
                       hor1; oblique12; hor2;
                       hor1; oblique12; hor2|]

    let mapping = DataMapping(origin = Cell2d(0L, 0L, 0), size = V2i(3, 4))

    // a layer gives meaning to raw data
    let bilinParameters = Layer(Defs.HeightsBilinear4f, parameters, mapping)
    
    // build the quadtree (incl. levels-of-detail)
    
    let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]

    let polygon = Polygon2d([|V2d(0.0,0.0); V2d(2.0,2.0); V2d(3.0,2.0); V2d(3.0,0.0); V2d(0.0,0.0)|])
    let result0 = qtree |> Query.InsidePolygon Query.Config.Default polygon |> Seq.collect (fun chunk -> chunk.GetSamples<V4f> Defs.HeightsBilinear4f) |> Seq.toArray

    //let result1 = qtree |> Query.All Query.Config.Default |> Seq.collect (fun chunk -> chunk.GetSamples<V4f> Defs.BilinearParams4f) |> Seq.toArray
    //printfn "Query.Polygon: length = %d" result1.Length

    ()

let isOnBorder (poly : Polygon2d) p maxDist =
    poly.EdgeLines |> Seq.exists (fun e -> e.IsDistanceToPointSmallerThan(p, maxDist))

let containsApprox (poly : Polygon2d) p maxDist =
    poly.Contains(p) || isOnBorder poly p maxDist

let polyTest () =
    let poly = Polygon2d(V2d.OO, V2d.IO, V2d.II, V2d.OI)
    
    let polyCcw = if poly.IsCcw() then poly else poly.Reversed
    
    poly.Contains       (V2d(0.5, 0.5)) |> printfn "%A"
    isOnBorder poly     (V2d(0.5, 0.5)) 0.00000001 |> printfn "%A"
    containsApprox poly (V2d(0.5, 0.5)) 0.00000001 |> printfn "%A"
    printfn ""
    
    poly.Contains       (V2d(0.5, 0.0)) |> printfn "%A"
    isOnBorder poly     (V2d(0.5, 0.0)) 0.00000001 |> printfn "%A"
    containsApprox poly (V2d(0.5, 0.0)) 0.00000001 |> printfn "%A"
    printfn ""
    
    poly.Contains       (V2d(0.5, -0.00000001)) |> printfn "%A"
    isOnBorder poly     (V2d(0.5, -0.00000001)) 0.00000001 |> printfn "%A"
    containsApprox poly (V2d(0.5, -0.00000001)) 0.00000001 |> printfn "%A"
    printfn ""
    
    poly.Contains       (V2d(0.5, -0.000000011)) |> printfn "%A"
    isOnBorder poly     (V2d(0.5, -0.000000011)) 0.00000001 |> printfn "%A"
    containsApprox poly (V2d(0.5, -0.000000011)) 0.00000001 |> printfn "%A"

let cpunz20200923 () =

    let hor = V4f(0.0, 0.0,0.0,0.0) 
    
    let parameters = [|hor|]

    let mapping = DataMapping(origin = Cell2d(0L, 0L, 2), size = V2i(1, 1))

    // a layer gives meaning to raw data
    let bilinParameters = Layer(Defs.HeightsBilinear4f, parameters, mapping)
    
    // build the quadtree (incl. levels-of-detail)
    
    let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]

    let r = Query.IntersectsCell Query.Config.Default (Cell2d(0,0,0)) qtree |> Array.ofSeq

    ()

/// Query.IntersectsCell speed too slow (seen for many queries) ...
let cpunz20200925 () =

    CultureInfo.DefaultThreadCurrentCulture <- CultureInfo.InvariantCulture

    for e = 3 to 10 do

        printfn "---------- e = %d --------------" e

        let (q0, cells0) = import @"T:\Vgm\Data\Raster\20200925_cpunz\epoche_deponie_Bodenpunktraster1_0,50.pts" -1 e false
        let (q1, cells1) = import @"T:\Vgm\Data\Raster\20200925_cpunz\epoche_deponie_Bodenpunktraster1_0,50_1.pts" -1 e false

        let polygon = parsePolygon @"T:\Vgm\Data\Raster\20200925_cpunz\polygon_volumen.txt"

        let makeReturnValOfQueryResults (resultChunk : seq<Query.Result>) =
        
            let debug = resultChunk |> Seq.toArray
            //printfn "[makeReturnValOfQueryResults] %d" debug.Length

            let posAndParams = // : V4f[]
                debug       
                |> Seq.collect (fun chunk -> chunk.GetSamples<V3f> Defs.Normals3f)        
                |> Seq.map (fun (cell, normal) -> 
                    //let bilParam = {| b0 = (float)bilParamV.X; b1 = (float)bilParamV.Y; b2 = (float)bilParamV.Z; b3 = (float)bilParamV.W |}
                    (normal, cell))   
            
            posAndParams |> Seq.filter (fun pos -> (fst pos).X.IsNaN() |> not)
                         |> Seq.map (fun pos -> (Some (snd pos), Some (fst pos)) )


        let mutable count = 0
        let queryQuadtreeCellCounterParts (qtree : QNodeRef) (cellForQuery : Cell2d) =
            count <- count + 1
            //if count % 1000 = 0 then printfn "[progress] %d" count
            let config = Query.Config.Default
            let result = qtree |> Query.IntersectsCell config cellForQuery |> makeReturnValOfQueryResults
            result

        let dtmCellsZeroBased = 
            Query.InsidePolygon Query.Config.Default polygon q0
            |> Seq.collect (fun chunk -> chunk.GetSamples<V3f> Defs.Normals3f)
            |> Seq.map fst
            |> Seq.toArray

        //printfn "cells count (polygon): %d" dtmCellsZeroBased.Length

        //printfn "cells count (q0)     : %d" cells0.Length
        //printfn "cells count (q1)     : %d" cells1.Length
        Report.BeginTimed("compute")

        let newDifferenceModel = 
            //cells0
            dtmCellsZeroBased
            //|> Seq.take 1
            |> Seq.map (fun oneElem -> 
                let cellEnum = queryQuadtreeCellCounterParts q1 oneElem |> Seq.toArray
                //printfn "[newDifferenceModel] %d" cellEnum.Length
                cellEnum
                )
            //|> Seq.take 100
            |> Seq.toArray

        Report.EndTimed() |> ignore

        let foo = newDifferenceModel |> Array.map (fun x -> (fst x.[0]).Value) |> Array.sortBy (fun x -> (x.X, x.Y, x.Exponent))
    
        //printfn "q0 node count               %d" (q0 |> Quadtree.CountNodes)
        //printfn "q1 node count               %d" (q1 |> Quadtree.CountNodes)
        printfn "newDifferenceModel.Length = %d" newDifferenceModel.Length

        ()

let subtractionEnumerationTest () =
    let a = Box2l(0L, 0L, 3L, 3L)
    let aSamples = a.GetAllSamples(0)
    printfn "%A" aSamples

    let b = Box2l(1L, 0L, 2L, 3L)
    let bSamples = b.GetAllSamples(0)
    printfn "%A" bSamples

    let result = a.GetAllSamplesFromFirstMinusSecond(b, 0)
    printfn "%A" result

let cpunz20201105 () =

    let createQuadTreePlanes =

        // define mapping of raw data to raster space
        let hor1 = V4f(1.0, 0.0,0.0,0.0)
        let hor2 = V4f(2.0, 0.0,0.0,0.0)
        let oblique12 = V4f(1.5, 1.0,0.0,0.0)
        
        let parameters = [|hor1; oblique12; hor2; 
                           hor1; oblique12; hor2;
                           hor1; oblique12; hor2;
                           hor1; oblique12; hor2|]

        let mapping = DataMapping(origin = Cell2d(0L, 0L, 0), size = V2i(3, 4))

        // a layer gives meaning to raw data
        let bilinParameters = Layer(Defs.HeightsBilinear4f, parameters, mapping)
        
        // build the quadtree (incl. levels-of-detail)
        
        let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]

        qtree

    let createOneCell = 
        // define mapping of raw data to raster space
        let hor1 = V4f(1.0, 0.0,0.0,0.0)
        let hor2 = V4f(2.0, 0.0,0.0,0.0)
        let oblique12 = V4f(1.5, 1.0,0.0,0.0)
        
        let parameters = [|oblique12|]

        let mapping = DataMapping(origin = Cell2d(0L, 0L, 0), size = V2i(1, 1))

        // a layer gives meaning to raw data
        let bilinParameters = Layer(Defs.HeightsBilinear4f, parameters, mapping)
        
        // build the quadtree (incl. levels-of-detail)
        
        let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]

        qtree

    let mainTree = createQuadTreePlanes
    let subTree = createOneCell
    let newTree = Quadtree.Merge SecondDominates mainTree subTree
    //let (_, _, x0) = get pos mainTree
    //let (_, _, x1) = get pos subTree

    let test target merged =
        let get (pos : V2d) root = Sample.PositionTyped<V4f> Query.Config.Default pos Defs.HeightsBilinear4f root
        let pos = V2d(0.5, 0.5)
        let x = (get pos merged).Value
        printfn "sample %A; target %A; %A" x target (x = target) 

    
    Quadtree.Merge FirstDominates  mainTree subTree |> test (V4f(1.0f, 0.0f, 0.0f, 0.0f))
    Quadtree.Merge SecondDominates mainTree subTree |> test (V4f(1.5f, 1.0f, 0.0f, 0.0f))
    Quadtree.Merge FirstDominates  subTree mainTree |> test (V4f(1.5f, 1.0f, 0.0f, 0.0f))
    Quadtree.Merge SecondDominates subTree mainTree |> test (V4f(1.0f, 0.0f, 0.0f, 0.0f))

    ()

let cpunz20201116 () =

    let createQuadTreePlanes =
        let parameters = [| 1.0;  2.0;  3.0; 
                            4.0;  5.0;  6.0;
                            7.0;  8.0;  9.0;
                           10.0; 11.0; 12.0 |]
        let mapping = DataMapping(origin = Cell2d(0L, 0L, 0), size = V2i(3, 4))
        let bilinParameters = Layer(Defs.Heights1d, parameters, mapping)
        let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
        qtree

    let createOneCell = 
        let parameters = [|501.0; 502.0;
                           503.0; 504.0|]
        let mapping = DataMapping(origin = Cell2d(0L, 2L, -1), size = V2i(2, 2))
        let bilinParameters = Layer(Defs.Heights1d, parameters, mapping)
        let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
        qtree

    let createOneSubCell = 
        let hor1 = V4f(4.0, 0.0,0.0,0.0)
        let parameters = [|9991.0; 9992.0;
                           9993.0; 9994.0 |]
        let mapping = DataMapping(origin = Cell2d(2L, 4L, -2), size = V2i(2, 2))
        let bilinParameters = Layer(Defs.Heights1d, parameters, mapping)
        let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
        qtree

    let printRaster raster =
        raster |> Query.All Query.Config.Default |> Seq.map (fun x -> x.GetSamples<float>(Defs.Heights1d)) |> printfn "%A"

    let mainTree = createQuadTreePlanes
    //printfn "[mainTree  ] isLeafNode = %A" mainTree'.IsLeafNode
    printRaster mainTree

    let subTree = createOneCell
    //printfn "[subTree   ] isLeafNode = %A" subTree'.IsLeafNode
    printRaster subTree

    let newTree = Quadtree.Merge SecondDominates mainTree subTree
    //printfn "[newTree   ] isLeafNode = %A" newTree'.IsLeafNode
    printRaster newTree
    
    let subSubTree = createOneSubCell
    //printfn "[subSubTree] isLeafNode = %A" subSubTree'.IsLeafNode
    printRaster subSubTree

    let subNewTree = Quadtree.Merge SecondDominates newTree subSubTree
    //printfn "[subNewTree] isLeafNode = %A" subNewTree'.IsLeafNode
    printRaster subNewTree

    //let qtreeCells = queryService.QueryQuadtreeAll subNewTree
    let resultCells = subNewTree |> Query.All Query.Config.Default |> Seq.toArray
    
    //printfn "%A" resultCells

    ()

open PrettyPrint
open Aardvark.Geometry.Quadtree.Serialization
open System.Threading
open Aardvark.Data
open System.Collections.Immutable
open Uncodium.SimpleStore

let prettyPrintTest () =

    let config = { BuildConfig.Default with SplitLimitPowerOfTwo = 2 }

    let createQuadtree ox oy w h e (value : float32) =
        let xs = Array.create (w*h) value
        Quadtree.Build config [| Layer(Defs.Heights1f, xs, DataMapping(V2l(int64 ox, int64 oy), V2i(int w,h), exponent = e)) |]

    let a = createQuadtree 0 0 10  7  0 1.0f
    let b = createQuadtree 5 3 12 8 -1 2.0f
    let m = Quadtree.Merge SecondDominates a b

    showHtmlDebugView<float32> "merge test" Defs.Heights1f [
        ("a: e= 0, origin (0,0), size (10,7)], split=2x2", a)
        ("b: e=-1, origin (5,3), size (12,8)]. split 2x2", b)
        ("m = Quadtree.Merge SecondDominates a b", m)
        ]

    ()

let loadObsoleteFormatTest () =

    let options = SerializationOptions.SimpleDiskStore(@"T:\Vgm\Data\Raster\20201210_obsolete_quadtree_store")
    
    let key1 = Guid("ae0aeb3e-3444-44bd-9aaf-c005d4e39f89")
    let key2 = Guid("b2706ae3-36b1-4545-8742-eb706957a915")
    let q1 = Quadtree.Load options key2
    
    let xs1 = Query.All Query.Config.Default q1 |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    printfn "%d" xs1.Length

    //Quadtree.PrintStructure true q1

    ()

let loadObsoleteFormatTest_20201223 () =

    let options = SerializationOptions.SimpleDiskStore(@"T:\Vgm\Data\Raster\20201223_testdtm_store\testdtm_store")
    
    let key1 = Guid("a1329b63-5763-4687-b4f1-e5948e30870b")
    let key2 = Guid("69d15895-5fbc-4a22-8bf2-a7c788a2df0c")

    let q1 = Quadtree.Load options key1
    Quadtree.printStructure true q1

    let q2 = Quadtree.Load options key2
    Quadtree.printStructure true q2

    ()

let intersectsCellTest_20210122 () =

    let createQuadtreeWithValue (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int) (value : float32) =
        let size = V2i(w, h)
        let xs = Array.zeroCreate<float32> (w * h)
        for y = 0 to size.Y - 1 do
            for x = 0 to size.X - 1 do
                let i = y * size.X + x
                xs.[i] <- value

        let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))

        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = int splitLimit }
        Quadtree.Build config [| a |]

    let a = createQuadtreeWithValue 0 0 5 3  0 0 10.0f
    let b = createQuadtreeWithValue 5 3 1 1 -1 0 20.0f
    let m = Quadtree.Merge SecondDominates a b
    
    let filter = Cell2d(0,0,2)
    let xs = Query.IntersectsCell  Query.Config.Default filter m |> Seq.collect(fun x -> x.GetSamples<float32> Defs.Heights1f) |> Seq.toArray
    //let ys = Query.IntersectsCell' Query.Config.Default filter m |> Seq.collect(fun x -> x.GetSamples<float32> Defs.Heights1f) |> Seq.toArray
    
    printfn "%d" xs.Length
    //printfn "%d" ys.Length

    //if xs.Length <> ys.Length then failwith "different result"

    //for i = 0 to xs.Length-1 do
    //    let (xcell, xsample) = xs.[i]
    //    let (ycell, ysample) = ys.[i]
    //    let ok = if xcell = ycell && xsample = ysample then " " else "X"
    //    printfn "%A = %A    %s" xcell ycell ok

    ()

/// fast when in memory, slow when from disk
let intersectsCellTest_20210125 () =

    let storeFolder = @"T:\tmp\intersectsCellTest_20210125"

    let createQuadtreeWithValue (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int) (value : float32) =
        let size = V2i(w, h)
        let xs = Array.zeroCreate<float32> (w * h)
        for y = 0 to size.Y - 1 do
            for x = 0 to size.X - 1 do
                let i = y * size.X + x
                xs.[i] <- value

        let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))

        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = int splitLimit }
        Quadtree.Build config [| a |]

    let sw = Stopwatch()

    sw.Restart()
    printfn "creating quadtree a"
    let a = createQuadtreeWithValue    0    0 5000 3000  0 8 10.0f
    printfn "%A" sw.Elapsed

    sw.Restart()
    printfn "creating quadtree b"
    let b = createQuadtreeWithValue 5000 3000 1000 1000 -1 8 20.0f
    printfn "%A" sw.Elapsed

    sw.Restart()
    printfn "merging"
    let m = Quadtree.Merge SecondDominates a b
    printfn "%A" sw.Elapsed

    sw.Restart()
    printfn "save"
    let store = new Uncodium.SimpleStore.SimpleDiskStore(storeFolder)
    let so = SerializationOptions.SimpleStore store
    let id = Quadtree.Save so m
    store.Dispose()
    printfn "%A" id
    printfn "%A" sw.Elapsed

    sw.Restart()
    printfn "load"
    let store = new Uncodium.SimpleStore.SimpleDiskStore(storeFolder)
    let so = SerializationOptions.SimpleStore store
    let mReloaded = Quadtree.Load so id
    printfn "%A" id
    printfn "%A" sw.Elapsed

    sw.Restart()
    printfn "query (in-memory)"
    for x=0 to 100 do
        for y=0 to 100 do
            let filter = Cell2d(0,0,2)
            let ys = Query.IntersectsCell Query.Config.Default filter m |> Seq.collect(fun x -> x.GetSamples<float32> Defs.Heights1f) |> Seq.toArray
            ()
    printfn "%A" sw.Elapsed

    sw.Restart()
    printfn "query (out-of-core)"
    for x=0 to 100 do
        for y=0 to 100 do
            let filter = Cell2d(0,0,2)
            let ys = Query.IntersectsCell Query.Config.Default filter mReloaded |> Seq.collect(fun x -> x.GetSamples<float32> Defs.Heights1f) |> Seq.toArray
            ()
    printfn "%A" sw.Elapsed
    
/// copy quadtree from one store to another store ...
let exportTest_20210126 () =

    let storeFolder = @"T:\tmp\exportTest_20210126"

    let createQuadtreeWithValue (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int) (value : float32) =
        let size = V2i(w, h)
        let xs = Array.zeroCreate<float32> (w * h)
        for y = 0 to size.Y - 1 do
            for x = 0 to size.X - 1 do
                let i = y * size.X + x
                xs.[i] <- value

        let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))

        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = int splitLimit }
        Quadtree.Build config [| a |]

    let sw = Stopwatch()

    sw.Restart()
    printfn "creating quadtree a"
    let a = createQuadtreeWithValue    0    0 5000 3000  0 8 10.0f
    printfn "%A" sw.Elapsed

    sw.Restart()
    printfn "creating quadtree b"
    let b = createQuadtreeWithValue 5000 3000 1000 1000 -1 8 20.0f
    printfn "%A" sw.Elapsed

    sw.Restart()
    printfn "merging"
    let m = Quadtree.Merge SecondDominates a b
    printfn "%A" sw.Elapsed

    let nodeCount = m |> Quadtree.CountNodes true
    printfn "node count = %A" nodeCount

    sw.Restart()
    printfn "save"
    let store = new Uncodium.SimpleStore.SimpleDiskStore(storeFolder)
    let so = SerializationOptions.SimpleStore store
    let id = Quadtree.Save so m
    store.Dispose()
    printfn "%A" id
    printfn "%A" sw.Elapsed

    sw.Restart()
    printfn "export"
    do
        let source = new Uncodium.SimpleStore.SimpleDiskStore(storeFolder) |> SerializationOptions.SimpleStore
        use targetStore = new Uncodium.SimpleStore.SimpleDiskStore(storeFolder + ".exported")
        let target = targetStore |> SerializationOptions.SimpleStore
        let progress (x, total) = printf "\r[progress] %d/%d" x total
        id |> Quadtree.Export source target (Some progress) CancellationToken.None
        printfn ""
    printfn "%A" sw.Elapsed

    sw.Restart()
    printfn "check export"
    do
        let target = new Uncodium.SimpleStore.SimpleDiskStore(storeFolder + ".exported") |> SerializationOptions.SimpleStore
        let exportNodeCount = id |> Quadtree.Load target |> Quadtree.EnumerateKeys true |> Seq.length
        if exportNodeCount <> nodeCount then
            sprintf "Different node count in export. Source has %d nodes, export has %d nodes." nodeCount exportNodeCount |> failwith
    printfn "%A" sw.Elapsed

let madorjan20210127() =

    let id = "756bb95e-b6ac-4afa-9042-46c1c08ee0b0" |> Guid.Parse
    let store = @"T:\Vgm\Data\Raster\20210127_auenpark_0116_store" |> SerializationOptions.SimpleDiskStore
    
    let sw = Stopwatch()

    let rawTest() =
        //Defs.init()
        let swStore = Stopwatch()
        let swDecode = Stopwatch()
        let swLayers = Stopwatch()
        let rec countNodes options id =

            swStore.Start()
            let buffer = options.TryLoad id
            swStore.Stop()
            match buffer with
            | None -> 0
            | Some buffer ->
                swDecode.Start()
                let struct (def, o) = DurableCodec.Deserialize(buffer)
                swDecode.Stop()
                if def = Defs.Node then
                    let map  = o :?> ImmutableDictionary<Durable.Def, obj>

                    let id   = map.Get(Defs.NodeId)              :?> Guid
                    let cell = map.Get(Defs.CellBounds)          :?> Cell2d
                    let sle  = map.Get(Defs.SplitLimitExponent)  :?> int

                    swLayers.Start()
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
                    swLayers.Stop()

                    match map.TryGetValue(Defs.SubnodeIds) with
                    | (false, _) -> 1
                    | (true, o)  ->
                        let keys = o :?> Guid[]
                        let counts = keys |> Seq.map (fun k -> if k <> Guid.Empty then countNodes options k else 0) |> Seq.sum
                        1 + counts
                else
                    failwith "wrong type"

        printfn "countNodes %A ..." id
        sw.Restart()
        let nodeCountFast = countNodes store id
        sw.Stop()
        printfn "total  %A" sw.Elapsed
        printfn "store  %A" swStore.Elapsed
        printfn "decode %A" swDecode.Elapsed
        printfn "layers %A" swLayers.Elapsed
        printfn "nodeCountFast = %d" nodeCountFast
    //rawTest()

    printfn "load %A ..." id
    sw.Restart()
    let q = Quadtree.Load store id
    sw.Stop()
    printfn "%A" sw.Elapsed
    printfn "%A" q.ExactBoundingBox

    printfn "\nQuadtree.CountNodes %A ..." q.Id
    sw.Restart()
    let countNodes = Quadtree.CountNodes true q
    sw.Stop()
    printfn "%A" sw.Elapsed
    printfn "%d" countNodes

    printfn "\nsave %A ..." q.Id
    sw.Restart()
    let idNew = Quadtree.Save store q
    sw.Stop()
    printfn "%A" sw.Elapsed
    printfn "%A" idNew

    printfn "\nreload %A ..." idNew
    sw.Restart()
    let qReloaded = Quadtree.Load store idNew
    sw.Stop()
    printfn "%A" sw.Elapsed
    printfn "%A" qReloaded.ExactBoundingBox

    printfn "\nQuery.All ..."
    sw.Restart()
    let all = Query.All Query.Config.Default q |> Seq.toArray
    sw.Stop()
    printfn "%A" sw.Elapsed
    printfn "%d" all.Length

    printfn "\nQuery.All (repeat) ..."
    sw.Restart()
    let all = Query.All Query.Config.Default q |> Seq.toArray
    sw.Stop()
    printfn "%A" sw.Elapsed
    printfn "%d" all.Length

    //printfn "[Instrumentation] countStoreGet                     = %d" Instrumentation.countStoreGet
    //printfn "[Instrumentation] countDurableCodecDeserializeLayer = %A" Instrumentation.countDurableCodecDeserializeLayer
    //printfn "[Instrumentation] swDurableCodecDeserializeLayer    = %f" Instrumentation.swDurableCodecDeserializeLayer.Elapsed.TotalSeconds
    //printfn "[Instrumentation] countDurableCodecDeserializeNode  = %A" Instrumentation.countDurableCodecDeserializeNode
    //printfn "[Instrumentation] swDurableCodecDeserializeNode     = %f" Instrumentation.swDurableCodecDeserializeNode.Elapsed.TotalSeconds
    //printfn "[Instrumentation] countDurableCodecDeserializeNode  = %A" Instrumentation.countSerializationOptionsLoadNode
    //printfn "[Instrumentation] swDurableCodecDeserializeNode     = %f" Instrumentation.swSerializationOptionsLoadNode.Elapsed.TotalSeconds

    ()

let madorjan20210216() =

    let id = "D56C938F-382E-456C-926B-BBBEE1DEA4E0" |> Guid.Parse
    let store = @"T:\Vgm\Stores\2021-02-16_madorjan" |> SerializationOptions.SimpleDiskStore

    match store.TryLoad id with
    | None   ->
        printfn "key %A does not exist in store" id
    | Some _ ->

        let root = OutOfCoreNode { Id = id; HasMask = false; Load = fun () -> Quadtree.Load store id }
        Quadtree.printStructure true root

        let q = Quadtree.Load store id
        printfn "%A" q.Id

let test_20210304_adorjan () =

    //let store = @"C:\DATA\2020.3.terrain_store"
    let storePath = @"T:\Vgm\Data\Raster\2020.3.terrain_store"
    let key = "396630ec-859d-4bc8-b53e-ffe21620227a" |> Guid.Parse

    let testKey (k : Guid) =
        use store = new SimpleDiskStore(storePath)
        printfn "Quadtree exists: %b" (store.Contains (string k))
        let options = SerializationOptions.SimpleStore(store)
        let q = Quadtree.Load options k
        let containsBil = Quadtree.ContainsLayer Defs.BilinearParams4f q
        let containsH = Quadtree.ContainsLayer Defs.HeightsBilinear4f q
        let lset = q.LayerSet

        let (success, q2) = Quadtree.UpdateLayerSemantic (Defs.BilinearParams4f, Defs.HeightsBilinear4f) q

        let sampleCount =
            q |> Query.All Query.Config.Default
              |> Seq.collect (fun cell -> cell.GetSamples<V4f> Defs.BilinearParams4f)
              |> Seq.length

        printfn "%A\n\nContains BilinearParams4f: %b\nContains HeightsBilinear4f: %b\nLayerSet: %A\nUpdateLayerSemantic successful: %b\nSamples at BilinearParams4f:%d" q containsBil containsH lset success sampleCount

        let containsBil = Quadtree.ContainsLayer Defs.BilinearParams4f q2
        let containsH = Quadtree.ContainsLayer Defs.HeightsBilinear4f q2

        printfn "=============================="
        printfn "%A\n\nContains BilinearParams4f: %b\nContains HeightsBilinear4f: %b\nLayerSet: %A\nUpdateLayerSemantic successful: %b\nSamples at BilinearParams4f:%d" q containsBil containsH lset success sampleCount
        

    testKey key

let test_20210318_cpunz () =

    let storePath = @"T:\Vgm\Data\Raster\20210318_cpunz"
    let key = "b32cc924-10d4-4251-a8ba-b20892007b65" |> Guid.Parse

    use store = new SimpleDiskStore(storePath)
    //for k in store.SnapshotKeys() do printfn "%A" k

    // check if specified key exists
    printfn "Quadtree exists: %b" (store.Contains (string key))
    let options = SerializationOptions.SimpleStore(store)

    // load quadtree
    let qtree = Quadtree.Load options key
    //Quadtree.PrintStructure true qtree
    printfn "loaded quadtree (bounds = %A)" qtree.ExactBoundingBox

    // query ...
    let poly = Polygon2d([|
        V2d(66017.513819274, 270086.360212579)
        V2d(66022.1350611048, 270086.360212579)
        V2d(66022.1350611048, 270090.195622167)
        V2d(66017.513819274, 270090.195622167)
        |])

    printfn "---------------------------"
    let config = { Query.Config.Default with Verbose = false }
    let xs = qtree |> Query.InsidePolygon config poly |> Seq.toArray
    printfn "results (count=%d):" xs.Length
    for x in xs do printfn "    %A" x
    
    printfn "---------------------------"
    let ys = xs |> Array.collect (fun chunk -> chunk.GetSamples<V4f> Defs.VolumesBilinear4f)
    printfn "samples (count=%d):" ys.Length
    for y in ys do printfn "    %A" y
    
    printfn "---------------------------"
    let zs = ys |> Seq.collect(fun (y,_) -> Query.IntersectsCell config y qtree) |> Seq.toArray
    printfn "samples (count=%d):" zs.Length
    for z in zs do printfn "    %A" z

let madorjan20211103 () =
    let heights = Array.create 16 V4f.Zero
    
    let mapping = DataMapping(origin = Cell2d(0L, 0L, 0), size = V2i(4, 4))
    let heightsLayer = Layer(Defs.HeightsBilinear4f, heights, mapping) :> ILayer
    let config = { BuildConfig.Default with SplitLimitPowerOfTwo = 2 }
    let qTree = Quadtree.Build config [| heightsLayer |]
    
    printfn "qTree.Cell.BoundingBox: %A" qTree.Cell.BoundingBox
    printfn "qTree.LayerSet        : %A" qTree.LayerSet

let builderSketch () =

    // (1) load octree with many merges as test data
    //let store = @"W:\Datasets\Vgm\Data\2023-09-04_quadtree"
    let store = @"W:\Datasets\Vgm\Data\2023-11-10_quadtree"

    let key = File.ReadAllText(Path.Combine(store, "key.txt")) |> Guid
    let options = Serialization.SerializationOptions.SimpleDiskStore store

    let originalQuadtree : QNodeRef = Quadtree.Load options key
    
    // (2) take all leaf nodes as test patches
    // (these should be the original data that was merged together)
    let patches = originalQuadtree |> Quadtree.EnumerateLeafNodesInMemory |> List.ofSeq
    
    // stats
    printfn "original quadtree has %d leaf nodes and %d merge nodes" patches.Length (Quadtree.CountMergeNodes true originalQuadtree)
    let resolutions = patches |> List.groupBy (fun n -> n.SampleExponent) |> List.map (fun (k, v) -> k) |> List.sortDescending   
    printfn "original quadtree resolution levels: %A" resolutions

    // (3) create a new builder and add all patches
    let builder = Builder()
    for n in patches do builder.Add n
    
    // (4) build new and better quadtree
    match builder.Build BuildConfig.Default with
    | None -> printfn "no quadtree"
    | Some newAndBetterTree ->
        let countLeafNodes = newAndBetterTree |> Quadtree.CountLeafNodes true
        let countMergeNodes = newAndBetterTree |> Quadtree.CountMergeNodes true
        printfn "new quadtree has %d leaf nodes and %d merge nodes" countLeafNodes countMergeNodes

        let options = SerializationOptions.NewInMemoryStore(verbose = true)
        let id = newAndBetterTree |> Quadtree.Save options
        printfn "saved quadtree id = %A" id


    
        

    ////let nodeCount = root |> Quadtree.enumerateNodesBreadthFirst true |> Seq.length
    //printfn "node count (all)   = %d" (root |> Quadtree.CountNodes true)
    //printfn "node count (inner) = %d" (root |> Quadtree.CountInner true)
    //printfn "node count (leafs) = %d" (root |> Quadtree.CountLeafs true)

    
    //printfn ""
    //printfn "grouping by sample size"
    //let gs = leafNodes |> Seq.groupBy (fun n -> n.LayerSet.SampleExponent) |> List.ofSeq
    //let histo = gs
    //            |> Seq.map (fun (key, ns) -> (key, ns |> List.ofSeq))
    //            //|> Seq.sortByDescending (fun (key, _) -> key)

    //for (k, ns) in histo do

    //    let countPatches = ns |> List.length
    //    let countSamples = ns |> List.sumBy (fun x -> x.LayerSet.SampleWindow.Area)
    //    let ebb = ns |> Seq.map (fun n -> n.ExactBoundingBox) |> Box2d
    //    printfn "  %d" k
    //    printfn "  patches %8d" countPatches
    //    printfn "  samples %8d" countSamples
    //    printfn "  ebb     %A"  ebb
    //    printfn ""

    //    //let result = ns |> List.map (fun n -> n.LayerSet) |> Builder.Build

    //    for n in ns do builder.Add(n.LayerSet)

    //    ()

        //let boxes = List<Box2l>()
        //for n in ns do
        //    let foo = n |> InMemoryNode |> Query.All Query.Config.Default |> Seq.collect (fun x -> x.GetSampleCells ()) |> Seq.groupBy id |> Seq.filter (fun (_, xs) -> xs |> Seq.length <> 1) |> Array.ofSeq
        //    for (c, cs) in foo do printfn "%A -> %d" c (cs |> Seq.length)
        //    // printfn "[%d] %A" k n.LayerSet.SampleWindow
        //    boxes.Add(n.LayerSet.SampleWindow)
        //    ()

        //let mergeBoxes (boxes : List<Box2l>) : List<Box2l> =
        //    let gbb = Box2l(boxes)
        //    printfn "gbb %A with size = %A" gbb gbb.Size

        //    let candidateSets = boxes |> Seq.groupBy (fun box -> box.Min.X) |> Seq.map (fun (k, bs) -> (k, bs |> List.ofSeq)) |> List.ofSeq
        //    printfn "    candidate sets     : %d" candidateSets.Length
        //    printfn "    candidate sets (>1): %d" (candidateSets |> Seq.filter (fun (k, bs) -> bs.Length > 1) |> Seq.length)

        //    //for (minX, bs) in candidateSets do
        //    //    let groupedByWidth = bs |> Seq.groupBy (fun b -> b.Size.X) |> List.ofSeq
        //    //    for (w, bs) in groupedByWidth do
        //    //        // same min.X, same width
        //    //        printfn "minX = %d, width = %d, count = %d" minX w (bs |> Seq.length)
        //    //        let bs2 = bs |> Seq.sortBy (fun b -> b.Min.Y)
        //    //        for b in bs2 do
        //    //            printfn "    * %A  %A" b b.Size


        //    let result = List<Box2l>()
        //    result

        //let boxesMerged = mergeBoxes boxes

        //printfn "  merged boxes ... %d" boxesMerged.Count
        
    
    //printfn ""
    //let nodeCount = root |> enumerateLeafs |> Seq.length
    //printfn "node count (foo  ) = %d" nodeCount

    //printfn ""
    //let histo = root 
    //            |> Quadtree.EnumerateNodesBreadthFirst true 
    //            |> Seq.groupBy (fun n -> n.GetType().Name)
    //            |> Seq.map (fun (key, ns) -> (key, ns |> Seq.length))
    //            |> Map.ofSeq
    //for kv in histo do printfn "%s -> %d" kv.Key kv.Value

    //Quadtree.PrintStructure true q

    //let xs = 
    //    root 
    //    |> Quadtree.enumerateNodesBreadthFirst true
    //    |> Seq.filter (fun x -> match x with | InMemoryNode _ -> true | _ -> false)
    //    |> Seq.map (fun x -> match x with | InMemoryNode n -> n | _ -> failwith "Unexpected type.")

    //printfn "count = %d" (xs |> Seq.length)

    //let gs = xs |> Seq.groupBy (fun x -> x.LayerSet.SampleWindow)
    //for (key, g) in gs do printfn "group: %A" key

    //for x in xs do
    //  printfn "%A" x.LayerSet.SampleWindow

    //q
    //|> Query.All { Query.Config.Default with Verbose=true }
    //|> Seq.toArray
    //|> ignore

    ()

let createQuadtreeWithValue (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int) (value : float32) : QNodeRef =
        let size = V2i(w, h)
        let xs = Array.zeroCreate<float32> (w * h)
        for y = 0 to size.Y - 1 do
            for x = 0 to size.X - 1 do
                let i = y * size.X + x
                xs.[i] <- value

        let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))

        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = int splitLimit }
        Quadtree.Build config [| a |]

let builderTest_20240112 () =

    let cpTree =

        let createQuadTreePlanesZeroBase =

            // define mapping of raw data to raster space
            let hor0 = V4f(0.0, 0.0,0.0,0.0)
            
            let parameters = [|hor0; hor0; hor0; 
                               hor0; hor0; hor0;
                               hor0; hor0; hor0;
                               hor0; hor0; hor0;
                               hor0; hor0; hor0;|]
            
            let mapping = DataMapping(origin = Cell2d(0L, 0L, 0), size = V2i(3, 5))
            
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.HeightsBilinear4f, parameters, mapping)
            
            // build the quadtree (incl. levels-of-detail)        
            let qtree = Quadtree.Build { BuildConfig.Default with SplitLimitPowerOfTwo = 10 } [| bilinParameters |]
            
            qtree

        let createOneSubCell (level : int) (east:int64) (north:int64)= 
            // define mapping of raw data to raster space
            let elevation = -1.0*((float)level)
            let hor1 = V4f(elevation, 0.0,0.0,0.0)
            
            let parameters = [|hor1;hor1;hor1;hor1|]

            let mapping = DataMapping(origin = Cell2d(east, north, level), size = V2i(2, 2))

            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.HeightsBilinear4f, parameters, mapping)
            
            // build the quadtree (incl. levels-of-detail)
            
            let qtree = Quadtree.Build { BuildConfig.Default with SplitLimitPowerOfTwo = 10 } [| bilinParameters |]

            qtree

        let mainTree = createQuadTreePlanesZeroBase

        let config = Query.Config.Default

        let subTree2 = createOneSubCell -2 0L 0L
        let subTree2_1 = createOneSubCell -2 2L 0L
        let subTree2_2 = createOneSubCell -2 2L 2L
        let subTree2_3 = createOneSubCell -2 0L 2L
        let mutable newTree = Quadtree.Merge SecondDominates mainTree subTree2
        newTree <- Quadtree.Merge SecondDominates newTree subTree2_1
        newTree <- Quadtree.Merge SecondDominates newTree subTree2_2
        newTree <- Quadtree.Merge SecondDominates newTree subTree2_3

        let subTree3 = createOneSubCell -1 2L 6L
        newTree <- Quadtree.Merge SecondDominates newTree subTree3

        newTree


    
    let sw = Stopwatch()

    // (1) create 10x10 base grid with 1m tiles
    //let store = @"W:\Datasets\Vgm\Data\2023-09-04_quadtree"
    let baseGrid = createQuadtreeWithValue 0 0 2 2 0 8 42.0f

    let singleTileA = createQuadtreeWithValue 0 0 1 1 -1 8 50.0f
    let singleTileB = createQuadtreeWithValue 2 2 1 1 -1 8 52.0f

    let originalQuadtree = Quadtree.Merge Dominance.MoreDetailedOrSecond baseGrid singleTileA
    let originalQuadtree = Quadtree.Merge Dominance.MoreDetailedOrSecond originalQuadtree singleTileB
    
    // (2) take all leaf nodes as test patches
    // (these should be the original data that was merged together)
    let patches = cpTree |> Quadtree.EnumerateLeafNodesInMemory |> List.ofSeq
    
    // stats
    printfn "original quadtree has %d leaf nodes and %d merge nodes" patches.Length (Quadtree.CountMergeNodes true originalQuadtree)
    let resolutions = patches |> List.groupBy (fun n -> n.SampleExponent) |> List.map (fun (k, v) -> k) |> List.sortDescending   
    printfn "original quadtree resolution levels: %A" resolutions

    // (3) create a new builder and add all patches
    let builder = Builder()
    for n in patches do builder.Add n
    
    //builder.Export(Path.GetFullPath("20240202_buildertest")) |> ignore

    builder.Print()

    // (4) build new and better quadtree
    match builder.Build BuildConfig.Default with
    | None -> printfn "no quadtree"
    | Some newAndBetterTree ->
        let countLeafNodes = newAndBetterTree |> Quadtree.CountLeafNodes true
        let countMergeNodes = newAndBetterTree |> Quadtree.CountMergeNodes true
        printfn "new quadtree has %d leaf nodes and %d merge nodes" countLeafNodes countMergeNodes

        let options = SerializationOptions.NewInMemoryStore(verbose = false)
        let id = newAndBetterTree |> Quadtree.Save options
        printfn "saved quadtree id = %A" id

        let xs = 
            Query.All Query.Config.Default originalQuadtree
            |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f))
            |> Array.ofSeq

        for (c, h) in xs do
            printfn "%5d %5d %5d %10.2f" c.X c.Y c.Exponent h

        ()

let buildSerializationTest_20240202 () =

    let path = Path.GetFullPath(@"c:\tmp\20240202_quadtreetest")
    
    printfn "path = %s" path

    let builder = Builder()

    createQuadtreeWithValue 0 0 2 2 0 8 42.0f  |> builder.Add
    createQuadtreeWithValue 0 0 1 1 -1 8 50.0f |> builder.Add
    createQuadtreeWithValue 2 2 1 1 -1 8 52.0f |> builder.Add

    let options = SerializationOptions.NewInMemoryStore(verbose = false)
   
    for i = 1 to 10 do
        let idInMem = builder.Save options
        let idFile = builder.Export(path)

        printfn "saved builder to memory, id = %A" idInMem
        printfn "saved builder to file  , id = %A" idFile

        let builderReloadedInMem = Builder.Load options idInMem
        match builderReloadedInMem with
        | None   -> printfn "reloaded from memory = None"
        | Some x -> printfn "reloaded from memory, %d patches" (x.GetPatches() |> Seq.length)
       
        let builderReloadedFile = Builder.Import(path, idFile)
        match builderReloadedFile with
        | None   -> printfn "reloaded from file = None"
        | Some x -> printfn "reloaded from file, %d patches" (x.GetPatches() |> Seq.length)
       
    ()

let cp_20240202_quadtreetest () =

    let path = Path.GetFullPath(@"W:\Datasets\Vgm\Quadtree\20240202_quadtreetest")
    
    printfn "path = %s" path

    let options = SerializationOptions.NewInMemoryStore(verbose = false)
   
    let idFile = Guid(File.ReadAllText(@"W:\Datasets\Vgm\Quadtree\20240202_quadtreetest\builder.20240202122614.638424735748032405.key.txt"))
    let builderReloadedFile = Builder.Import(path, idFile)
    match builderReloadedFile with
    | None   -> printfn "reloaded from file = None"
    | Some x ->
        printfn "reloaded from file, %d patches" (x.GetPatches() |> Seq.length)

        let samplesCount = 
            x.GetPatches()
            |> Seq.filter(fun patch -> patch.SampleExponent = -3)
            |> Seq.sumBy(fun patch -> patch.SampleWindow.Area)
        printfn("total samples count with e = -3: %d") samplesCount

        let buildConfig = { BuildConfig.Default with Verbose = true }
        match x.Build2 buildConfig with
        | None -> failwith ""
        | Some qtree ->
            let makeReturnValOfQueryResults (resultChunk : seq<Query.Result>) (def : Aardvark.Data.Durable.Def) =
                
                let samples =
                    resultChunk
                    |> Seq.collect (fun chunk -> chunk.GetSamples<V4f> def)
                    //|> Seq.map (fun (cell,bilParamV) -> let bilParam = BilinearSurfaceParams.FromV4f bilParamV
                    //                                    (bilParam, cell))
                    |> Seq.toList
            
                //posAndParams |> Seq.filter (fun pos -> (fst pos).b0.IsNaN() |> not)
                //             |> Seq.map (fun pos -> { originalCell = snd pos; surface = fst pos} )

                //let mutable i = 0
                //for s in samples do
                //    i <- i + 1
                //    printfn "%A" s

                samples

            let config = Query.Config.Default //{ Query.Config.Default with Verbose = true }
            let resultCells = qtree |> Query.All config |> Seq.toArray
            let samples = makeReturnValOfQueryResults resultCells Defs.HeightsBilinear4f

            ()
       
    ()

[<EntryPoint>]
let main argv =

    cp_20240202_quadtreetest ()

    //buildSerializationTest_20240202 ()

    //builderTest_20240112 ()

    //builderSketch ()

    //madorjan20211103 ()

    //Perftests.timeReallyLargeMergedQuadtreeQueries ()

    //test_20210318_cpunz ()

    //test_20210304_adorjan ()

    //madorjan20210216 ()

    //madorjan20210127 ()
    
    //exportTest_20210126 ()

    //intersectsCellTest_20210125 ()

    //intersectsCellTest_20210122 ()

    //loadObsoleteFormatTest_20201223()

    //loadObsoleteFormatTest()

    //prettyPrintTest ()

    //let xs = [| 1; 2; 3; |]
    //let ys = [| 4; 5; |]
    //let ms = Array.concat [| xs; ys |]
    //printfn "%A" ms

    //let x = Seq.empty<Box2l> |> Box2l
    //let y = Box2l.Unit
    //printfn "%A" x
    //printfn "%A" y
    //printfn "%A" (Box2l(x, y, x, y))

    //cpunz20201116 ()

    //cpunz20201105 ()

    //subtractionEnumerationTest ()
   
    //cpunz20200925 ()

    //cpunz20200923 ()

    //polyTest ()

    //cpunz20200829 ()

    //example ()

    //merge ()

    //buildQuadtree ()

    //test ()

    //performanceTest ()

    0
