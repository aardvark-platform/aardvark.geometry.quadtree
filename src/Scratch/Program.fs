open System
open Aardvark.Base
open Aardvark.Geometry.Quadtree
open System.Diagnostics
open System.IO
open System.Globalization

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

//let merge () =

//    // create first quadtree
//    let heights1 = [| 
//        1.0; 1.0; 2.0; 2.0
//        1.5; 1.6; 1.7; 1.8
//        1.6; 1.7; 2.0; 2.2
//        |]
//    let mapping1 = DataMapping(origin = Cell2d(0, 0, 0), size = V2i(4, 3))
//    let heightsLayer1 = Layer(Defs.Heights1d, heights1, mapping1)
//    let firstQuadtree = Quadtree.Build BuildConfig.Default [| heightsLayer1 |]
//    printfn "%A" (firstQuadtree.TryGetInMemory().Value.Cell)

//    // create second quadtree
//    let heights2 = [| 
//        3.1; 3.2
//        3.3; 3.4
//        |]
//    let mapping2 = DataMapping(origin = Cell2d(4, 2, -1), size = V2i(2, 2))
//    let heightsLayer2 = Layer(Defs.Heights1d, heights2, mapping2)
//    let secondQuadtree = Quadtree.Build BuildConfig.Default [| heightsLayer2 |]
//    printfn "%A" (secondQuadtree.TryGetInMemory().Value.Cell)

//    // merge both octrees
//    let mergedQtree = Quadtree.Merge MoreDetailedOrSecond firstQuadtree secondQuadtree
//    printfn "%A" (mergedQtree.TryGetInMemory().Value.Cell)

//    // enumerate all samples
//    let chunks = mergedQtree |> Query.InsideCell Query.Config.Default (mergedQtree.TryGetInMemory().Value.Cell)
    
//    let allSamples = chunks |> Seq.collect (fun chunk -> chunk.GetSamples<float> Defs.Heights1d)
//    for cell, x in allSamples do printfn "%A -> %f" cell x

//    // save
//    let options = SerializationOptions.NewInMemoryStore(verbose = true)
//    //let options = SerializationOptions.SimpleDiskStore(@"T:\qstore2")
//    let id = mergedQtree |> Quadtree.Save options
//    printfn "saved quadtree %A" id

//    // load
//    let loadedQtree = Quadtree.Load options id
//    match loadedQtree with
//    | InMemoryNode loadedQtree -> printfn "loaded quadtree %A" loadedQtree.Id
//    | NoNode                   -> printfn "quadtree %A does not exist" id
//    | OutOfCoreNode _          -> printfn "quadtree %A came back is OutOfCoreNode - strange!" id

//    printfn "count nodes: %d" (Quadtree.CountNodes loadedQtree)

//    ()
   
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
            qtree |> Query.IntersectsCell' config cellForQuery |> makeReturnValOfQueryResults

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


[<EntryPoint>]
let main argv =

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
   
    cpunz20200925 ()

    //cpunz20200923 ()

    //polyTest ()

    //cpunz20200829 ()

    //example ()

    //merge ()

    //buildQuadtree ()

    //test ()

    //performanceTest ()

    0
