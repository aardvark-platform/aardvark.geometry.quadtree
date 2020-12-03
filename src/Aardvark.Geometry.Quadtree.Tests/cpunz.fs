namespace Tests

open Aardvark.Base
open Aardvark.Geometry.Quadtree
open System
open System.Globalization
open System.IO
open Xunit

module cpunz =

    let USE_LOCAL_TESTFILES = false

    [<AutoOpen>]
    module Helpers =

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
    

    [<Fact>]
    let ``cpunz_20200829`` () =
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
    
        Assert.True(result0.Length = 5)
        Assert.True(fst result0.[0] = Cell2d(0, 0, 0))
        Assert.True(fst result0.[1] = Cell2d(1, 0, 0))
        Assert.True(fst result0.[2] = Cell2d(2, 0, 0))
        Assert.True(fst result0.[3] = Cell2d(1, 1, 0))
        Assert.True(fst result0.[4] = Cell2d(2, 1, 0))

    [<Fact>]
    let ``cpunz_20200923`` () =

        let hor = V4f(0.0, 0.0,0.0,0.0) 
    
        let parameters = [|hor|]

        let mapping = DataMapping(origin = Cell2d(0L, 0L, 2), size = V2i(1, 1))

        // a layer gives meaning to raw data
        let bilinParameters = Layer(Defs.HeightsBilinear4f, parameters, mapping)
    
        // build the quadtree (incl. levels-of-detail)
    
        let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]

        let r = Query.IntersectsCell Query.Config.Default (Cell2d(0,0,0)) qtree |> Array.ofSeq

        ()

    [<Fact>]
    let ``cpunz_20200925`` () =
    
      if USE_LOCAL_TESTFILES then

        
        /// Query.IntersectsCell speed too slow (seen for many queries) ...
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
                qtree |> Query.IntersectsCell config cellForQuery |> makeReturnValOfQueryResults

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

    [<Fact>]
    let ``cpunz_20201105`` () =

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

    [<Fact>]
    let ``cpunz_20201116`` () =

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
        let mainTree' = mainTree.TryGetInMemory().Value
        printfn "[mainTree  ] isLeafNode = %A" mainTree'.IsLeafNode
        printRaster mainTree

        let subTree = createOneCell
        let subTree' = subTree.TryGetInMemory().Value
        printfn "[subTree   ] isLeafNode = %A" subTree'.IsLeafNode
        printRaster subTree

        let newTree = Quadtree.Merge SecondDominates mainTree subTree
        let newTree' = newTree.TryGetInMemory().Value
        printfn "[newTree   ] isLeafNode = %A" newTree'.IsLeafNode
        printRaster newTree
    
        let subSubTree = createOneSubCell
        let subSubTree' = subSubTree.TryGetInMemory().Value
        printfn "[subSubTree] isLeafNode = %A" subSubTree'.IsLeafNode
        printRaster subSubTree

        let subNewTree = Quadtree.Merge SecondDominates newTree subSubTree
        let subNewTree' = subNewTree.TryGetInMemory().Value
        printfn "[subNewTree] isLeafNode = %A" subNewTree'.IsLeafNode
        printRaster subNewTree

        //let qtreeCells = queryService.QueryQuadtreeAll subNewTree
        let resultCells = subNewTree |> Query.All Query.Config.Default |> Seq.toArray
    
        //printfn "%A" resultCells


        ()

    [<Fact>]
    let ``cpunz_20201130`` () =

        let createQuadTreePlanes =
            
            // define mapping of raw data to raster space
            let hor1 = V4f(1.0, 0.0,0.0,0.0)
            let hor2 = V4f(2.0, 0.0,0.0,0.0)
            let oblique12 = V4f(1.5, 1.0,0.0,0.0)
            let nanVal = V4f(nan, nan, nan, nan)  
            let parameters = [|nanVal; nanVal; nanVal; 
                                hor1; oblique12; nanVal;
                                hor1; oblique12; nanVal;
                                |]
            
            let mapping = DataMapping(origin = Cell2d(0L, -1L, 0), size = V2i(3, 3))
            
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                    
            // build the quadtree (incl. levels-of-detail)
                    
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
            
            qtree
            
        let createOneCell = 
            // define mapping of raw data to raster space
            let hor1 = V4f(3.0, 0.0,0.0,0.0)
                    
            let parameters = [|hor1|]//;hor1;hor1;hor1|]
            
            let mapping = DataMapping(origin = Cell2d(3L, 2L, -1), size = V2i(1, 1))
            
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                    
            // build the quadtree (incl. levels-of-detail)
                    
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
            
            qtree

        let mainTree = createQuadTreePlanes
        let subTree = createOneCell
        let newTree = Quadtree.Merge SecondDominates mainTree subTree

        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let samples = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
        samples |> printfn "%A"
               
        //let ptsNormals = getCenterDataOfCells qtreeCells
        //                                        |> Seq.toArray

        ()

    let createQuadTreePlanesWithNaN =
        
            // define mapping of raw data to raster space
            let hor1 = V4f(1.0, 0.0,0.0,0.0)
            let hor2 = V4f(2.0, 0.0,0.0,0.0)
            let oblique12 = V4f(1.5, 1.0,0.0,0.0)
            let nanVal = V4f(nan, nan, nan, nan)  
            let parameters = [|nanVal; nanVal; nanVal; 
                               hor1; oblique12; nanVal;
                               hor1; oblique12; nanVal;
                               |]
        
            let mapping = DataMapping(origin = Cell2d(0L, 0L, 0), size = V2i(3, 3))
        
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
        
            qtree
   
    
    [<Fact>]
    let ``punz_double_merge_volume`` () =
        let createOneCell = 
            // define mapping of raw data to raster space
            let hor1 = V4f(3.0, 0.0,0.0,0.0)
                    
            let parameters = [|hor1|]//;hor1;hor1;hor1|]
            
            let mapping = DataMapping(origin = Cell2d(4L, 4L, -1), size = V2i(1, 1))
            
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                    
            // build the quadtree (incl. levels-of-detail)
                    
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
            
            qtree
    
        let createOneSubCell4 = 
            // define mapping of raw data to raster space
            let hor1 = V4f(1.1, 0.0,0.0,0.0)
            let hor2 = V4f(1.2, 0.0,0.0,0.0)
            let hor3 = V4f(1.3, 0.0,0.0,0.0)
            let hor4 = V4f(1.4, 0.0,0.0,0.0)
                
            let parameters = [|hor1;hor2;
                               hor3;hor4|]
    
            let mapping = DataMapping(origin = Cell2d(2L, 4L, -2), size = V2i(2, 2))
    
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
    
            qtree
    
            
        let mainTree = createQuadTreePlanesWithNaN
        let subTree = createOneCell 
            
        let newTree = Quadtree.Merge SecondDominates mainTree subTree
    
        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let qtreeCells = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr)
                   
        let subTree4 = createOneSubCell4
        let newTree4 = Quadtree.Merge SecondDominates newTree subTree4
        let resultCells4 = newTree4 |> Query.All config
        let qtreeCells4 = resultCells4 |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f)) 
                                       |> Seq.collect (fun arr -> arr)
       
        ()

    [<Fact>]
    let ``punz_merge_verySmall_into_coarse_volume`` () =
        let createOneCell = 
            // define mapping of raw data to raster space
            let hor1 = V4f(3.0, 0.0,0.0,0.0)
                    
            let parameters = [|hor1|]//;hor1;hor1;hor1|]
            
            let mapping = DataMapping(origin = Cell2d(4L, 4L, -2), size = V2i(1, 1))
            
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                    
            // build the quadtree (incl. levels-of-detail)
                    
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
            
            qtree
    
    
        let mainTree = createQuadTreePlanesWithNaN
        let subTree = createOneCell 
            
        let newTree = Quadtree.Merge SecondDominates mainTree subTree
    
        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let qtreeCells = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr)
                   
        ()

    [<Fact>]
    let ``punz_merge_withOverlap_within_other_volume`` () =
        let createOverlap = 
            // define mapping of raw data to raster space
            let hor1 = V4f(1.1, 0.0,0.0,0.0)
            let hor2 = V4f(1.2, 0.0,0.0,0.0)
            let hor3 = V4f(1.3, 0.0,0.0,0.0)
            let hor4 = V4f(1.4, 0.0,0.0,0.0)
                
            let parameters = [|hor1;hor2;
                               hor3;hor4|]
    
            let mapping = DataMapping(origin = Cell2d(3L, 3L, -2), size = V2i(2, 2))
    
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
    
            qtree
    
    
        let mainTree = createQuadTreePlanesWithNaN
        let subTree = createOverlap 
            
        let newTree = Quadtree.Merge SecondDominates mainTree subTree
    
        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let qtreeCells = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr)
                   
        ()

    [<Fact>]
    let ``punz_merge_withOverlap_overboarder_other_volume`` () =
        
        let createOverlap = 
            // define mapping of raw data to raster space
            let hor1 = V4f(1.1, 0.0,0.0,0.0)
            let hor2 = V4f(1.2, 0.0,0.0,0.0)
            let hor3 = V4f(1.3, 0.0,0.0,0.0)
            let hor4 = V4f(1.4, 0.0,0.0,0.0)
                
            let parameters = [|hor1;hor2;
                               hor3;hor4|]
    
            let mapping = DataMapping(origin = Cell2d(11L, 11L, -2), size = V2i(2, 2))
    
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
    
            qtree
    
    
        let mainTree = createQuadTreePlanesWithNaN
        let subTree = createOverlap 
            
        let newTree = Quadtree.Merge SecondDominates mainTree subTree
    
        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let qtreeCells = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr)
                   
        ()

    [<Fact>]
    let ``punz_merge_withOverlap_overboarder_overOrigin_other_volume`` () =
        let createOverlap = 
            // define mapping of raw data to raster space
            let hor1 = V4f(1.1, 0.0,0.0,0.0)
            let hor2 = V4f(1.2, 0.0,0.0,0.0)
            let hor3 = V4f(1.3, 0.0,0.0,0.0)
            let hor4 = V4f(1.4, 0.0,0.0,0.0)
                
            let parameters = [|hor1;hor2;
                               hor3;hor4|]
    
            let mapping = DataMapping(origin = Cell2d(-1L, -1L, -2), size = V2i(2, 2))
    
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
    
            qtree
    
    
        let mainTree = createQuadTreePlanesWithNaN
        let subTree = createOverlap 
            
        let newTree = Quadtree.Merge SecondDominates mainTree subTree
    
        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let qtreeCells = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr)
                   
        ()

    

    [<Fact>]
    let ``punz_2_merge_fine_first_other_volume`` () =
        let createOverlap = 
            // define mapping of raw data to raster space
            let hor1 = V4f(1.1, 0.0,0.0,0.0)
            let hor2 = V4f(1.2, 0.0,0.0,0.0)
            let hor3 = V4f(1.3, 0.0,0.0,0.0)
            let hor4 = V4f(1.4, 0.0,0.0,0.0)
                
            let parameters = [|hor1;hor2;
                               hor3;hor4|]
    
            let mapping = DataMapping(origin = Cell2d(4L, 4L, -2), size = V2i(2, 2))
    
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
    
            qtree
        let createOneCell0 = 
            // define mapping of raw data to raster space
            let hor1 = V4f(3.0, 0.0,0.0,0.0)
                    
            let parameters = [|hor1|]//;hor1;hor1;hor1|]
            
            let mapping = DataMapping(origin = Cell2d(1L, 1L, 0), size = V2i(1, 1))
            
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                    
            // build the quadtree (incl. levels-of-detail)
                    
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
            
            qtree

        let createOneCell1 = 
            // define mapping of raw data to raster space
            let hor1 = V4f(5.0, 0.0,0.0,0.0)
                    
            let parameters = [|hor1|]//;hor1;hor1;hor1|]
            
            let mapping = DataMapping(origin = Cell2d(0L, 0L, 1), size = V2i(1, 1))
            
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                    
            // build the quadtree (incl. levels-of-detail)
                    
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
            
            qtree

    
        let mainTree = createQuadTreePlanesWithNaN
        let subTree = createOverlap 
            
        let mutable newTree = Quadtree.Merge SecondDominates mainTree subTree
    
        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let qtreeCells = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr)
        
        
        let subTree0 = createOneCell0 
        
        newTree <- Quadtree.Merge SecondDominates newTree subTree0
    
        let config = Query.Config.Default  
        let resultCells0 = newTree |> Query.All config
        let qtreeCells0 = resultCells0  |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                        |> Seq.collect (fun arr -> arr)


        let subTree1 = createOneCell1 
        
        newTree <- Quadtree.Merge SecondDominates newTree subTree1
    
        let config = Query.Config.Default  
        let resultCells1 = newTree |> Query.All config
        let qtreeCells1 = resultCells1  |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                        |> Seq.collect (fun arr -> arr)
        ()