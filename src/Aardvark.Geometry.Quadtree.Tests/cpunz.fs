namespace Aardvark.Geometry.Quadtree.Tests

open Aardvark.Base
open Aardvark.Geometry.Quadtree
open Aardvark.Geometry.Quadtree.PrettyPrint
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
        //let newTree = Quadtree.Merge SecondDominates mainTree subTree
        //let (_, _, x0) = get pos mainTree
        //let (_, _, x1) = get pos subTree

        let test target merged =
            let get (pos : V2d) root = Sample.PositionTyped<V4f> Query.Config.Default pos Defs.HeightsBilinear4f root
            let pos = V2d(0.5, 0.5)
            let x = (get pos merged).Value
            printfn "sample %A; target %A; %A" x target (x = target) 


        //showHtmlDebugView<V4f> "cpunz_20201105" Defs.HeightsBilinear4f [
        //    ("mainTree", mainTree)
        //    ("subTree", subTree)
        //    ("Quadtree.Merge FirstDominates  mainTree subTree", Quadtree.Merge FirstDominates  mainTree subTree)
        //    ("Quadtree.Merge SecondDominates mainTree subTree", Quadtree.Merge SecondDominates mainTree subTree)
        //    ("Quadtree.Merge FirstDominates  subTree mainTree", Quadtree.Merge FirstDominates  subTree mainTree)
        //    ("Quadtree.Merge SecondDominates subTree mainTree", Quadtree.Merge SecondDominates subTree mainTree)
        //    ]
    
        Quadtree.printStructure true mainTree
        mainTree    |> test (V4f(1.0f, 0.0f, 0.0f, 0.0f))
        subTree     |> test (V4f(1.5f, 1.0f, 0.0f, 0.0f))

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

        //showHtmlDebugView<V4f> "cpunz_20201130" Defs.VolumesBilinear4f [
        //    ("mainTree", mainTree)
        //    ("subTree", subTree)
        //    ("newTree = Quadtree.Merge SecondDominates mainTree subTree", newTree)
        //    ]

        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let samples = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
        samples |> printfn "%A"
               
        //let ptsNormals = getCenterDataOfCells qtreeCells
        //                                        |> Seq.toArray

        ()

    let hor1_main = V4f(1.0, 0.0,0.0,0.0)
    let oblique1_main = V4f(1.5, 1.0,0.0,0.0)
    let nanVal = V4f(nan, nan, nan, nan)  

    let createQuadTreePlanesWithNaN (exponent:int) =
        
            // define mapping of raw data to raster space
            
            let parameters = [|nanVal; nanVal; nanVal; 
                               hor1_main; oblique1_main; nanVal;
                               hor1_main; oblique1_main; nanVal;
                               |]
        
            let mapping = DataMapping(origin = Cell2d(0L, 0L, exponent), size = V2i(3, 3))
        
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build { BuildConfig.Default with SplitLimitPowerOfTwo=8 } [| bilinParameters |]
        
            qtree
   
    
    [<Fact>]
    let ``punz_double_merge_volume`` () =
        let hor3 = V4f(3.0, 0.0,0.0,0.0)
        let hor11 = V4f(1.1, 0.0,0.0,0.0)
        let hor12 = V4f(1.2, 0.0,0.0,0.0)
        let hor13 = V4f(1.3, 0.0,0.0,0.0)
        let hor14 = V4f(1.4, 0.0,0.0,0.0)
            
        let createOneCell = 
            // define mapping of raw data to raster space
            
                    
            let parameters = [|hor3|]//;hor1;hor1;hor1|]
            
            let mapping = DataMapping(origin = Cell2d(4L, 4L, -1), size = V2i(1, 1))
            
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                    
            // build the quadtree (incl. levels-of-detail)
                    
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
            
            qtree
    
        let createOneSubCell4 = 
            // define mapping of raw data to raster space
            
            let parameters = [|hor11;hor12;
                               hor13;hor14|]
    
            let mapping = DataMapping(origin = Cell2d(2L, 4L, -2), size = V2i(2, 2))
    
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
    
            qtree
    
            
        let mainTree = createQuadTreePlanesWithNaN 0
        let subTree = createOneCell 
            
        let newTree = Quadtree.Merge SecondDominates mainTree subTree
    
        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let qtreeCells = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr)

        Assert.True((qtreeCells |> Seq.length) = 12 )
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(0,0,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(1,0,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,0,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,1,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(0,1,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(0,2,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(1,1,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(1,2,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(5,4,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(5,5,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(4,5,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor3) && elemCell.Equals(Cell2d(4,4,-1))))

        let subTree4 = createOneSubCell4
        let newTree4 = Quadtree.Merge SecondDominates newTree subTree4
        let resultCells4 = newTree4 |> Query.All config
        let qtreeCells4 = resultCells4 |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f)) 
                                       |> Seq.collect (fun arr -> arr) |> Seq.toArray
        

        //for (c,x) in qtreeCells4 do printfn "%A -> %A" c x
        //showHtmlDebugView<V4f> "punz_double_merge_volume" Defs.VolumesBilinear4f [
        //    ("mainTree", mainTree)
        //    ("subTree", subTree)
        //    ("newTree = Quadtree.Merge SecondDominates mainTree subTree", newTree)
        //    ("subTree4", subTree4)
        //    ("newTree4 = Quadtree.Merge SecondDominates newTree subTree4", newTree4)
        //    ]

        
        //Assert.True((qtreeCells4 |> Seq.length) = 18 )
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(0,0,0))))                      // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(1,0,0))))                      // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,0,0))))                      // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,1,0))))                      // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(0,2,0))))        // OK

        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(0,2,-1))))       // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(0,3,-1))))       // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(1,3,-1))))       // OK

        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor11) && elemCell.Equals(Cell2d(2,4,-2))))           // OK    
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor12) && elemCell.Equals(Cell2d(3,4,-2))))           // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor13) && elemCell.Equals(Cell2d(2,5,-2))))           // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor14) && elemCell.Equals(Cell2d(3,5,-2))))           // OK


        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(1,1,0))))    // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(1,2,0))))    // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(5,4,-1))))                     // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(5,5,-1))))                     // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(4,5,-1))))                     // OK
        Assert.True(qtreeCells4 |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor3) && elemCell.Equals(Cell2d(4,4,-1))))            // OK
        
        
        ()

    open PrettyPrint
    let generatePrettyPrintTable () =
    
        let f = { HAlign=Center; VAlign=Middle; Bgcolor=C3b.White}
        
        let cells = 
            Group(
                {X=0;Y=0}, f, "Label 1", [
                    Text({X=0; Y=0}, f, "Hello World!")
                    Text({X=1; Y=0}, f, "(1,0)")
                    Text({X=0; Y=1}, f, "(0,1)")
                    Text({X=3; Y=4}, f, "(3,4)")
                    Group(
                        {X=2;Y=3}, f, "Label 2", [
                            Text({X=0; Y=0}, f, "Hello World!")
                            Text({X=1; Y=0}, f, "(1,0)")
                            Text({X=0; Y=1}, f, "(0,1)")
                            Text({X=3; Y=4}, f, "(3,4)")
                        ])
                ])
    
        File.WriteAllLines(@"T:\index.html", cells |> Cells.toHtml)
    
        ()

    open PrettyPrint

    [<Fact>]
    let ``punz_merge_verySmall_into_coarse_volume`` () =
        let hor3 = V4f(3.0, 0.0,0.0,0.0)
        let createOneCell = 
            // define mapping of raw data to raster space
            
                    
            let parameters = [|hor3|]//;hor1;hor1;hor1|]
            
            let mapping = DataMapping(origin = Cell2d(4L, 4L, -2), size = V2i(1, 1))
            
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                    
            // build the quadtree (incl. levels-of-detail)
                    
            let qtree = Quadtree.Build  { BuildConfig.Default with SplitLimitPowerOfTwo=8 } [| bilinParameters |]
            
            qtree
    
    
        let mainTree = createQuadTreePlanesWithNaN 0
        let subTree = createOneCell 
            
        let newTree = Quadtree.Merge SecondDominates mainTree subTree
        //Quadtree.printStructure newTree

        let config = Query.Config.Default  
        let results = newTree |> Query.All { config with Verbose = false } |> Seq.toArray
        let foo = results |> Seq.map (fun x -> x.GetSampleCells ()) |> Seq.toArray
        let qtreeCells = results |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr) |> Seq.toArray
                   
    
        for (c,x) in qtreeCells do printfn "%A -> %A" c x

        //showHtmlDebugView<V4f> "punz_merge_verySmall_into_coarse_volume" Defs.VolumesBilinear4f [
        //    ("mainTree", mainTree)
        //    ("subTree", subTree)
        //    ("newTree = Quadtree.Merge SecondDominates mainTree subTree", newTree)
        //    ]
    

        Assert.True(qtreeCells.Length = 15)

        let check cell value = Assert.True(qtreeCells |> Seq.exists(fun (c,x) -> x = value && c = cell))
        let checkNan cell    = qtreeCells |> Seq.exists (fun (c,x) -> c = cell && x.X.IsNaN()) |> Assert.True

        checkNan (Cell2d(0,0,0))
        checkNan (Cell2d(1,0,0))
        checkNan (Cell2d(2,0,0))
        checkNan (Cell2d(2,1,0))
        checkNan (Cell2d(2,2,0))

        check (Cell2d(0,1,0)) hor1_main
        check (Cell2d(0,2,0)) hor1_main
        
        // Christian Punz: hier wäre so ein Fall, wo bei den "neuen" Subzellen für mich nicht ganz nachvollziehbare Datenwerte entstehen,...
        // ev. sollte einfach der alte von der übergeordneten Zelle übernommen werden
        // die Deluxe Variante wäre ein Resampling der Werte, aber das hat jetzt keine hohe Priorität
        check (Cell2d(1,2,0)) oblique1_main

        let avg = (hor3 + oblique1_main + oblique1_main + oblique1_main) / 4.0f

        check (Cell2d(3,2,-1)) oblique1_main
        check (Cell2d(3,3,-1)) oblique1_main
        check (Cell2d(2,3,-1)) oblique1_main

        check (Cell2d(4,4,-2)) hor3
        
        check (Cell2d(4,5,-2)) oblique1_main
        check (Cell2d(5,5,-2)) oblique1_main
        check (Cell2d(5,4,-2)) oblique1_main
      
        ()

    [<Fact>]
    let ``punz_merge_withOverlap_within_other_volume`` () =
        
        let hor11 = V4f(1.1, 0.0,0.0,0.0)
        let hor12 = V4f(1.2, 0.0,0.0,0.0)
        let hor13 = V4f(1.3, 0.0,0.0,0.0)
        let hor14 = V4f(1.4, 0.0,0.0,0.0)

        let createOverlap = 
            // define mapping of raw data to raster space
            
                
            let parameters = [|hor11;hor12;
                               hor13;hor14|]
    
            let mapping = DataMapping(origin = Cell2d(3L, 3L, -2), size = V2i(2, 2))
    
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
    
            qtree
    
    
        let mainTree = createQuadTreePlanesWithNaN 0
        let subTree = createOverlap 
            
        let newTree = Quadtree.Merge SecondDominates mainTree subTree
    
        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let qtreeCells = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr)
                                     |> Array.ofSeq


        Assert.True((qtreeCells |> Seq.length) = 33 )

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(0,0,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(0,1,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(1,0,-1))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,0,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(3,0,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(3,1,-1))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,0,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,1,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,2,0))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(0,2,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(0,3,-1))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(3,1,-1)))) // wrong
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(1,3,-1))))


        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(0,2,0))))
        
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(1,2,0))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(3,2,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(3,3,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(2,3,-1))))


        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor11) && elemCell.Equals(Cell2d(3,3,-2))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(3,2,-2))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,3,-2))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,2,-2))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor12) && elemCell.Equals(Cell2d(4,3,-2))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(4,2,-2))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(5,2,-2))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(5,3,-2))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor14) && elemCell.Equals(Cell2d(4,4,-2))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(4,5,-2))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(5,5,-2))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(5,4,-2))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor13) && elemCell.Equals(Cell2d(3,4,-2))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(3,5,-2))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(2,5,-2))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(5,2,-2)))) // wrong
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(2,4,-2))))
        
        ()

    [<Fact>]
    let ``punz_merge_withOverlap_overboarder_other_volume`` () =
        let hor11 = V4f(1.1, 0.0,0.0,0.0)
        let hor12 = V4f(1.2, 0.0,0.0,0.0)
        let hor13 = V4f(1.3, 0.0,0.0,0.0)
        let hor14 = V4f(1.4, 0.0,0.0,0.0)

        let createOverlap = 
            // define mapping of raw data to raster space
            
                
            let parameters = [|hor11;hor12;
                               hor13;hor14|]
    
            let mapping = DataMapping(origin = Cell2d(5L, 5L, -1), size = V2i(2, 2))
    
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
    
            qtree
    
    
        let mainTree = createQuadTreePlanesWithNaN 0
        let subTree = createOverlap 
            
        let newTree = Quadtree.Merge SecondDominates mainTree subTree
    

        


        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let qtreeCells = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr)
                                     |> Seq.toArray

        //showHtmlDebugView<V4f> "punz_merge_withOverlap_overboarder_other_volume" Defs.VolumesBilinear4f [
        //    ("mainTree", mainTree)
        //    ("subTree", subTree)
        //    ("newTree = Quadtree.Merge SecondDominates mainTree subTree", newTree)
        //    ]
        //Quadtree.printStructure newTree
        //printfn ""
        //for (c,x) in qtreeCells do printfn "%A -> %A" c x
                   
        
        
        //Assert.True((qtreeCells |> Seq.length) = 28 ) // original cpunz
        Assert.True((qtreeCells |> Seq.length) = 15 )

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(0,0,0))))
        
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(1,0,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,0,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(2,1,0))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(0,1,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor1_main) && elemCell.Equals(Cell2d(0,2,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(1,1,0))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(oblique1_main) && elemCell.Equals(Cell2d(1,2,0))))

        // new cells

        // [2020-12-09 sm] no additional/new cells are generated outside the area covered by original data

        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(3,0,0))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(3,1,0))))

        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(0,3,0))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(1,3,0))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor11) && elemCell.Equals(Cell2d(5,5,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(4,4,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(5,4,-1))))
        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(4,5,-1))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor12) && elemCell.Equals(Cell2d(6,5,-1))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(6,4,-1))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(7,4,-1))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(7,5,-1))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor14) && elemCell.Equals(Cell2d(6,6,-1))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(7,6,-1))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(6,7,-1))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(7,7,-1))))

        Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.ApproximateEquals(hor13) && elemCell.Equals(Cell2d(5,6,-1))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(4,6,-1))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(4,7,-1))))
        //Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(nanVal) && elemCell.Equals(Cell2d(5,7,-1))))

        

        ()

    [<Fact>]
    let ``punz_merge_withOverlap_overboarder_overOrigin_other_volume`` () =
        
        let hor11 = V4f(1.1, 0.0,0.0,0.0)
        let hor12 = V4f(1.2, 0.0,0.0,0.0)
        let hor13 = V4f(1.3, 0.0,0.0,0.0)
        let hor14 = V4f(1.4, 0.0,0.0,0.0)

        let createOverlap = 
            // define mapping of raw data to raster space
            
                
            let parameters = [|hor11;hor12;
                               hor13;hor14|]
    
            let mapping = DataMapping(origin = Cell2d(-1L, -1L, -1), size = V2i(2, 2))
    
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
    
            qtree
    
    
        let mainTree = createQuadTreePlanesWithNaN 0
        let subTree = createOverlap 

       
        let newTree = Quadtree.Merge SecondDominates mainTree subTree
    
        let config = Query.Config.Default  
        let resultCells = newTree |> Query.All config
        let qtreeCells = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr)
                                     |> Seq.toArray
                   
        //showHtmlDebugView<V4f> "punz_merge_withOverlap_overboarder_overOrigin_other_volume" Defs.VolumesBilinear4f [
        //    ("mainTree", mainTree)
        //    ("subTree", subTree)
        //    ("newTree = Quadtree.Merge SecondDominates mainTree subTree", newTree)
        //    ]
        //Quadtree.printStructure newTree
        //printfn ""
        //for (c,x) in qtreeCells do printfn "%A -> %A" c x
            


        //Assert.True((qtreeCells |> Seq.length) = 28 ) // original cpunz
        Assert.True((qtreeCells |> Seq.length) = 15 )
        
        
        // [2020-12-09 sm] no additional/new cells are generated outside the area covered by original data

        let check cell value = Assert.True(qtreeCells |> Seq.exists(fun (elemCell,elemV4f) -> elemV4f.Equals(value) && elemCell.Equals(cell)))
        let checkmany xs = for ((x,y,e), v) in xs do check (Cell2d(int64 x, int64 y, e)) v

        checkmany [
            ((1,0,0), nanVal) 
            ((2,0,0), nanVal) 
            ((2,1,0), nanVal) 
            ((2,2,0), nanVal) 
                    
            ((0,1,0), hor1_main)         
            ((0,2,0), hor1_main)        
            ((1,1,0), oblique1_main)  
            ((1,2,0), oblique1_main)   

            ((0,0,-1), hor14)  
            ((1,0,-1), nanVal) 
            ((1,1,-1), nanVal) 
            ((0,1,-1), nanVal) 

            ((-1,0,-1), hor13)
            ((-1,-1,-1), hor11) 
            ((0,-1,-1), hor12)
        ]

    [<Fact>]
    let ``punz_merge_2_levels``()=
        let createOneSubCell (level : int) (east:int64) (north:int64)= 
            // define mapping of raw data to raster space
            let elevation = -1.0*((float)level)
            let hor1 = V4f(elevation, 0.0,0.0,0.0)
            let nanVal = V4f(nan, nan, nan, nan)
            
            let parameters = [|hor1;hor1;hor1;hor1|]

            let mapping = DataMapping(origin = Cell2d(east, north, level), size = V2i(2, 2))

            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
            
            // build the quadtree (incl. levels-of-detail)
            
            let qtree = Quadtree.Build { BuildConfig.Default with SplitLimitPowerOfTwo = 10 } [| bilinParameters |]

            qtree

        let mainTree = createQuadTreePlanesWithNaN -1

        let config = Query.Config.Default  
        let resultCells = mainTree |> Query.All config
        let qtreeCells = resultCells |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                     |> Seq.collect (fun arr -> arr)
                   
        
        Assert.True((qtreeCells |> Seq.length) = 9 )

        let subTree = createOneSubCell -2 0L 2L
        let mutable newTree = Quadtree.Merge SecondDominates mainTree subTree
        
        let resultCells1 = newTree |> Query.All config
        let qtreeCells1 = resultCells1 |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                       |> Seq.collect (fun arr -> arr)
                   
        
        Assert.True((qtreeCells1 |> Seq.length) = 12 )
        let subTree1 = createOneSubCell -3 0L 4L
        //let subNewTree = Quadtree.Merge SecondDominates newTree subSubTree
        newTree <- Quadtree.Merge SecondDominates newTree subTree1
        
        
        let resultCells2 = newTree |> Query.All config
        let qtreeCells2 = resultCells2 |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.VolumesBilinear4f))
                                       |> Seq.collect (fun arr -> arr)
                   
        
        Assert.True((qtreeCells2 |> Seq.length) = 15 )
        //
        ()
    

    [<Fact>]
    let ``punz_2_merge_fine_first_other_volume`` () =
        
        let hor11 = V4f(1.1, 0.0,0.0,0.0)
        let hor12 = V4f(1.2, 0.0,0.0,0.0)
        let hor13 = V4f(1.3, 0.0,0.0,0.0)
        let hor14 = V4f(1.4, 0.0,0.0,0.0)

        let createOverlap = 
            // define mapping of raw data to raster space
            
                
            let parameters = [|hor11;hor12;
                               hor13;hor14|]
    
            let mapping = DataMapping(origin = Cell2d(4L, 4L, -2), size = V2i(2, 2))
    
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                
            // build the quadtree (incl. levels-of-detail)
                
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
    
            qtree
        
        let hor3 = V4f(3.0, 0.0,0.0,0.0)
        let createOneCell0 = 
            // define mapping of raw data to raster space
            
                    
            let parameters = [|hor3|]//;hor1;hor1;hor1|]
            
            let mapping = DataMapping(origin = Cell2d(1L, 1L, 0), size = V2i(1, 1))
            
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                    
            // build the quadtree (incl. levels-of-detail)
                    
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
            
            qtree
        let hor5 = V4f(5.0, 0.0,0.0,0.0)
        let createOneCell1 = 
            // define mapping of raw data to raster space
            
                    
            let parameters = [|hor5|]//;hor1;hor1;hor1|]
            
            let mapping = DataMapping(origin = Cell2d(0L, 0L, 1), size = V2i(1, 1))
            
            // a layer gives meaning to raw data
            let bilinParameters = Layer(Defs.VolumesBilinear4f, parameters, mapping)
                    
            // build the quadtree (incl. levels-of-detail)
                    
            let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]
            
            qtree

    
        let mainTree = createQuadTreePlanesWithNaN 0
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