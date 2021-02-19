namespace Aardvark.Geometry.Quadtree.Tests

open Aardvark.Base
open Aardvark.Data
open Aardvark.Geometry.Quadtree
open Aardvark.Geometry.Quadtree.Serialization
open Xunit
open System
open System.Threading

module SerializationTests =

    [<Fact>]
    let ``C3b array roundtrip`` () =

        let colors = [| 
            C3b(255,0,0); C3b(255,1,0); C3b(255,2,0); C3b(255,3,0)
            C3b(0,255,1); C3b(1,255,1); C3b(2,255,1); C3b(3,255,1)
            C3b(0,2,255); C3b(1,2,255); C3b(2,2,255); C3b(3,2,255)
            |]

        let buffer = DurableCodec.Serialize(Defs.Colors3b, colors)

        let struct (def, b) = DurableCodec.Deserialize buffer

        Assert.True(Defs.Colors3b = def)

        let data = b :?> C3b[]
        Assert.True(data.Length = colors.Length)
        ()

    [<Fact>]
    let ``Quadtree save single node`` () =

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

        // serialize
        let options = SerializationOptions.NewInMemoryStore(verbose = true)
        let id = qtree |> Quadtree.Save options

        //Assert.True(a.Mapping.BufferOrigin = b.Mapping.BufferOrigin)
        //Assert.True(a.Mapping.BufferSize   = b.Mapping.BufferSize)
        //Assert.True(a.Mapping.Window       = b.Mapping.Window)
        ()

    [<Fact>]
    let ``Quadtree save multiple nodes`` () =

        let size = V2i(1500,1000)
        let data = Array.zeroCreate<float32> (size.X * size.Y)
        let mapping = DataMapping(origin = V2l(500_000, 2_000), size = size, exponent = 0)
        let layer = Layer(Defs.Heights1f, data, mapping)
        let qtree = Quadtree.Build BuildConfig.Default [| layer |]

    
        let options = SerializationOptions.NewInMemoryStore(verbose = true)
        let id = qtree |> Quadtree.Save options

        ()

    [<Fact>]
    let ``Quadtree save merged nodes`` () =

        let a = createQuadtreeWithValue 0 0 1 1  0 8<powerOfTwo> 10.0f
        let b = createQuadtreeWithValue 1 0 2 1 -1 8<powerOfTwo> 20.0f
        let m = Quadtree.Merge MoreDetailedOrSecond a b

        let options = SerializationOptions.NewInMemoryStore(verbose = true)
        let id = m |> Quadtree.Save options

        ()

    [<Fact>]
    let ``Quadtree save linked node`` () =

        let a = createQuadtreeWithValue 0 0 1 1  0 8<powerOfTwo> 10.0f
        let customid = Guid.NewGuid()
        let l = Quadtree.Link customid a

        let options = SerializationOptions.NewInMemoryStore(verbose = true)
        let id = l |> Quadtree.Save options

        ()


    let private roundTrip (qtree : QNodeRef) =

        let options1 = SerializationOptions.NewInMemoryStore(verbose = true)
        let id1 = qtree |> Quadtree.Save options1
        let buffer1 = options1.TryLoad id1

        let reloaded = Quadtree.Load options1 id1

        let options2 = SerializationOptions.NewInMemoryStore(verbose = true)
        let id2 = reloaded |> Quadtree.Save options2
        let buffer2 = options2.TryLoad id2

        buffer1.Value.Length = buffer2.Value.Length     |> Assert.True

    [<Fact>]
    let ``Quadtree roundtrip single node`` () =

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

        // roundtrip
        roundTrip qtree


    [<Fact>]
    let ``Quadtree roundtrip multiple nodes`` () =

        let size = V2i(1500,1000)
        let data = Array.zeroCreate<float32> (size.X * size.Y)
        let mapping = DataMapping(origin = V2l(500_000, 2_000), size = size, exponent = 0)
        let layer = Layer(Defs.Heights1f, data, mapping)
        let qtree = Quadtree.Build BuildConfig.Default [| layer |]

        // roundtrip
        roundTrip qtree

    [<Fact>]
    let ``Quadtree roundtrip merged nodes`` () =

        let a = createQuadtreeWithValue 0 0 1 1  0 8<powerOfTwo> 10.0f
        let b = createQuadtreeWithValue 1 0 2 1 -1 8<powerOfTwo> 20.0f
        let m = Quadtree.Merge MoreDetailedOrSecond a b

        // roundtrip
        roundTrip m

        ()

    [<Fact>]
    let ``Quadtree roundtrip linked node`` () =

        let a = createQuadtreeWithValue 0 0 1 1  0 8<powerOfTwo> 10.0f
        let customid = Guid.NewGuid()
        let l = Quadtree.Link customid a

        // roundtrip
        roundTrip l

        ()



    [<Fact>]
    let ``Quadtree load obsolete`` () =

        let options = SerializationOptions.SimpleDiskStore(@"T:\Vgm\Data\Raster\20201210_obsolete_quadtree_store")

        let key1 = Guid("ae0aeb3e-3444-44bd-9aaf-c005d4e39f89")
        let q1 = Quadtree.Load options key1

        Assert.True(match q1 with | NoNode -> false | _ -> true)
        let xs1 = Query.All Query.Config.Default q1 |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
        Assert.True(xs1.Length > 0)


        let key2 = Guid("b2706ae3-36b1-4545-8742-eb706957a915")
        let q2 = Quadtree.Load options key2

        Assert.True(match q2 with | NoNode -> false | _ -> true)
        let xs2 = Query.All Query.Config.Default q2 |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
        Assert.True(xs2.Length > 0)

        ()

    [<Fact>]
    let ``Quadtree export`` () =
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

        let a = createQuadtreeWithValue    0    0 5000 3000  0 8 10.0f
        let b = createQuadtreeWithValue 5000 3000 1000 1000 -1 8 20.0f
        let m = Quadtree.Merge SecondDominates a b

        let nodeCount = m |> Quadtree.CountNodes true

        let store = new Uncodium.SimpleStore.SimpleMemoryStore()
        let so = SerializationOptions.SimpleStore store
        let id = Quadtree.Save so m

        do
            let source = so
            let target = SerializationOptions.NewInMemoryStore()
            let progress (x, total) = printf "\r[progress] %d/%d" x total
            id |> Quadtree.Export source target (Some progress) CancellationToken.None
            printfn ""

            let exportNodeCount = id |> Quadtree.Load target |> Quadtree.EnumerateKeys true |> Seq.length
            exportNodeCount = nodeCount |> Assert.True