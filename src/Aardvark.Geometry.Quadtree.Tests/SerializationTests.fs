namespace Aardvark.Geometry.Quadtree.Tests

open Aardvark.Geometry.Quadtree
open Aardvark.Base
open Aardvark.Data
open System
open System.Collections.Generic
open Xunit

module SerializationTests =

    [<Fact>]
    let ``C3b array serialization`` () =

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
    let ``Quadtree serialization single node`` () =

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
    let ``Quadtree serialization multiple nodes`` () =

        let size = V2i(1500,1000)
        let data = Array.zeroCreate<float32> (size.X * size.Y)
        let mapping = DataMapping(origin = V2l(500_000, 2_000), size = size, exponent = 0)
        let layer = Layer(Defs.Heights1f, data, mapping)
        let qtree = Quadtree.Build BuildConfig.Default [| layer |]

    
        let options = SerializationOptions.NewInMemoryStore(verbose = true)
        let id = qtree |> Quadtree.Save options

        ()

    [<Fact>]
    let ``Quadtree serialization merged nodes`` () =

        let a = createQuadtreeWithValue 0 0 1 1  0 8<powerOfTwo> 10.0f
        let b = createQuadtreeWithValue 1 0 2 1 -1 8<powerOfTwo> 20.0f
        let m = Quadtree.Merge MoreDetailedOrSecond a b

        let options = SerializationOptions.NewInMemoryStore(verbose = true)
        let id = m |> Quadtree.Save options

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
    let ``Quadtree serialization single node roundtrip`` () =

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
    let ``Quadtree serialization multiple nodes roundtrip`` () =

        let size = V2i(1500,1000)
        let data = Array.zeroCreate<float32> (size.X * size.Y)
        let mapping = DataMapping(origin = V2l(500_000, 2_000), size = size, exponent = 0)
        let layer = Layer(Defs.Heights1f, data, mapping)
        let qtree = Quadtree.Build BuildConfig.Default [| layer |]

        // roundtrip
        roundTrip qtree

    [<Fact>]
    let ``Quadtree serialization merged nodes roundtrip`` () =

        let a = createQuadtreeWithValue 0 0 1 1  0 8<powerOfTwo> 10.0f
        let b = createQuadtreeWithValue 1 0 2 1 -1 8<powerOfTwo> 20.0f
        let m = Quadtree.Merge MoreDetailedOrSecond a b

        // roundtrip
        roundTrip m

        ()