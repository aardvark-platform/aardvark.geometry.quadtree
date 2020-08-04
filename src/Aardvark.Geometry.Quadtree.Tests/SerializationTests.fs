module SerializationTests

open Aardvark.Geometry.Quadtree
open Aardvark.Base
open System.IO
open Xunit
open Aardvark.Data
open System.Collections.Generic
open System

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

    let store = Dictionary<Guid, byte[]>()
    let options = { 
        Save = fun id buffer ->
            store.[id] <- buffer
            printfn "[SAVE] %A %d" id buffer.Length
        }

    let buffer = qtree.Serialize options

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

    let store = Dictionary<Guid, byte[]>()
    let options = { 
        Save = fun id buffer ->
            store.[id] <- buffer
            printfn "[SAVE] %A %d" id buffer.Length
        }

    qtree.Serialize options

    //Assert.True(a.Mapping.BufferOrigin = b.Mapping.BufferOrigin)
    //Assert.True(a.Mapping.BufferSize   = b.Mapping.BufferSize)
    //Assert.True(a.Mapping.Window       = b.Mapping.Window)
    ()