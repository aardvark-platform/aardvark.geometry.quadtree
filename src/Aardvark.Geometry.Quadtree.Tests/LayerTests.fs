namespace Aardvark.Geometry.Quadtree.Tests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base

module LayerTests =

    [<Fact>]
    let ``ILayer_Window`` () =
        let a = Layer(Defs.Heights1f, Array.zeroCreate<float32> 200, DataMapping(V2l(1000, 500), V2i(20, 10), 0))
        Assert.True(a.Mapping.Window = Box2l(V2l(1000,500), V2l(1020,510)))

        let b = (a.WithWindow (Box2l(V2l(1000,500), V2l(1020, 510)))).Value
        Assert.True(b.Mapping.Window = Box2l(V2l(1000,500), V2l(1020, 510)))

    [<Fact>]
    let ``ILayer_Merge`` () =
        let a = Layer(Defs.Heights1f, Array.create 200 1.23f, DataMapping(V2l(1000, 500), V2i(20, 10), 0))
        let b = Layer(Defs.Heights1f, Array.create 200 4.56f, DataMapping(V2l(1020, 500), V2i(20, 10), 0))
        let m = seq {a :> ILayer; b :> ILayer} |> Layer.Merge
        Assert.True(m.IsSome)
    
        let m = m.Value
        Assert.True(a.Def = b.Def && b.Def = m.Def)
        Assert.True(m.Mapping.Window.Area = a.Mapping.Window.Area + b.Mapping.Window.Area)
        Assert.True(m.Mapping.Window = Box2l(V2l(1000,500),V2l(1040,510)))