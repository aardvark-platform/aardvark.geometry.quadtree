namespace Aardvark.Geometry.Quadtree.Tests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base

module LayerTests =

    let private createLayer (ox : int) (oy : int) (w : int) (h : int) (e : int) =
        Layer(Defs.Heights1f, Array.zeroCreate<float32> (w * h), DataMapping(V2l(ox, oy), V2i(w, h), exponent = e))

    [<Fact>]
    let ``ILayer_CellBounds_Centered`` () =
        Assert.True((createLayer -2 -2 4 4 0).CellBounds = Cell2d(2))
        Assert.True((createLayer -1 -1 2 2 0).CellBounds = Cell2d(1))

    [<Fact>]
    let ``ILayer_CellBounds_MaxOnCellBoundary`` () =
        Assert.True((createLayer 0 0 4 4 0).CellBounds = Cell2d(0L, 0L, 2))
        Assert.True((createLayer 4 4 4 4 -1).CellBounds = Cell2d(1L, 1L, 1))

    [<Fact>]
    let ``ILayer_CellBounds_Unaligned`` () =
        Assert.True((createLayer 0 0 10 7 0).CellBounds = Cell2d(0L, 0L, 4))

    [<Fact>]
    let ``ILayer_Materialize_Masked`` () =
        // sample 12 is masked out, window starts at 11 (not at buffer origin 10)
        let a = Layer(Defs.Heights1d, [| 0.0; 1.0; 2.0; 3.0 |], DataMapping(V2l(10, 0), V2i(4, 1), 0), Some [| 1uy; 1uy; 0uy; 1uy |])
        let b = (a.WithWindow (Box2l(V2l(11, 0), V2l(14, 1)))).Value :?> Layer<float>
        let m = b.Materialize()
        Assert.True(m.Mapping.Window = Box2l(V2l(11, 0), V2l(14, 1)))
        Assert.True(((m :> ILayer).Mask = Some [| 1uy; 0uy; 1uy |]))
        Assert.True(m.GetSample(Fail, Cell2d(11L, 0L, 0)) = 1.0)
        Assert.True(m.GetSample(Fail, Cell2d(13L, 0L, 0)) = 3.0)

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