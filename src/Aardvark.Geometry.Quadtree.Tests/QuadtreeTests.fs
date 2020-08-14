module QuadtreeTests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base

let private createQuadtree (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int) =
    let size = V2i(w, h)
    let xs = Array.zeroCreate<float32> (w * h)
    for y = 0 to size.Y - 1 do
        for x = 0 to size.X - 1 do
            let i = y * size.X + x
            xs.[i] <- float32 x + float32 y / 100.0f

    let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))

    let config = { BuildConfig.Default with SplitLimit = splitLimit }
    Quadtree.Build config [| a |]


[<Fact>]
let ``Build1`` () =

    let q = createQuadtree 0 0 10 7 0 4
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Merge_NonOverlapping_Adjacent_SameDepth`` () =

    let q00 = createQuadtree 0 0 8 8 0 4
    let q10 = createQuadtree 8 0 8 8 0 4
    let q01 = createQuadtree 0 8 8 8 0 4
    let q11 = createQuadtree 8 8 8 8 0 4

    Assert.True(q00.Cell = Cell2d(0L, 0L, 3))
    Assert.True(q10.Cell = Cell2d(1L, 0L, 3))
    Assert.True(q01.Cell = Cell2d(0L, 1L, 3))
    Assert.True(q11.Cell = Cell2d(1L, 1L, 3))

    let m1 = Merge q00 q10
    Assert.True(Quadtree.CountLeafs m1 = 8)
    Assert.True(Quadtree.CountNodes m1 = Quadtree.CountInner m1 + Quadtree.CountLeafs m1)

    let m2 = Merge m1 q01
    Assert.True(Quadtree.CountLeafs m2 = 12)
    Assert.True(Quadtree.CountNodes m2 = Quadtree.CountInner m2 + Quadtree.CountLeafs m2)

    let m = Merge m2 q11
    Assert.True(Quadtree.CountLeafs m = 16)
    Assert.True(Quadtree.CountNodes m = Quadtree.CountInner m + Quadtree.CountLeafs m)
    Assert.True(m.Cell = Cell2d(0L,0L,4))

    ()

[<Fact>]
let ``Merge_NonOverlapping_Adjacent_DifferentDepth`` () =

    let q00 = createQuadtree 0 0 8 8 0 8
    let q10 = createQuadtree 8 0 8 8 0 4
    let q01 = createQuadtree 0 8 8 8 0 2
    let q11 = createQuadtree 8 8 8 8 0 1

    Assert.True(Quadtree.CountLeafs q00 = 1)
    Assert.True(Quadtree.CountLeafs q10 = 4)
    Assert.True(Quadtree.CountLeafs q01 = 16)
    Assert.True(Quadtree.CountLeafs q11 = 64)

    let m1 = Merge q00 q10
    Assert.True(Quadtree.CountLeafs m1 = 5)

    let m2 = Merge m1 q01
    Assert.True(Quadtree.CountLeafs m2 = 21)

    let m = Merge m2 q11
    Assert.True(Quadtree.CountLeafs m = 85)
    Assert.True(m.Cell = Cell2d(0L,0L,4))

    ()
