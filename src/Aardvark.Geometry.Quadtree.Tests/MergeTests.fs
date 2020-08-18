module MergeTests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base
open Aardvark.Data

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
let ``Merge_NonOverlapping_NonAdjacent_SameDepth`` () =

    let q00 = createQuadtree 0 0 1 1 0 4
    let q10 = createQuadtree 7 0 1 1 0 4
    let q01 = createQuadtree 0 6 1 1 0 4
    let q11 = createQuadtree 5 7 1 1 0 4

    Assert.True(q00.Cell = Cell2d(0L, 0L, 0))
    Assert.True(q10.Cell = Cell2d(7L, 0L, 0))
    Assert.True(q01.Cell = Cell2d(0L, 6L, 0))
    Assert.True(q11.Cell = Cell2d(5L, 7L, 0))

    let m1 = Merge q00 q10
    Assert.True(Quadtree.CountLeafs m1 = 2)
    Assert.True(Quadtree.CountNodes m1 = Quadtree.CountInner m1 + Quadtree.CountLeafs m1)

    let m2 = Merge m1 q01
    Assert.True(Quadtree.CountLeafs m2 = 3)
    Assert.True(Quadtree.CountNodes m2 = Quadtree.CountInner m2 + Quadtree.CountLeafs m2)

    let m = Merge m2 q11
    Assert.True(Quadtree.CountLeafs m = 4)
    Assert.True(Quadtree.CountNodes m = Quadtree.CountInner m + Quadtree.CountLeafs m)
    Assert.True(m.Cell = Cell2d(0L,0L,3))

    ()

[<Fact>]
let ``Merge_NonOverlapping_Adjacent_DifferentExp`` () =

    let q00 = createQuadtree 0 0 2 2 2 2
    let q10 = createQuadtree 4 0 4 4 1 2
    let q01 = createQuadtree 0 8 8 8 0 2
    let q11 = createQuadtree 16 16 16 16 -1 2

    let bb00 = q00.SampleWindowBoundingBox
    let bb10 = q10.SampleWindowBoundingBox
    let bb01 = q01.SampleWindowBoundingBox
    let bb11 = q11.SampleWindowBoundingBox

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

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth`` () =

    let a = createQuadtree 0 0 1 1 0 1
    let b = createQuadtree 0 0 1 1 0 1

    Assert.True(Quadtree.CountLeafs a = 1)
    Assert.True(Quadtree.CountLeafs b = 1)

    let m = Merge a b
    Assert.True(Quadtree.CountLeafs m = 1)

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_DifferentDepth_1`` () =

    let a = createQuadtree 0 0 1 1 0 1
    let b = createQuadtree 0 0 2 2 -1 1

    Assert.True(Quadtree.CountLeafs a = 1)
    Assert.True(Quadtree.CountLeafs b = 4)

    let m = Merge a b
    let mLeafCount = Quadtree.CountLeafs m
    Assert.True((mLeafCount = 4))

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_DifferentDepth_2`` () =

    let a = createQuadtree 0 0 2 2 -1 1
    let b = createQuadtree 0 0 1 1 0 1

    Assert.True(Quadtree.CountLeafs a = 4)
    Assert.True(Quadtree.CountLeafs b = 1)

    let m = Merge a b
    Assert.True(Quadtree.CountLeafs m = 4)

    ()