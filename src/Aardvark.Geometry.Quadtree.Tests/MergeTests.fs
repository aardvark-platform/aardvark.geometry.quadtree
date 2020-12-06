module MergeTests

open Aardvark.Geometry.Quadtree
open Aardvark.Base
open System
open Xunit
open Aardvark.Geometry.Quadtree.PrettyPrint
open System.IO

[<Measure>] type powerOfTwo

let private createQuadtree (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int<powerOfTwo>) =
    let size = V2i(w, h)
    let xs = Array.zeroCreate<float32> (w * h)
    for y = 0 to size.Y - 1 do
        for x = 0 to size.X - 1 do
            let i = y * size.X + x
            xs.[i] <- float32 x + float32 y / 100.0f

    let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))

    let config = { BuildConfig.Default with SplitLimitPowerOfTwo = int splitLimit }
    Quadtree.Build config [| a |]

let private createQuadtreeWithRandomValues (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int<powerOfTwo>) =
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

let private createQuadtreeWithValue (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int<powerOfTwo>) (value : float32) =
    let size = V2i(w, h)
    let xs = Array.zeroCreate<float32> (w * h)
    for y = 0 to size.Y - 1 do
        for x = 0 to size.X - 1 do
            let i = y * size.X + x
            xs.[i] <- value

    let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))

    let config = { BuildConfig.Default with SplitLimitPowerOfTwo = int splitLimit }
    Quadtree.Build config [| a |]

[<Fact>]
let ``Merge_NonOverlapping_Adjacent_SameDepth`` () =

    let q00 = createQuadtree 0 0 8 8 0 2<powerOfTwo>
    let q10 = createQuadtree 8 0 8 8 0 2<powerOfTwo>
    let q01 = createQuadtree 0 8 8 8 0 2<powerOfTwo>
    let q11 = createQuadtree 8 8 8 8 0 2<powerOfTwo>

    Assert.True(q00.Cell = Cell2d(0L, 0L, 3))
    Assert.True(q10.Cell = Cell2d(1L, 0L, 3))
    Assert.True(q01.Cell = Cell2d(0L, 1L, 3))
    Assert.True(q11.Cell = Cell2d(1L, 1L, 3))

    let m1 = Quadtree.Merge SecondDominates q00 q10
    Assert.True(Quadtree.CountLeafs m1 = 8)
    Assert.True(Quadtree.CountNodes m1 = Quadtree.CountInner m1 + Quadtree.CountLeafs m1)

    let m2 = Quadtree.Merge SecondDominates m1 q01
    Assert.True(Quadtree.CountLeafs m2 = 12)
    Assert.True(Quadtree.CountNodes m2 = Quadtree.CountInner m2 + Quadtree.CountLeafs m2)

    let m = Quadtree.Merge SecondDominates m2 q11
    Assert.True(Quadtree.CountLeafs m = 16)
    Assert.True(Quadtree.CountNodes m = Quadtree.CountInner m + Quadtree.CountLeafs m)
    Assert.True(m.Cell = Cell2d(0L,0L,4))

    ()

[<Fact>]
let ``Merge_NonOverlapping_NonAdjacent_SameDepth`` () =

    let q00 = createQuadtree 0 0 1 1 0 2<powerOfTwo>
    let q10 = createQuadtree 7 0 1 1 0 2<powerOfTwo>
    let q01 = createQuadtree 0 6 1 1 0 2<powerOfTwo>
    let q11 = createQuadtree 5 7 1 1 0 2<powerOfTwo>

    Assert.True(q00.Cell = Cell2d(0L, 0L, 2))
    Assert.True(q10.Cell = Cell2d(1L, 0L, 2))
    Assert.True(q01.Cell = Cell2d(0L, 1L, 2))
    Assert.True(q11.Cell = Cell2d(1L, 1L, 2))

    let m1 = Quadtree.Merge SecondDominates q00 q10
    Assert.True(Quadtree.CountLeafs m1 = 2)
    Assert.True(Quadtree.CountNodes m1 = Quadtree.CountInner m1 + Quadtree.CountLeafs m1)

    let m2 = Quadtree.Merge SecondDominates m1 q01
    Assert.True(Quadtree.CountLeafs m2 = 3)
    Assert.True(Quadtree.CountNodes m2 = Quadtree.CountInner m2 + Quadtree.CountLeafs m2)

    let m = Quadtree.Merge SecondDominates m2 q11
    Assert.True(Quadtree.CountLeafs m = 4)
    Assert.True(Quadtree.CountNodes m = Quadtree.CountInner m + Quadtree.CountLeafs m)
    Assert.True(m.Cell = Cell2d(0L,0L,3))

    ()

[<Fact>]
let ``Merge_NonOverlapping_Adjacent_DifferentExp`` () =

    let q00 = createQuadtree 0 0 2 2 2 1<powerOfTwo>
    let q10 = createQuadtree 4 0 4 4 1 1<powerOfTwo>
    let q01 = createQuadtree 0 8 8 8 0 1<powerOfTwo>
    let q11 = createQuadtree 16 16 16 16 -1 1<powerOfTwo>

    let bb00 = q00.SampleWindowBoundingBox
    let bb10 = q10.SampleWindowBoundingBox
    let bb01 = q01.SampleWindowBoundingBox
    let bb11 = q11.SampleWindowBoundingBox

    Assert.True(Quadtree.CountLeafs q00 = 1)
    Assert.True(Quadtree.CountLeafs q10 = 4)
    Assert.True(Quadtree.CountLeafs q01 = 16)
    Assert.True(Quadtree.CountLeafs q11 = 64)

    let m1 = Quadtree.Merge SecondDominates q00 q10
    let m1LeafCount = Quadtree.CountLeafs m1
    m1LeafCount = 5 |> Assert.True

    let m2 = Quadtree.Merge SecondDominates m1 q01
    let m2LeafCount = Quadtree.CountLeafs m2
    m2LeafCount = 21 |> Assert.True

    let m = Quadtree.Merge SecondDominates m2 q11
    let mLeafCount = Quadtree.CountLeafs m
    mLeafCount = 85 |> Assert.True
    Assert.True(m.Cell = Cell2d(0L,0L,4))

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth`` () =

    let a = createQuadtree 0 0 1 1 0 0<powerOfTwo>
    let b = createQuadtree 0 0 1 1 0 0<powerOfTwo>

    Assert.True(Quadtree.CountLeafs a = 1)
    Assert.True(Quadtree.CountLeafs b = 1)

    let m = Quadtree.Merge SecondDominates a b
    Assert.True(Quadtree.CountLeafs m = 1)

    ()


[<Fact>]
let ``Merge_Overlapping_1x1_DifferentDepth_FirstMoreDetailed`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 1 1  0 0<powerOfTwo> 20.0f

    Assert.True(Quadtree.CountLeafs a = 4)
    Assert.True(Quadtree.CountLeafs b = 1)

    let m = Quadtree.Merge FirstDominates a b
    let leafcount = Quadtree.CountLeafs m
    Assert.True((leafcount = 4))

    let l = m.GetLayer<float32> Defs.Heights1f
    let x = l.GetSample(Fail, Cell2d(0,0,0))
    Assert.True((x = 10.0f))

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_DifferentDepth_SecondMoreDetailed`` () =

    let a = createQuadtreeWithValue 0 0 1 1  0 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 20.0f

    Assert.True(Quadtree.CountLeafs a = 1)
    Assert.True(Quadtree.CountLeafs b = 4)

    let m = Quadtree.Merge SecondDominates a b
    let mLeafCount = Quadtree.CountLeafs m
    Assert.True((mLeafCount = 4))
    
    let l = m.GetLayer<float32> Defs.Heights1f
    let x = l.GetSample(Fail, Cell2d(0,0,0))
    Assert.True((x = 20.0f))

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth_FirstDominates`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 20.0f

    let m = Quadtree.Merge FirstDominates a b
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 10.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth_FirstDominates_2_e0`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 1 1 -1 0<powerOfTwo> 20.0f

    let m = Quadtree.Merge FirstDominates a b
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 10.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth_FirstDominates_2_e1`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 1<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 1 1 -1 1<powerOfTwo> 20.0f

    let m = Quadtree.Merge FirstDominates a b
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 10.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth_FirstDominates_3_e0`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 1 1 -1 0<powerOfTwo> 20.0f

    let m = Quadtree.Merge FirstDominates b a
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 20.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth_FirstDominates_3_e1`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 1<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 1 1 -1 1<powerOfTwo> 20.0f

    let m = Quadtree.Merge FirstDominates b a
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 20.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth_SecondDominates`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 20.0f

    let m = Quadtree.Merge SecondDominates a b
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 20.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth_SecondDominates_2_e0`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 1 1 -1 0<powerOfTwo> 20.0f

    let m = Quadtree.Merge SecondDominates a b
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 20.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth_SecondDominates_2_e1`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 1<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 1 1 -1 1<powerOfTwo> 20.0f

    let m = Quadtree.Merge SecondDominates a b
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 20.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth_SecondDominates_3_e0`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 1 1 -1 0<powerOfTwo> 20.0f

    let m = Quadtree.Merge SecondDominates b a
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 10.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth_SecondDominates_3_e1`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 1<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 1 1 -1 1<powerOfTwo> 20.0f

    let m = Quadtree.Merge SecondDominates b a
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 10.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDepth_NoneDominates`` () =

    let a = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 20.0f

    let m = Quadtree.Merge MoreDetailedOrSecond a b
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 20.0f)

    ()

    
[<Fact>]
let ``Merge_Overlapping_BothCentered_DifferentDepth_SecondMoreDetailed`` () =
    
    let a = createQuadtreeWithValue -1 -1 2 2  0 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue -2 -2 4 4 -1 0<powerOfTwo> 20.0f
    
    let m = Quadtree.Merge MoreDetailedOrSecond a b
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 20.0f)
    
    ()
    
[<Fact>]
let ``Merge_Overlapping_BothCentered_DifferentDepth_FirstMoreDetailed`` () =
    
    let a = createQuadtreeWithValue -1 -1 2 2  0 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue -2 -2 4 4 -1 0<powerOfTwo> 20.0f
    
    let m = Quadtree.Merge MoreDetailedOrSecond b a
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 20.0f)
    
    ()

[<Fact>]
let ``Merge_Overlapping_BothCentered_SameDetail_FirstDominates`` () =

    let a = createQuadtreeWithValue -1 -1 2 2  0 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue -1 -1 2 2  0 0<powerOfTwo> 20.0f

    let m = Quadtree.Merge FirstDominates a b
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 10.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_BothCentered_SameDetail_SecondDominates`` () =

    let a = createQuadtreeWithValue -1 -1 2 2  0 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue -1 -1 2 2  0 0<powerOfTwo> 20.0f

    let m = Quadtree.Merge SecondDominates a b
    let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.25, 0.25)) Defs.Heights1f m
    Assert.True(x.Value = 20.0f)

    ()

[<Fact>]
let ``Merge_Overlapping_BothCentered_SameDetail_NoneDominates`` () =

    let a = createQuadtreeWithValue -1 -1 2 2  0 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue -1 -1 2 2  0 0<powerOfTwo> 20.0f

    let m = Quadtree.Merge MoreDetailedOrSecond a b
    let l = m.GetLayer<float32> Defs.Heights1f
    let x = l.GetSample(Fail, Cell2d(1))
    Assert.True((x = 20.0f))

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDetail_LastWins_1`` () =

    let a = createQuadtreeWithValue 0 0 1 1 0 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 1 1 0 0<powerOfTwo> 20.0f

    Assert.True(Quadtree.CountLeafs a = 1)
    Assert.True(Quadtree.CountLeafs b = 1)

    let m = Quadtree.Merge SecondDominates a b
    Assert.True(Quadtree.CountLeafs m = 1)

    let l = m.GetLayer<float32> Defs.Heights1f
    let x = l.GetSample(Fail, Cell2d(0,0,0))
    Assert.True((x = 20.0f))

    ()

[<Fact>]
let ``Merge_Overlapping_1x1_SameDetail_LastWins_2`` () =

    let a = createQuadtreeWithValue 0 0 1 1 0 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 0 0 1 1 0 0<powerOfTwo> 20.0f

    Assert.True(Quadtree.CountLeafs a = 1)
    Assert.True(Quadtree.CountLeafs b = 1)

    let m = Quadtree.Merge SecondDominates b a
    Assert.True(Quadtree.CountLeafs m = 1)

    let l = m.GetLayer<float32> Defs.Heights1f
    let x = l.GetSample(Fail, Cell2d(0,0,0))
    Assert.True((x = 10.0f))

    ()

[<Fact>]
let ``Merge_LayersWithDifferentResolution_1`` () =

    let a = createQuadtreeWithValue 0 0 1 1  0 0<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 1 0 2 1 -1 0<powerOfTwo> 20.0f


    Assert.True(Quadtree.CountLeafs a = 1)
    Assert.True(Quadtree.CountLeafs b = 2)

    let m = Quadtree.Merge MoreDetailedOrSecond a b
    Assert.True(Quadtree.CountLeafs m = 2)
    Assert.True(m.Cell = Cell2d(0,0,1))

    let l = m.GetLayer<float32> Defs.Heights1f
    let x = l.GetSample(Fail, Cell2d(0,0,1))
    x = 16.25f   |> Assert.True

    ()

[<Fact>]
let ``Merge_LayersWithDifferentResolution_256`` () =

    let a = createQuadtreeWithValue 0 0 1 1  0 8<powerOfTwo> 10.0f
    let b = createQuadtreeWithValue 1 0 2 1 -1 8<powerOfTwo> 20.0f

    Assert.True(Quadtree.CountLeafs a = 1)
    Assert.True(Quadtree.CountLeafs b = 1)

    let m = Quadtree.Merge MoreDetailedOrSecond a b
    Assert.True(Quadtree.CountLeafs m = 1)
    Assert.True(m.Cell = Cell2d(0,0,8))

    let l = m.GetLayer<float32> Defs.Heights1f
    let x = l.GetSample(Fail, Cell2d(0,0,0))
    Assert.True((x = 12.5f))

    ()



let ``Merge_Random_SplitLimit1`` dominance =

    let mutable quadtree = createQuadtreeWithRandomValues 0 0 1 1 0 0<powerOfTwo>

    let r = Random()
    for i = 1 to 25 do
        let e = r.Next(20) - 10
        let ox = if e >= 0 then (r.Next(2000)) >>> e else (r.Next(2000)) <<< -e
        let oy = if e >= 0 then (r.Next(2000)) >>> e else (r.Next(2000)) <<< -e
        let w  = r.Next(10) + 1
        let h  = r.Next(10) + 1

        let other = createQuadtreeWithRandomValues ox oy w h e 0<powerOfTwo>
        let merged = Quadtree.Merge dominance quadtree other
        quadtree <- merged

    ()

[<Fact>]
let ``Merge_Random_SplitLimit1_FirstDominates`` () =
    Merge_Random_SplitLimit1 FirstDominates

[<Fact>]
let ``Merge_Random_SplitLimit1_SecondDominates`` () =
    Merge_Random_SplitLimit1 SecondDominates

[<Fact>]
let ``Merge_Random_SplitLimit1_MoreDetailedOrSecond`` () =
    Merge_Random_SplitLimit1 MoreDetailedOrSecond

[<Fact>]
let ``Merge_Random_SplitLimit1_MoreDetailedOrFirst`` () =
    Merge_Random_SplitLimit1 MoreDetailedOrFirst


let ``Merge_Random_SplitLimit256`` dominance =

    let mutable quadtree = createQuadtreeWithRandomValues 0 0 1 1 0 8<powerOfTwo>

    let r = Random()
    for i = 1 to 25 do
        let e = r.Next(5) - 2
        let ox = if e >= 0 then (r.Next(100)) >>> e else (r.Next(100)) <<< -e
        let oy = if e >= 0 then (r.Next(100)) >>> e else (r.Next(100)) <<< -e
        let w  = r.Next(50) + 1
        let h  = r.Next(50) + 1

        let other = createQuadtreeWithRandomValues ox oy w h e 8<powerOfTwo>
        let merged = Quadtree.Merge dominance quadtree other
        quadtree <- merged

    //let f = { HAlign=Center; VAlign=Middle; Bgcolor=C3b.White}
    //let pp = Cells.Group(
    //                Position = {X=0;Y=0}, 
    //                Format= f,
    //                Label = "Merge_Random_SplitLimit256",
    //                Content = [
    //                    Cells.ofQNodeRef<float32> "mainTree" {X=0;Y=0} f Defs.Heights1f quadtree
    //                ])
    //File.WriteAllLines(@"T:\index.html", Cells.toHtml pp)

    ()

[<Fact>]
let ``Merge_Random_SplitLimit256_FirstDominates`` () =
    Merge_Random_SplitLimit256 FirstDominates

[<Fact>]
let ``Merge_Random_SplitLimit256_SecondDominates`` () =
    Merge_Random_SplitLimit256 SecondDominates

[<Fact>]
let ``Merge_Random_SplitLimit256_MoreDetailedOrFirst`` () =
    Merge_Random_SplitLimit256 MoreDetailedOrFirst

[<Fact>]
let ``Merge_Random_SplitLimit256_MoreDetailedOrSecond`` () =
    Merge_Random_SplitLimit256 MoreDetailedOrSecond

let ``Merge_Random_Centered_SplitLimit1`` dominance =

    for _ = 1 to 10 do
        let mutable quadtree = createQuadtreeWithRandomValues 0 0 1 1 0 0<powerOfTwo>

        let r = Random()
        for i = 1 to 10 do
            let e = r.Next(20) - 10
            let ox = if e >= 0 then (r.Next(2000) - 1000) >>> e else (r.Next(2000) - 1000) <<< -e
            let oy = if e >= 0 then (r.Next(2000) - 1000) >>> e else (r.Next(2000) - 1000) <<< -e
            let w  = r.Next(10) + 1
            let h  = r.Next(10) + 1

            let other = createQuadtreeWithRandomValues ox oy w h e 0<powerOfTwo>
            let merged = Quadtree.Merge dominance quadtree other
            quadtree <- merged

    ()

[<Fact>]
let ``Merge_Random_Centered_SplitLimit1_FirstDominates`` () =
    Merge_Random_Centered_SplitLimit1 FirstDominates

[<Fact>]
let ``Merge_Random_Centered_SplitLimit1_SecondDominates`` () =
    Merge_Random_Centered_SplitLimit1 SecondDominates

[<Fact>]
let ``Merge_Random_Centered_SplitLimit1_MoreDetailedOrFirst`` () =
    Merge_Random_Centered_SplitLimit1 MoreDetailedOrFirst

[<Fact>]
let ``Merge_Random_Centered_SplitLimit1_MoreDetailedOrSecond`` () =
    Merge_Random_Centered_SplitLimit1 MoreDetailedOrSecond

let ``Merge_Random_Centered_SplitLimit64`` dominance =

    let mutable quadtree = createQuadtreeWithRandomValues 0 0 1 1 0 6<powerOfTwo>

    let r = Random()
    for i = 1 to 200 do
        let e = r.Next(20) - 10
        let ox = if e >= 0 then (r.Next(2000) - 1000) >>> e else (r.Next(2000) - 1000) <<< -e
        let oy = if e >= 0 then (r.Next(2000) - 1000) >>> e else (r.Next(2000) - 1000) <<< -e
        let w  = r.Next(100) + 1
        let h  = r.Next(100) + 1

        let other = createQuadtreeWithRandomValues ox oy w h e 6<powerOfTwo>
        let merged = Quadtree.Merge dominance quadtree other
        quadtree <- merged

    ()

[<Fact>]
let ``Merge_Random_Centered_SplitLimit64_FirstDominates`` () =
    Merge_Random_Centered_SplitLimit64 FirstDominates

[<Fact>]
let ``Merge_Random_Centered_SplitLimit64_SecondDominates`` () =
    Merge_Random_Centered_SplitLimit64 SecondDominates

[<Fact>]
let ``Merge_Random_Centered_SplitLimit64_MoreDetailedOrFirst`` () =
    Merge_Random_Centered_SplitLimit64 MoreDetailedOrFirst

[<Fact>]
let ``Merge_Random_Centered_SplitLimit64_MoreDetailedOrSecond`` () =
    Merge_Random_Centered_SplitLimit64 MoreDetailedOrSecond