namespace Aardvark.Geometry.Quadtree.Tests

open Aardvark.Base
open Aardvark.Geometry.Quadtree
open Aardvark.Geometry.Quadtree.PrettyPrint
open System
open Xunit

module MergeTests =

    [<Fact>]
    let ``Merge_NonOverlapping_Adjacent_SameDepth`` () =

        let q00 = createQuadtree 0 0 8 8 0 2<powerOfTwo>
        let q10 = createQuadtree 8 0 8 8 0 2<powerOfTwo>
        let q01 = createQuadtree 0 8 8 8 0 2<powerOfTwo>
        let q11 = createQuadtree 8 8 8 8 0 2<powerOfTwo>

        // before log2int/Cell2d-fix
        //Assert.True(q00.Cell = Cell2d(0L, 0L, 3))
        //Assert.True(q10.Cell = Cell2d(1L, 0L, 3))
        //Assert.True(q01.Cell = Cell2d(0L, 1L, 3))
        //Assert.True(q11.Cell = Cell2d(1L, 1L, 3))
        // after log2int/Cell2d-fix
        Assert.True(q00.Cell = Cell2d(0L, 0L, 4))
        Assert.True(q10.Cell = Cell2d(0L, 0L, 5))
        Assert.True(q01.Cell = Cell2d(0L, 0L, 5))
        Assert.True(q11.Cell = Cell2d(0L, 0L, 5))

        let m1 = Quadtree.Merge SecondDominates q00 q10
        Assert.True(Quadtree.CountLeafs true m1 = 8)
        Assert.True(Quadtree.CountNodes true m1 = Quadtree.CountInner true m1 + Quadtree.CountLeafs true m1)

        let m2 = Quadtree.Merge SecondDominates m1 q01
        Assert.True(Quadtree.CountLeafs true m2 = 12)
        Assert.True(Quadtree.CountNodes true m2 = Quadtree.CountInner true m2 + Quadtree.CountLeafs true m2)

        let m = Quadtree.Merge SecondDominates m2 q11
        Assert.True(Quadtree.CountLeafs true m = 16)
        Assert.True(Quadtree.CountNodes true m = Quadtree.CountInner true m + Quadtree.CountLeafs true m)
        // before log2int/Cell2d-fix
        //Assert.True(m.Cell = Cell2d(0L,0L,4))
        // after log2int/Cell2d-fix
        Assert.True(m.Cell = Cell2d(0L,0L,5))

        ()

    [<Fact>]
    let ``Merge_NonOverlapping_NonAdjacent_SameDepth`` () =

        let q00 = createQuadtree 0 0 1 1 0 2<powerOfTwo>
        let q10 = createQuadtree 7 0 1 1 0 2<powerOfTwo>
        let q01 = createQuadtree 0 6 1 1 0 2<powerOfTwo>
        let q11 = createQuadtree 5 7 1 1 0 2<powerOfTwo>

        // before log2int/Cell2d-fix
        //Assert.True(q00.Cell = Cell2d(0L, 0L, 2))
        //Assert.True(q10.Cell = Cell2d(1L, 0L, 2))
        //Assert.True(q01.Cell = Cell2d(0L, 1L, 2))
        //Assert.True(q11.Cell = Cell2d(1L, 1L, 2))
        // after log2int/Cell2d-fix
        Assert.True(q00.Cell = Cell2d(0L, 0L, 2))
        Assert.True(q10.Cell = Cell2d(0L, 0L, 4))
        Assert.True(q01.Cell = Cell2d(0L, 1L, 2))
        Assert.True(q11.Cell = Cell2d(0L, 0L, 4))

        let m1 = Quadtree.Merge SecondDominates q00 q10
        Assert.True(Quadtree.CountLeafs true m1 = 2)
        Assert.True(Quadtree.CountNodes true m1 = Quadtree.CountInner true m1 + Quadtree.CountLeafs true m1)

        let m2 = Quadtree.Merge SecondDominates m1 q01
        Assert.True(Quadtree.CountLeafs true m2 = 3)
        Assert.True(Quadtree.CountNodes true m2 = Quadtree.CountInner true m2 + Quadtree.CountLeafs true m2)

        let m = Quadtree.Merge SecondDominates m2 q11
        Assert.True(Quadtree.CountLeafs true m = 4)
        Assert.True(Quadtree.CountNodes true m = Quadtree.CountInner true m + Quadtree.CountLeafs true m)
        // before log2int/Cell2d-fix
        //Assert.True(m.Cell = Cell2d(0L,0L,3))
        // after log2int/Cell2d-fix
        Assert.True(m.Cell = Cell2d(0L,0L,4))

        ()

    [<Fact>]
    let ``Merge_NonOverlapping_Adjacent_DifferentExp`` () =

        let q00 = createQuadtree 0 0 2 2 2 1<powerOfTwo>
        let q10 = createQuadtree 4 0 4 4 1 1<powerOfTwo>
        let q01 = createQuadtree 0 8 8 8 0 1<powerOfTwo>
        let q11 = createQuadtree 16 16 16 16 -1 1<powerOfTwo>

        //let bb00 = q00.SampleWindowBoundingBox
        //let bb10 = q10.SampleWindowBoundingBox
        //let bb01 = q01.SampleWindowBoundingBox
        //let bb11 = q11.SampleWindowBoundingBox

        Assert.True(Quadtree.CountLeafs true q00 = 1)
        Assert.True(Quadtree.CountLeafs true q10 = 4)
        Assert.True(Quadtree.CountLeafs true q01 = 16)
        Assert.True(Quadtree.CountLeafs true q11 = 64)

        let m1 = Quadtree.Merge SecondDominates q00 q10
        let m1LeafCount = Quadtree.CountLeafs true m1
        m1LeafCount = 5 |> Assert.True

        let m2 = Quadtree.Merge SecondDominates m1 q01
        let m2LeafCount = Quadtree.CountLeafs true m2
        m2LeafCount = 21 |> Assert.True

        let m = Quadtree.Merge SecondDominates m2 q11
        let mLeafCount = Quadtree.CountLeafs true m
        mLeafCount = 85 |> Assert.True
        // before log2int/Cell2d-fix
        //Assert.True(m.Cell = Cell2d(0L,0L,4))
        // after log2int/Cell2d-fix
        Assert.True(m.Cell = Cell2d(0L,0L,5))

        ()

    [<Fact>]
    let ``Merge_Overlapping_1x1_SameDepth`` () =

        let a = createQuadtree 0 0 1 1 0 0<powerOfTwo>
        let b = createQuadtree 0 0 1 1 0 0<powerOfTwo>

        Assert.True(Quadtree.CountLeafs true a = 1)
        Assert.True(Quadtree.CountLeafs true b = 1)

        let m = Quadtree.Merge SecondDominates a b
        Assert.True(Quadtree.CountLeafs true m = 1)

        ()


    [<Fact>]
    let ``Merge_Overlapping_1x1_DifferentDepth_FirstMoreDetailed`` () =

        let a = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 10.0f
        let b = createQuadtreeWithValue 0 0 1 1  0 0<powerOfTwo> 20.0f

        Assert.True(Quadtree.CountLeafs true a = 4)
        Assert.True(Quadtree.CountLeafs true b = 1)

        let m = Quadtree.Merge FirstDominates a b
        let leafcount = Quadtree.CountLeafs true m
        Assert.True((leafcount = 4))

        let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.5,0.5)) Defs.Heights1f m
        Assert.True((x.Value = 10.0f))

        ()

    [<Fact>]
    let ``Merge_Overlapping_1x1_DifferentDepth_SecondMoreDetailed`` () =

        let a = createQuadtreeWithValue 0 0 1 1  0 0<powerOfTwo> 10.0f
        let b = createQuadtreeWithValue 0 0 2 2 -1 0<powerOfTwo> 20.0f

        Assert.True(Quadtree.CountLeafs true a = 1)
        Assert.True(Quadtree.CountLeafs true b = 4)

        let m = Quadtree.Merge SecondDominates a b
        let mLeafCount = Quadtree.CountLeafs true m
        Assert.True((mLeafCount = 4))
    
        let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.5,0.5)) Defs.Heights1f m
        Assert.True((x.Value = 20.0f))

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
    
        let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.0,0.0)) Defs.Heights1f m
        Assert.True((x.Value = 20.0f))

        ()

    [<Fact>]
    let ``Merge_Overlapping_1x1_SameDetail_LastWins_1`` () =

        let a = createQuadtreeWithValue 0 0 1 1 0 0<powerOfTwo> 10.0f
        let b = createQuadtreeWithValue 0 0 1 1 0 0<powerOfTwo> 20.0f

        Assert.True(Quadtree.CountLeafs true a = 1)
        Assert.True(Quadtree.CountLeafs true b = 1)

        let m = Quadtree.Merge SecondDominates a b
        Assert.True(Quadtree.CountLeafs true m = 1)

        let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.5,0.5)) Defs.Heights1f m
        Assert.True((x.Value = 20.0f))

        ()

    [<Fact>]
    let ``Merge_Overlapping_1x1_SameDetail_LastWins_2`` () =

        let a = createQuadtreeWithValue 0 0 1 1 0 0<powerOfTwo> 10.0f
        let b = createQuadtreeWithValue 0 0 1 1 0 0<powerOfTwo> 20.0f

        Assert.True(Quadtree.CountLeafs true a = 1)
        Assert.True(Quadtree.CountLeafs true b = 1)

        let m = Quadtree.Merge SecondDominates b a
        Assert.True(Quadtree.CountLeafs true m = 1)

        let x = Sample.PositionTyped<float32> Query.Config.Default (V2d(0.5,0.5)) Defs.Heights1f m
        Assert.True((x.Value = 10.0f))

        ()


    [<Fact>]
    let ``ExactBoundingBox 1`` () =
        let a = createQuadtreeWithValue 0 0 1 1  0 0<powerOfTwo> 10.0f
        Assert.True(a.ExactBoundingBox = Box2d(V2d(0.0,0.0),V2d(1.0,1.0)))

    [<Fact>]
    let ``ExactBoundingBox 2`` () =
        let a = createQuadtreeWithValue 0 0 2 1  0 0<powerOfTwo> 10.0f
        Assert.True(a.ExactBoundingBox = Box2d(V2d(0.0,0.0),V2d(2.0,1.0)))

    [<Fact>]
    let ``ExactBoundingBox 3`` () =
        let a = createQuadtreeWithValue 1 0 2 1  0 0<powerOfTwo> 10.0f
        Assert.True(a.ExactBoundingBox = Box2d(V2d(1.0,0.0),V2d(3.0,1.0)))

    [<Fact>]
    let ``Merge_LayersWithDifferentResolution_1`` () =

        // Query.IntersectsCell'
        let a = createQuadtreeWithValue 0 0 1 1  0 0<powerOfTwo> 10.0f
        let b = createQuadtreeWithValue 1 0 2 1 -1 0<powerOfTwo> 20.0f
        let m = Quadtree.Merge SecondDominates a b
    
        Assert.True(Quadtree.CountLeafs true a = 1)
        Assert.True(Quadtree.CountLeafs true b = 2)
        Assert.True(Quadtree.CountLeafs true m = 3)
        Assert.True(m.Cell = Cell2d(0,0,1))

        let xs = Query.All Query.Config.Default m |> Seq.collect(fun x -> x.GetSamples<float32> Defs.Heights1f) |> Seq.toArray
        let check (x : int) (y : int) (e : int) value =
            let cell = Cell2d(x, y, e)
            xs |> Array.filter(fun (c, v) -> c = cell && v = value) |> Array.tryExactlyOne |> Option.isSome |> Assert.True

        check 1 0 -1   20.0f
        check 2 0 -1   20.0f
        check 0 0 -1   10.0f
        check 0 1 -1   10.0f
        check 1 1 -1   10.0f
    
    [<Fact>]
    let ``Merge_LayersWithDifferentResolution_256`` () =

        let a = createQuadtreeWithValue 0 0 1 1  0 8<powerOfTwo> 10.0f
        let b = createQuadtreeWithValue 1 0 2 1 -1 8<powerOfTwo> 20.0f
        let m = Quadtree.Merge MoreDetailedOrSecond a b

        Assert.True(Quadtree.CountLeafs true a = 1)
        Assert.True(Quadtree.CountLeafs true b = 1)
        Assert.True(Quadtree.CountLeafs true m = 2)
        Assert.True(m.Cell = Cell2d(0,0,8))

        let xs = Query.All Query.Config.Default m |> Seq.toArray
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

        //showHtmlDebugView<float32> "Merge_Random_Centered_SplitLimit64" Defs.Heights1f [
        //    ("quadtree", quadtree)
        //    ]

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