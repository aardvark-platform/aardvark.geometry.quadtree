namespace Aardvark.Geometry.Quadtree.Tests

open Aardvark.Base
open Aardvark.Data
open Aardvark.Geometry.Quadtree
open Aardvark.Geometry.Quadtree.Serialization
open Xunit

#nowarn "44"
#nowarn "3560"

module QuadtreeTests =

    let private createQuadtree (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int) =
        let size = V2i(w, h)
        let xs = Array.zeroCreate<float32> (w * h)
        for y = 0 to size.Y - 1 do
            for x = 0 to size.X - 1 do
                let i = y * size.X + x
                xs.[i] <- float32 x + float32 y / 100.0f

        let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))

        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = splitLimit }
        Quadtree.Build config [| a |]

    let private createQuadtree' (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int) (layerDef : Durable.Def) (createSample : int -> int -> 'a) =
        let size = V2i(w, h)
        let xs = Array.zeroCreate<'a> (w * h)
        for y = 0 to size.Y - 1 do
            for x = 0 to size.X - 1 do
                let i = y * size.X + x
                xs.[i] <- createSample x y

        let a = Layer(layerDef, xs, DataMapping(V2l(ox, oy), size, exponent = e))

        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = splitLimit }
        Quadtree.Build config [| a |]

    [<Fact>]
    let ``Build1`` () =
        let q = createQuadtree 0 0 10 7 0 2
        let countLeafs = Quadtree.CountLeafs true q
        let countNodes = Quadtree.CountNodes true q
        let countInner = Quadtree.CountInner true q

        countLeafs = 6                          |> Assert.True
        countNodes = countInner + countLeafs    |> Assert.True

    [<Fact>]
    let ``Build_Centered_a`` () =

        let q = createQuadtree -2 -2 4 4 0 2
        let countTotal = Quadtree.CountNodes true q
        let countInner = Quadtree.CountInner true q
        let countLeafs = Quadtree.CountLeafs true q
        Assert.True((countTotal = 1))
        Assert.True((countInner = 0))
        Assert.True((countLeafs = 1))

    [<Fact>]
    let ``Build_Centered_b`` () =

        let q = createQuadtree -2 -2 4 4 0 1
        let countTotal = Quadtree.CountNodes true q
        let countInner = Quadtree.CountInner true q
        let countLeafs = Quadtree.CountLeafs true q
        Assert.True((countTotal = 5))
        Assert.True((countInner = 1))
        Assert.True((countLeafs = 4))

    [<Fact>]
    let ``Build_Centered_c`` () =

        let q = createQuadtree -1 -1 2 2 0 0
        let countTotal = Quadtree.CountNodes true q
        let countInner = Quadtree.CountInner true q
        let countLeafs = Quadtree.CountLeafs true q
        Assert.True((countTotal = 5))
        Assert.True((countInner = 1))
        Assert.True((countLeafs = 4))

    [<Fact>]
    let ``Build_Centered_RootCell`` () =
        Assert.True((createQuadtree -2 -2 4 4 0 2).Cell = Cell2d(2))
        Assert.True((createQuadtree -2 -2 4 4 0 1).Cell = Cell2d(2))
        Assert.True((createQuadtree -1 -1 2 2 0 0).Cell = Cell2d(1))

    [<Fact>]
    let ``Build_MaxOnCellBoundary_RootCell`` () =
        let q = createQuadtree 0 0 4 4 0 2
        Assert.True(q.Cell = Cell2d(0L, 0L, 2))
        Assert.True(Quadtree.CountNodes true q = 1)

    /// Smallest cell with exponent >= minExponent containing all samples of window [min, max) at sample exponent e.
    /// Brute force with integer arithmetic only, independent of Cell2d(Box2d) and Cell2d.GetCommonRoot.
    let private expectedRootCell (min : V2l) (max : V2l) (e : int) (minExponent : int) =
        let rec find k =
            let s = k - e
            let x = min.X >>> s
            let y = min.Y >>> s
            if x = ((max.X - 1L) >>> s) && y = ((max.Y - 1L) >>> s) then
                Cell2d(x, y, k)
            elif s >= 1 && (let h = 1L <<< (s - 1) in -h <= min.X && -h <= min.Y && max.X <= h && max.Y <= h) then
                Cell2d(k)
            else
                find (k + 1)
        find minExponent

    [<Fact>]
    let ``Build_RootCell_IsSmallestCellContainingAllSamples`` () =
        let r = System.Random(5)
        for _ in 1 .. 2000 do
            let ox = r.Next(-40, 41)
            let oy = r.Next(-40, 41)
            let w = r.Next(1, 41)
            let h = r.Next(1, 41)
            let e = r.Next(-2, 3)
            let split = r.Next(0, 4)
            let min = V2l(ox, oy)
            let max = V2l(ox + w, oy + h)
            let layer = Layer(Defs.Heights1f, Array.zeroCreate<float32> (w * h), DataMapping(min, V2i(w, h), exponent = e))
            let q = Quadtree.Build { BuildConfig.Default with SplitLimitPowerOfTwo = split } [| layer |]
            let case = sprintf "window %A..%A, e = %d, split = %d" min max e split
            Assert.True(layer.CellBounds = expectedRootCell min max e e, sprintf "CellBounds %A for %s" layer.CellBounds case)
            Assert.True(q.Cell = expectedRootCell min max e (e + split), sprintf "root %A for %s" q.Cell case)

    [<Fact>]
    let ``QNode_FromLayerSet_MaxOnCellBoundary`` () =
        let layer = Layer(Defs.Heights1f, Array.zeroCreate<float32> 16, DataMapping(V2l(-2, -2), V2i(4, 4), exponent = 0))
        let n = QNode(2, LayerSet([| layer |]))
        Assert.True(n.Cell = Cell2d(2))

    [<Fact>]
    let ``Build_Heights1f`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Heights1f (fun x y -> 1.0f)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Heights1d`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Heights1d (fun x y -> 1.0)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_HeightsBilinear4f`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.HeightsBilinear4f (fun x y -> V4f.Zero)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_HeightsBilinear4d`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.HeightsBilinear4d (fun x y -> V4d.Zero)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Normals3f`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Normals3f (fun x y -> V3f.ZAxis)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Normals3d`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Normals3d (fun x y -> V3d.ZAxis)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_HeightStdDevs1f`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.HeightStdDevs1f (fun x y -> 0.5f)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_HeightStdDevs1d`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.HeightStdDevs1d (fun x y -> 0.5)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Colors3b`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Colors3b (fun x y -> C3b.White)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Colors4b`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Colors4b (fun x y -> C4b.White)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Colors3f`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Colors3f (fun x y -> C3f.White)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Colors4f`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Colors4f (fun x y -> C4f.White)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Intensities1i`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Intensities1i (fun x y -> 123)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Intensities1l`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Intensities1l (fun x y -> 123L)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Intensities1f`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Intensities1f (fun x y -> 3.14f)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Intensities1d`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Intensities1d (fun x y -> 3.14)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Volumes1f`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Volumes1f (fun x y -> 1.0f)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_Volumes1d`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.Volumes1d (fun x y -> 1.0)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_VolumesBilinear4f`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.VolumesBilinear4f (fun x y -> V4f.Zero)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_VolumesBilinear4d`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.VolumesBilinear4d (fun x y -> V4d.Zero)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    (* 
        Quadtree.[ContainsLayer|UpdateLayerSemantic]
    *)

    let private genQuadtree () =
        let a1 = Layer(Defs.Colors4f,         [| C4f.Chocolate |], DataMapping(V2l.OO, V2i.II, exponent = 0))
        let a2 = Layer(Defs.BilinearParams4f, [| V4f(1,2,3,4)  |], DataMapping(V2l.OO, V2i.II, exponent = 0))
        Quadtree.Build BuildConfig.Default [| a1; a2 |]

    [<Fact>]
    let ``Quadtree.ContainsLayer`` () =
    
        let q = genQuadtree ()
    
        (q |> Quadtree.ContainsLayer(Defs.Colors4f))                                    |> Assert.True
        (q |> Quadtree.ContainsLayer(Defs.BilinearParams4f))                            |> Assert.True
        (q |> Quadtree.GetLayer<V4f>(Defs.BilinearParams4f)).Data.[0] = V4f(1,2,3,4)    |> Assert.True
        (q |> Quadtree.ContainsLayer(Defs.HeightsBilinear4f))                           |> Assert.False

        (q |> Quadtree.TryGetLayer<V4f>(Defs.BilinearParams4f)).IsSome                  |> Assert.True
        (q |> Quadtree.TryGetLayer<V4f>(Defs.HeightsBilinear4f)).IsSome                 |> Assert.False

        (q |> Quadtree.TryGetLayer<V4f>(Defs.BilinearParams4f)).IsSome                  |> Assert.True
        (q |> Quadtree.TryGetLayer<V4f>(Defs.HeightsBilinear4f)).IsSome                 |> Assert.False

    [<Fact>]
    let ``Quadtree.GetLayer`` () =
    
        let q = genQuadtree ()
    
        q |> Quadtree.GetLayer<V4f>(Defs.BilinearParams4f) |> ignore
        Assert.ThrowsAny<exn>(fun () -> q |> Quadtree.GetLayer<V4f>(Defs.HeightsBilinear4f) |> ignore)  |> ignore

        q |> Quadtree.GetLayerUntyped(Defs.BilinearParams4f) |> ignore
        Assert.ThrowsAny<exn>(fun () -> q |> Quadtree.GetLayer(Defs.HeightsBilinear4f) |> ignore) |> ignore

    [<Fact>]
    let ``Quadtree.TryGetLayer`` () =
    
        let q = genQuadtree ()
    
        q.TryGetLayer<V4f>(Defs.BilinearParams4f ).IsSome   |> Assert.True
        q.TryGetLayer<V4f>(Defs.HeightsBilinear4f).IsSome   |> Assert.False

        q.TryGetLayer(Defs.BilinearParams4f ).IsSome        |> Assert.True
        q.TryGetLayer(Defs.HeightsBilinear4f).IsSome        |> Assert.False

    [<Fact>]
    let ``Quadtree.UpdateLayerSemantic, existing -> non-existing, works`` () =
    
        let q = genQuadtree ()
    
        q.LayerSet.Value.Layers.Length = 2                              |> Assert.True
        q.ContainsLayer(Defs.Colors4f)                                  |> Assert.True
        q.ContainsLayer(Defs.BilinearParams4f)                          |> Assert.True
        q.GetLayer<V4f>(Defs.BilinearParams4f).Data.[0] = V4f(1,2,3,4)  |> Assert.True
        q.ContainsLayer(Defs.HeightsBilinear4f)                         |> Assert.False

        // existing -> non-existing
        let (updated, r) = q.UpdateLayerSemantic(Defs.BilinearParams4f, Defs.HeightsBilinear4f)
    
        updated                                                         |> Assert.True
        r.Id = q.Id                                                     |> Assert.False
        q.LayerSet.Value.Layers.Length = 2                              |> Assert.True
        r.ContainsLayer(Defs.Colors4f)                                  |> Assert.True
        r.ContainsLayer(Defs.HeightsBilinear4f)                         |> Assert.True
        r.GetLayer<V4f>(Defs.HeightsBilinear4f).Data.[0] = V4f(1,2,3,4) |> Assert.True
        r.ContainsLayer(Defs.BilinearParams4f)                          |> Assert.False

    [<Fact>]
    let ``Quadtree.UpdateLayerSemantic, existing -> existing, fails`` () =
    
        let q = genQuadtree ()

        // existing -> existing
        Assert.ThrowsAny<exn>(fun () ->
            q |> Quadtree.UpdateLayerSemantic(Defs.Colors4f, Defs.BilinearParams4f) |> ignore
            ) |> ignore

    [<Fact>]
    let ``Quadtree.UpdateLayerSemantic, non-existing -> non-existing, nop`` () =
    
        let q = genQuadtree ()

        // non-existing -> non-existing
        let (updated, r) = q.UpdateLayerSemantic(Defs.VolumesBilinear4f, Defs.HeightsBilinear4f)
    
        updated                                                         |> Assert.False
        r.Id = q.Id                                                     |> Assert.True
        r.LayerSet.Value.Layers.Length = 2                              |> Assert.True
        r.ContainsLayer(Defs.Colors4f)                                  |> Assert.True
        r.ContainsLayer(Defs.BilinearParams4f)                          |> Assert.True
        r.GetLayer<V4f>(Defs.BilinearParams4f).Data.[0] = V4f(1,2,3,4)  |> Assert.True
        r.ContainsLayer(Defs.HeightsBilinear4f)                         |> Assert.False

    [<Fact>]
    let ``Quadtree.UpdateLayerSemantic, non-existing -> existing, nop`` () =
    
        let q = genQuadtree ()

        // non-existing -> existing
        let (updated, r) = q.UpdateLayerSemantic(Defs.VolumesBilinear4f, Defs.BilinearParams4f)

        updated                                                         |> Assert.False
        r.Id = q.Id                                                     |> Assert.True
        r.LayerSet.Value.Layers.Length = 2                              |> Assert.True
        r.ContainsLayer(Defs.Colors4f)                                  |> Assert.True
        r.ContainsLayer(Defs.BilinearParams4f)                          |> Assert.True
        r.GetLayer<V4f>(Defs.BilinearParams4f).Data.[0] = V4f(1,2,3,4)  |> Assert.True
        r.ContainsLayer(Defs.HeightsBilinear4f)   

    [<Fact>]
    let ``Workflow. upgrade old semantic on load from store`` () =
    
        let upgrade config (x : QNodeRef) =
            match x.UpdateLayerSemantic(Defs.BilinearParams4f, Defs.HeightsBilinear4f) with
            | false, unchangedTree ->
                printfn "  nothing changed"
                unchangedTree.Id = x.Id                     |> Assert.True
                unchangedTree
            | true , changedTree   ->
                let newId = changedTree |> Quadtree.Save config
                changedTree.Id = newId                      |> Assert.True
                changedTree.Id = x.Id                       |> Assert.False
                printfn "  saved changed tree %A" newId
                changedTree

        let config = SerializationOptions.NewInMemoryStore false

        printfn "[save old-style tree]"
        let q = genQuadtree ()
        q.ContainsLayer(Defs.BilinearParams4f)              |> Assert.True
        q.ContainsLayer(Defs.HeightsBilinear4f)             |> Assert.False
        let id = q |> Quadtree.Save config
        printfn "  id = %A" id

        printfn "[load old-style tree]"
        let oldTree = Quadtree.Load config id
        oldTree.Id = q.Id                                   |> Assert.True
        oldTree.ContainsLayer(Defs.BilinearParams4f)        |> Assert.True
        oldTree.ContainsLayer(Defs.HeightsBilinear4f)       |> Assert.False
        printfn "  id = %A" oldTree.Id

        printfn "[upgrade and save changed tree]"
        let newTree = oldTree |> upgrade config
        newTree.Id <> oldTree.Id                            |> Assert.True
        newTree.ContainsLayer(Defs.BilinearParams4f)        |> Assert.False
        newTree.ContainsLayer(Defs.HeightsBilinear4f)       |> Assert.True

        printfn "[load upgraded tree]"
        let upgradedTree = Quadtree.Load config newTree.Id
        printfn "  id = %A" upgradedTree.Id
        upgradedTree.Id = newTree.Id                        |> Assert.True
        upgradedTree.ContainsLayer(Defs.BilinearParams4f)   |> Assert.False
        upgradedTree.ContainsLayer(Defs.HeightsBilinear4f)  |> Assert.True

        printfn "[upgrade already upgraded tree]"
        let newTree2 = upgradedTree |> upgrade config
        newTree.Id = upgradedTree.Id                        |> Assert.True
        newTree2.ContainsLayer(Defs.BilinearParams4f)       |> Assert.False
        newTree2.ContainsLayer(Defs.HeightsBilinear4f)      |> Assert.True

        ()
 
    [<Fact>]
    let ``Madorjan 2021-11-03`` () =
        // https://github.com/vrvis/Vgm.Api/issues/26
        let heights = Array.create 16 V4f.Zero
        
        let mapping = DataMapping(origin = Cell2d(0L, 0L, 0), size = V2i(4, 4))
        let heightsLayer = Layer(Defs.HeightsBilinear4f, heights, mapping) :> ILayer
        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = 2 }
        let qTree = Quadtree.Build config [| heightsLayer |]

        Assert.True(qTree.Cell.BoundingBox = Box2d(0.0, 0.0, 4.0, 4.0))
        Assert.True(qTree.LayerSet.IsSome)

        ()