module QuadtreeTests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base
open Aardvark.Data

#nowarn "44"

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
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Centered_a`` () =

    let q = createQuadtree -2 -2 4 4 0 2
    let countTotal = Quadtree.CountNodes q
    let countInner = Quadtree.CountInner q
    let countLeafs = Quadtree.CountLeafs q
    Assert.True((countTotal = 1))
    Assert.True((countInner = 0))
    Assert.True((countLeafs = 1))

[<Fact>]
let ``Build_Centered_b`` () =

    let q = createQuadtree -2 -2 4 4 0 1
    let countTotal = Quadtree.CountNodes q
    let countInner = Quadtree.CountInner q
    let countLeafs = Quadtree.CountLeafs q
    Assert.True((countTotal = 5))
    Assert.True((countInner = 1))
    Assert.True((countLeafs = 4))

[<Fact>]
let ``Build_Centered_c`` () =

    let q = createQuadtree -1 -1 2 2 0 0
    let countTotal = Quadtree.CountNodes q
    let countInner = Quadtree.CountInner q
    let countLeafs = Quadtree.CountLeafs q
    Assert.True((countTotal = 5))
    Assert.True((countInner = 1))
    Assert.True((countLeafs = 4))





[<Fact>]
let ``Build_Heights1f`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Heights1f (fun x y -> 1.0f)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Heights1d`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Heights1d (fun x y -> 1.0)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_HeightsBilinear4f`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.HeightsBilinear4f (fun x y -> V4f.Zero)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_HeightsBilinear4d`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.HeightsBilinear4d (fun x y -> V4d.Zero)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Normals3f`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Normals3f (fun x y -> V3f.ZAxis)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Normals3d`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Normals3d (fun x y -> V3d.ZAxis)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_HeightStdDevs1f`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.HeightStdDevs1f (fun x y -> 0.5f)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_HeightStdDevs1d`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.HeightStdDevs1d (fun x y -> 0.5)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Colors3b`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Colors3b (fun x y -> C3b.White)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Colors4b`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Colors4b (fun x y -> C4b.White)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Colors3f`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Colors3f (fun x y -> C3f.White)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Colors4f`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Colors4f (fun x y -> C4f.White)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Intensities1i`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Intensities1i (fun x y -> 123)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Intensities1l`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Intensities1l (fun x y -> 123L)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Intensities1f`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Intensities1f (fun x y -> 3.14f)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Intensities1d`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Intensities1d (fun x y -> 3.14)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Volumes1f`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Volumes1f (fun x y -> 1.0f)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_Volumes1d`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.Volumes1d (fun x y -> 1.0)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_VolumesBilinear4f`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.VolumesBilinear4f (fun x y -> V4f.Zero)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)

[<Fact>]
let ``Build_VolumesBilinear4d`` () =

    let q = createQuadtree' 0 0 10 7 0 2 Defs.VolumesBilinear4d (fun x y -> V4d.Zero)
    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)



(* 
    Quadtree.[ContainsLayer|UpdateLayerSemantic]
*)

let private genQuadtree () =
    let a1 = Layer(Defs.Colors4f,         [| V4f(0,1,0,1) |], DataMapping(V2l.OO, V2i.II, exponent = 0))
    let a2 = Layer(Defs.BilinearParams4f, [| V4f(1,2,3,4) |], DataMapping(V2l.OO, V2i.II, exponent = 0))
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
    
    (q |> Quadtree.TryGetLayer<V4f>(Defs.BilinearParams4f )).IsSome  |> Assert.True
    (q |> Quadtree.TryGetLayer<V4f>(Defs.HeightsBilinear4f)).IsSome  |> Assert.False

    (q |> Quadtree.TryGetLayerUntyped(Defs.BilinearParams4f )).IsSome  |> Assert.True
    (q |> Quadtree.TryGetLayerUntyped(Defs.HeightsBilinear4f)).IsSome  |> Assert.False

[<Fact>]
let ``Quadtree.UpdateLayerSemantic, existing -> non-existing, works`` () =
    
    let q = genQuadtree ()
    
    (q.TryGetInMemory().Value.Layers.Length = 2)                                    |> Assert.True
    (q |> Quadtree.ContainsLayer(Defs.Colors4f))                                    |> Assert.True
    (q |> Quadtree.ContainsLayer(Defs.BilinearParams4f))                            |> Assert.True
    (q |> Quadtree.GetLayer<V4f>(Defs.BilinearParams4f)).Data.[0] = V4f(1,2,3,4)    |> Assert.True
    (q |> Quadtree.ContainsLayer(Defs.HeightsBilinear4f))                           |> Assert.False

    // existing -> non-existing
    let (updated, r) = q.UpdateLayerSemantic(Defs.BilinearParams4f, Defs.HeightsBilinear4f)
    
    updated                                                                         |> Assert.True
    r.Id = q.Id                                                                     |> Assert.False
    (r.TryGetInMemory().Value.Layers.Length = 2)                                    |> Assert.True
    (r |> Quadtree.ContainsLayer(Defs.Colors4f))                                    |> Assert.True
    (r |> Quadtree.ContainsLayer(Defs.HeightsBilinear4f))                           |> Assert.True
    (r |> Quadtree.GetLayer<V4f>(Defs.HeightsBilinear4f)).Data.[0] = V4f(1,2,3,4)   |> Assert.True
    (r |> Quadtree.ContainsLayer(Defs.BilinearParams4f))                            |> Assert.False

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
    r.TryGetInMemory().Value.Layers.Length = 2                      |> Assert.True
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
    r.TryGetInMemory().Value.Layers.Length = 2                      |> Assert.True
    r.ContainsLayer(Defs.Colors4f)                                  |> Assert.True
    r.ContainsLayer(Defs.BilinearParams4f)                          |> Assert.True
    r.GetLayer<V4f>(Defs.BilinearParams4f).Data.[0] = V4f(1,2,3,4)  |> Assert.True
    r.ContainsLayer(Defs.HeightsBilinear4f)   
