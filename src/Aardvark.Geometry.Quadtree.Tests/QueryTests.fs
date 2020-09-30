module QueryTests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base

let private createQuadtree () =
    let size = V2i(10, 7)
    let xs = Array.zeroCreate<float32> (size.X * size.Y)
    for y = 0 to size.Y - 1 do
        for x = 0 to size.X - 1 do
            let i = y * size.X + x
            xs.[i] <- float32 x + float32 y / 100.0f

    let a = Layer(Defs.Heights1f, xs, DataMapping(V2l.OO, size, exponent = 0))

    let config = { BuildConfig.Default with SplitLimitPowerOfTwo = 2 }
    Quadtree.Build config [| a |]


[<Fact>]
let ``cpunz_20200829`` () =
    // define mapping of raw data to raster space
    let hor1 = V4f(1.0, 0.0,0.0,0.0)
    let hor2 = V4f(2.0, 0.0,0.0,0.0)
    let oblique12 = V4f(1.5, 1.0,0.0,0.0)
    
    let parameters = [|hor1; oblique12; hor2; 
                       hor1; oblique12; hor2;
                       hor1; oblique12; hor2;
                       hor1; oblique12; hor2|]

    let mapping = DataMapping(origin = Cell2d(0L, 0L, 0), size = V2i(3, 4))

    // a layer gives meaning to raw data
    let bilinParameters = Layer(Defs.HeightsBilinear4f, parameters, mapping)
    
    // build the quadtree (incl. levels-of-detail)
    
    let qtree = Quadtree.Build BuildConfig.Default [| bilinParameters |]

    let polygon = Polygon2d([|V2d(0.0,0.0); V2d(2.0,2.0); V2d(3.0,2.0); V2d(3.0,0.0); V2d(0.0,0.0)|])
    let result0 = qtree |> Query.InsidePolygon Query.Config.Default polygon |> Seq.collect (fun chunk -> chunk.GetSamples<V4f> Defs.HeightsBilinear4f) |> Seq.toArray

    //let result1 = qtree |> Query.All Query.Config.Default |> Seq.collect (fun chunk -> chunk.GetSamples<V4f> Defs.BilinearParams4f) |> Seq.toArray
    //printfn "Query.Polygon: length = %d" result1.Length

    Assert.True(result0.Length = 5)
    Assert.True(fst result0.[0] = Cell2d(0, 0, 0))
    Assert.True(fst result0.[1] = Cell2d(1, 0, 0))
    Assert.True(fst result0.[2] = Cell2d(2, 0, 0))
    Assert.True(fst result0.[3] = Cell2d(1, 1, 0))
    Assert.True(fst result0.[4] = Cell2d(2, 1, 0))

[<Fact>]
let ``All`` () =
    let q = createQuadtree ()
    let r = Query.All Query.Config.Default q |> Array.ofSeq
    Assert.True(r.Length = 6)
    Assert.True(r |> Array.forall (fun x -> match x.Selection with | Query.FullySelected -> true | _ -> false ))


[<Fact>]
let ``InsideCell2d_FullyInside`` () =
    let q = createQuadtree ()
    let r = Query.InsideCell Query.Config.Default (Cell2d(0L, 0L, 4)) q |> Array.ofSeq
    Assert.True(r.Length = 6)
    Assert.True(r |> Array.forall (fun x -> match x.Selection with | Query.FullySelected -> true | _ -> false ))

[<Fact>]
let ``InsideCell2d_FullyOutside`` () =
    let q = createQuadtree ()
    let r = Query.InsideCell Query.Config.Default (Cell2d(1L, 0L, 4)) q |> Array.ofSeq
    Assert.True(r.Length = 0)

[<Fact>]
let ``InsideCell2d_Partial`` () =
    let q = createQuadtree ()
    let r = Query.InsideCell Query.Config.Default (Cell2d(0L, 0L, 3)) q |> Array.ofSeq
    Assert.True(r.Length = 4)

[<Fact>]
let ``InsideCell2d_ExactCellMatch`` () =
    let q = createQuadtree ()
    let r = Query.InsideCell Query.Config.Default (Cell2d(2L, 1L, 0)) q |> Array.ofSeq
    Assert.True(r.Length = 1)
    match r.[0].Selection with
    | Query.PartiallySelected xs ->
        Assert.True(xs.Length = 1)
        let xs = r.[0].GetSamples<float32> Defs.Heights1f
        Assert.True(xs.Length = 1)
        let (c, x) = xs.[0]
        Assert.True(Cell2d(2L, 1L, 0) = c)
        Assert.True(2.01f = x)
    | _ -> Assert.True(false)

[<Fact>]
let ``InsideCell2d_Supersampling`` () =
    let q = createQuadtree ()
    let r = Query.InsideCell Query.Config.Default (Cell2d(4L, 2L, -1)) q |> Array.ofSeq
    Assert.True(r.Length = 0)



[<Fact>]
let ``IntersectsCell2d_FullyInside`` () =
    let q = createQuadtree ()
    let r = Query.IntersectsCell Query.Config.Default (Cell2d(0L, 0L, 4)) q |> Array.ofSeq
    Assert.True(r.Length = 6)
    Assert.True(r |> Array.forall (fun x -> match x.Selection with | Query.FullySelected -> true | _ -> false ))

[<Fact>]
let ``IntersectsCell2d_FullyOutside`` () =
    let q = createQuadtree ()
    let r = Query.IntersectsCell Query.Config.Default (Cell2d(1L, 0L, 4)) q |> Array.ofSeq
    Assert.True(r.Length = 0)

[<Fact>]
let ``IntersectsCell2d_Partial`` () =
    let q = createQuadtree ()
    let r = Query.IntersectsCell Query.Config.Default (Cell2d(0L, 0L, 3)) q |> Array.ofSeq
    Assert.True(r.Length = 4)

[<Fact>]
let ``IntersectsCell2d_ExactCellMatch`` () =
    let q = createQuadtree ()
    let r = Query.IntersectsCell Query.Config.Default (Cell2d(2L, 1L, 0)) q |> Array.ofSeq
    Assert.True(r.Length = 1)
    match r.[0].Selection with
    | Query.PartiallySelected xs ->
        Assert.True(xs.Length = 1)
        let xs = r.[0].GetSamples<float32> Defs.Heights1f
        Assert.True(xs.Length = 1)
        let (c, x) = xs.[0]
        Assert.True(Cell2d(2L, 1L, 0) = c)
        Assert.True(2.01f = x)
    | _ -> Assert.True(false)

[<Fact>]
let ``IntersectsCell2d_Supersampling`` () =
    let q = createQuadtree ()
    let r = Query.IntersectsCell Query.Config.Default (Cell2d(4L, 2L, -1)) q |> Array.ofSeq
    Assert.True(r.Length = 1)
    match r.[0].Selection with
    | Query.PartiallySelected xs ->
        Assert.True(xs.Length = 1)
        let xs = r.[0].GetSamples<float32> Defs.Heights1f
        Assert.True(xs.Length = 1)
        let (c, x) = xs.[0]
        Assert.True(Cell2d(2L, 1L, 0) = c)
        Assert.True(2.01f = x)
    | _ -> Assert.True(false)


[<Fact>]
let ``InsideBox2d_FullyInside`` () =
    let q = createQuadtree ()
    let r = Query.InsideBox Query.Config.Default (Box2d(V2d(0,0),V2d(10,7))) q |> Array.ofSeq
    Assert.True(r.Length = 6)
    Assert.True(r |> Array.forall (fun x -> match x.Selection with | Query.FullySelected -> true | _ -> false ))

[<Fact>]
let ``InsideBox2d_FullyOutside`` () =
    let q = createQuadtree ()
    let r = Query.InsideBox Query.Config.Default (Box2d(V2d(10,1),V2d(100,100))) q |> Array.ofSeq
    Assert.True(r.Length = 0)

[<Fact>]
let ``InsideBox2d_Partial`` () =
    let q = createQuadtree ()
    let r = Query.InsideBox Query.Config.Default (Box2d(V2d(-1,-1),V2d(5,3))) q |> Array.ofSeq
    Assert.True(r.Length = 2)



[<Fact>]
let ``InsidePolygon2d_FullyInside`` () =
    let q = createQuadtree ()
    let filter = Polygon2d([|V2d(3.9,-0.1);V2d(10.1,-0.1);V2d(10.1,7.1);V2d(8.1,7.1);V2d(8.1,4.1);V2d(3.9,4.1)|])
    let r = Query.InsidePolygon Query.Config.Default filter q |> Array.ofSeq
    Assert.True(r.Length = 3)
    //Assert.True(r |> Array.forall (fun x -> match x.Selection with | Query.FullySelected -> true | _ -> false ))

[<Fact>]
let ``InsidePolygon2d_FullyOutside`` () =
    let q = createQuadtree ()
    let filter = Polygon2d([|V2d(4,7);V2d(10,8);V2d(6,15)|])
    let r = Query.InsidePolygon Query.Config.Default filter q |> Array.ofSeq
    Assert.True(r.Length = 0)

[<Fact>]
let ``InsidePolygon2d_Partial`` () =
    let q = createQuadtree ()
    let filter = Polygon2d([|V2d(0,0);V2d(5,0);V2d(4,4);V2d(4.0,4.5);V2d(0,4)|])
    let r = Query.InsidePolygon Query.Config.Default filter q |> Array.ofSeq
    Assert.True(r.Length = 2)



[<Fact>]
let ``NearLine2d_FullyInside`` () =
    let q = createQuadtree ()
    let filter = Ray2d(origin = V2d.OO, direction = V2d(10,7).Normalized)
    let r = Query.NearLine Query.Config.Default filter 10.0 q |> Array.ofSeq
    Assert.True(r.Length = 6)
    Assert.True(r |> Array.forall (fun x -> match x.Selection with | Query.FullySelected -> true | _ -> false ))

[<Fact>]
let ``NearLine2d_FullyOutside`` () =
    let q = createQuadtree ()
    let filter = Ray2d(origin = V2d(10,-2), direction = V2d(10,7).Normalized)
    let r = Query.NearLine Query.Config.Default filter 1.0 q |> Array.ofSeq
    Assert.True(r.Length = 0)

[<Fact>]
let ``NearLine2d_Partial`` () =
    let q = createQuadtree ()
    let filter = Ray2d(origin = V2d(0,0), direction = V2d(3,4).Normalized)
    let r = Query.NearLine Query.Config.Default filter 1.0 q |> Array.ofSeq
    Assert.True(r.Length = 3)

    let foo = r |> Array.collect (fun x -> x.GetSamples<float32> Defs.Heights1f)
    Assert.True(foo.Length = 16)



[<Fact>]
let ``Position`` () =
    let q = createQuadtree ()

    let trySample p =
            Query.Position Query.Config.Default p q 
            |> Seq.collect (fun r -> r.GetSamples<float32> Defs.Heights1f)
            |> Seq.tryExactlyOne

    let isOutside pos = Assert.True(match trySample pos with | Some (c, h) -> false | None -> true)
    let isInside pos cell = Assert.True(match trySample pos with | Some (c, h) -> c = cell | None -> false)

    isOutside (V2d(-1.0, -2.0))
    isOutside (V2d( 0.5,  8.0))
    isOutside (V2d(10.0,  0.0))
    isOutside (V2d(10.0,  7.0))
    isOutside (V2d( 0.0,  7.0))

    isInside (V2d(0.0, 0.0)) (Cell2d(0,0,0))
    isInside (V2d(0.4, 0.6)) (Cell2d(0,0,0))
    isInside (V2d(1.0, 2.0)) (Cell2d(1,2,0))
    isInside (V2d(9.9, 6.9)) (Cell2d(9,6,0))

[<Fact>]
let ``TryGetSampleCell`` () =
    let q = createQuadtree ()

    let trySample p = Query.TryGetSampleCellAtPosition Query.Config.Default p q 

    let isOutside pos = Assert.True(match trySample pos with | Some c -> false | None -> true)
    let isInside pos cell = Assert.True(match trySample pos with | Some c -> c = cell | None -> false)

    isOutside (V2d(-1.0, -2.0))
    isOutside (V2d( 0.5,  8.0))
    isOutside (V2d(10.0,  0.0))
    isOutside (V2d(10.0,  7.0))
    isOutside (V2d( 0.0,  7.0))

    isInside (V2d(0.0, 0.0)) (Cell2d(0,0,0))
    isInside (V2d(0.4, 0.6)) (Cell2d(0,0,0))
    isInside (V2d(1.0, 2.0)) (Cell2d(1,2,0))
    isInside (V2d(9.9, 6.9)) (Cell2d(9,6,0))