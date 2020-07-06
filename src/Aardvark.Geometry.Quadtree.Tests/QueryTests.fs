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

    let config = { BuildConfig.Default with SplitLimit = 4 }
    Quadtree.Build config [| a |]

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