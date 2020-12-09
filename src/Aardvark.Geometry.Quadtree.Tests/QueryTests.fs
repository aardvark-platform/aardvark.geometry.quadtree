module QueryTests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base
open PrettyPrint

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
let ``All`` () =
    let q = createQuadtree ()
    let rs = Query.All Query.Config.Default q |> Seq.toArray
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(cs.Length = 70)
    Assert.True(xs.Length = 70)


[<Fact>]
let ``InsideCell2d_FullyInside`` () =
    let q = createQuadtree ()
    let rs = Query.InsideCell Query.Config.Default (Cell2d(0L, 0L, 4)) q |> Array.ofSeq
    
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 70)

[<Fact>]
let ``InsideCell2d_FullyOutside`` () =
    let q = createQuadtree ()
    let rs = Query.InsideCell Query.Config.Default (Cell2d(1L, 0L, 4)) q |> Array.ofSeq
    
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 0)

[<Fact>]
let ``InsideCell2d_Partial`` () =
    let q = createQuadtree ()
    let rs = Query.InsideCell Query.Config.Default (Cell2d(0L, 0L, 3)) q |> Array.ofSeq
    
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 56)

[<Fact>]
let ``InsideCell2d_ExactCellMatch`` () =
    let q = createQuadtree ()
    let r = Query.InsideCell Query.Config.Default (Cell2d(2L, 1L, 0)) q |> Array.ofSeq
    Assert.True(r.Length = 1)
    match r.[0].Selection with
    | Query.CellsSelected xs ->
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
    let rs = Query.IntersectsCell Query.Config.Default (Cell2d(0L, 0L, 4)) q |> Array.ofSeq
    
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 70)

[<Fact>]
let ``IntersectsCell2d_FullyOutside`` () =
    let q = createQuadtree ()
    let r = Query.IntersectsCell Query.Config.Default (Cell2d(1L, 0L, 4)) q |> Array.ofSeq
    Assert.True(r.Length = 0)

[<Fact>]
let ``IntersectsCell2d_Partial`` () =
    let q = createQuadtree ()
    let rs = Query.IntersectsCell Query.Config.Default (Cell2d(0L, 0L, 3)) q |> Array.ofSeq
    
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 56)

[<Fact>]
let ``IntersectsCell2d_ExactCellMatch`` () =
    let q = createQuadtree ()
    
    let rs = Query.IntersectsCell Query.Config.Default (Cell2d(2L, 1L, 0)) q |> Array.ofSeq
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 1)
    
    let (c, x) = xs.[0]
    Assert.True(Cell2d(2L, 1L, 0) = c)
    Assert.True(2.01f = x)

[<Fact>]
let ``IntersectsCell2d_Supersampling`` () =
    let q = createQuadtree ()
    
    let rs = Query.IntersectsCell Query.Config.Default (Cell2d(4L, 2L, -1)) q |> Array.ofSeq
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 1)

    let (c, x) = xs.[0]
    Assert.True(Cell2d(2L, 1L, 0) = c)
    Assert.True(2.01f = x)

[<Fact>]
let ``InsideBox2d_FullyInside`` () =
    let q = createQuadtree ()
    let rs = Query.InsideBox Query.Config.Default (Box2d(V2d(0,0),V2d(10,7))) q |> Array.ofSeq
    
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 70)

[<Fact>]
let ``InsideBox2d_FullyOutside`` () =
    let q = createQuadtree ()
    let r = Query.InsideBox Query.Config.Default (Box2d(V2d(10,1),V2d(100,100))) q |> Array.ofSeq
    Assert.True(r.Length = 0)

[<Fact>]
let ``InsideBox2d_Partial`` () =
    let q = createQuadtree ()
    let rs = Query.InsideBox Query.Config.Default (Box2d(V2d(-1,-1),V2d(5,3))) q |> Array.ofSeq
    
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 15)


[<Fact>]
let ``InsidePolygon2d_FullyInside`` () =
    let q = createQuadtree ()
    let filter = Polygon2d([|V2d(3.9,-0.1);V2d(10.1,-0.1);V2d(10.1,7.1);V2d(8.1,7.1);V2d(8.1,4.1);V2d(3.9,4.1)|])
    let rs = Query.InsidePolygon Query.Config.Default filter q |> Array.ofSeq
    
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 30)

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
    let rs = Query.InsidePolygon Query.Config.Default filter q |> Array.ofSeq
    
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 18)


[<Fact>]
let ``NearLine2d_FullyInside`` () =
    let q = createQuadtree ()
    let filter = Ray2d(origin = V2d.OO, direction = V2d(10,7).Normalized)
    let rs = Query.NearLine Query.Config.Default filter 10.0 q |> Array.ofSeq
    
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 70)

[<Fact>]
let ``NearLine2d_FullyOutside`` () =
    let q = createQuadtree ()
    let filter = Ray2d(origin = V2d(10,-2), direction = V2d(10,7).Normalized)
    let rs = Query.NearLine Query.Config.Default filter 1.0 q |> Array.ofSeq
    
    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 0)

[<Fact>]
let ``NearLine2d_Partial`` () =
    let q = createQuadtree ()
    let filter = Ray2d(origin = V2d(0,0), direction = V2d(3,4).Normalized)
    let rs = Query.NearLine Query.Config.Default filter 1.0 q |> Array.ofSeq

    let cs = rs |> Seq.collect (fun x -> x.GetSampleCells()) |> Seq.toArray
    let xs = rs |> Seq.collect (fun x -> x.GetSamples<float32>(Defs.Heights1f)) |> Seq.toArray
    Assert.True(xs.Length = 16)




//[<Fact>]
//let ``Position`` () =
//    let q = createQuadtree ()

//    let trySample p =
//            Sample.Position Query.Config.Default p q 
//            |> Seq.collect (fun r -> r.GetSamples<float32> Defs.Heights1f)
//            |> Seq.tryExactlyOne

//    let isOutside pos = Assert.True(match trySample pos with | Some (p, c, h) -> false | None -> true)
//    let isInside pos cell = Assert.True(match trySample pos with | Some (p, c, h) -> c = cell | None -> false)

//    isOutside (V2d(-1.0, -2.0))
//    isOutside (V2d( 0.5,  8.0))
//    isOutside (V2d(10.0,  0.0))
//    isOutside (V2d(10.0,  7.0))
//    isOutside (V2d( 0.0,  7.0))

//    isInside (V2d(0.0, 0.0)) (Cell2d(0,0,0))
//    isInside (V2d(0.4, 0.6)) (Cell2d(0,0,0))
//    isInside (V2d(1.0, 2.0)) (Cell2d(1,2,0))
//    isInside (V2d(9.9, 6.9)) (Cell2d(9,6,0))

//[<Fact>]
//let ``TryGetCellAtPosition`` () =
//    let q = createQuadtree ()

//    let trySample p = Sample.TryGetCellAtPosition Query.Config.Default p q 

//    let isOutside pos = Assert.True(match trySample pos with | Some c -> false | None -> true)
//    let isInside pos cell = Assert.True(match trySample pos with | Some c -> c = cell | None -> false)

//    isOutside (V2d(-1.0, -2.0))
//    isOutside (V2d( 0.5,  8.0))
//    isOutside (V2d(10.0,  0.0))
//    isOutside (V2d(10.0,  7.0))
//    isOutside (V2d( 0.0,  7.0))

//    isInside (V2d(0.0, 0.0)) (Cell2d(0,0,0))
//    isInside (V2d(0.4, 0.6)) (Cell2d(0,0,0))
//    isInside (V2d(1.0, 2.0)) (Cell2d(1,2,0))
//    isInside (V2d(9.9, 6.9)) (Cell2d(9,6,0))

[<Fact>]
let ``Positions`` () =
    let q = createQuadtree ()

    Quadtree.printStructure q

    let ps = [| V2d(0.0, 0.0); V2d(0.4, 0.6); V2d(1.0, 2.0); V2d(3.5, 2.1); V2d(9.9, 6.9) |]
    let r = Sample.Positions { Query.Config.Default with Verbose = true } ps q |> Seq.toArray

    let count = r |> Array.sumBy (fun x -> x.Cells.Length)
    count = 5       |> Assert.True

    let xs = r |> Array.collect (fun x -> x.GetSamples<float32> Defs.Heights1f)
    xs.Length = 5   |> Assert.True

    