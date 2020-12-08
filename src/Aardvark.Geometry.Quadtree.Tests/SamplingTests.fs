module SamplingTests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base

//[<Fact>]
//let ``Supersample 1`` () =

//    let a = Layer(Defs.Heights1d, [| 1.0 |], DataMapping(V2l(0, 0), V2i(1, 1), 0))
//    let b = a.Supersample ()

//    b.Mapping.BufferOrigin = Cell2d(0,0,-1)     |> Assert.True
//    b.Mapping.BufferSize = V2i(2,2)             |> Assert.True
//    b.Data.Length = 4                           |> Assert.True

//    let check x y s = b.GetSample(Fail, Cell2d(int x, y, -1)) = s  |> Assert.True
//    check 0 0 1.0
//    check 1 0 1.0
//    check 0 1 1.0
//    check 1 1 1.0

//[<Fact>]
//let ``Supersample 1 windowed`` () =

//    let a = Layer(Defs.Heights1d, [| 1.0; 2.0; 3.0; 4.0 |], DataMapping(V2l(0, 0), V2i(2, 2), 0).WithWindow(Box2l(V2l(0,0),V2l(1,1))).Value)
//    let b = a.Supersample ()

//    b.Mapping.BufferOrigin = Cell2d(0,0,-1)     |> Assert.True
//    b.Mapping.BufferSize = V2i(2,2)             |> Assert.True
//    b.Data.Length = 4                           |> Assert.True

//    let check x y s = b.GetSample(Fail, Cell2d(int x, y, -1)) = s  |> Assert.True
//    check 0 0 1.0
//    check 1 0 1.0
//    check 0 1 1.0
//    check 1 1 1.0

//[<Fact>]
//let ``Supersample centered`` () =

//    let a = Layer(Defs.Heights1d, [| 1.0 |], DataMapping(Cell2d(0)))
//    let b = a.Supersample ()

//    b.Mapping.BufferOrigin = Cell2d(-1,-1,-1)   |> Assert.True
//    b.Mapping.BufferSize = V2i(2,2)             |> Assert.True
//    b.Data.Length = 4                           |> Assert.True

//    let check x y s = b.GetSample(Fail, Cell2d(int x, y, -1)) = s  |> Assert.True
//    check -1 -1 1.0
//    check  0 -1 1.0
//    check -1  0 1.0
//    check  0  0 1.0

//[<Fact>]
//let ``Supersample 2`` () =

//    let a = Layer(Defs.Heights1d, [| 1.0 |], DataMapping(V2l(-1, 3), V2i(1, 1), 0))
//    let b = a.Supersample ()

//    b.Mapping.BufferOrigin = Cell2d(-2,6,-1)    |> Assert.True
//    b.Mapping.BufferSize = V2i(2,2)             |> Assert.True
//    b.Data.Length = 4                           |> Assert.True

//    let check x y s = b.GetSample(Fail, Cell2d(int x, y, -1)) = s  |> Assert.True
//    check -2 6 1.0 
//    check -1 6 1.0 
//    check -2 7 1.0 
//    check -1 7 1.0

//[<Fact>]
//let ``Supersample 2 windowed`` () =

//    let a = Layer(Defs.Heights1d, [| 1.0; 2.0; 3.0; 4.0 |], DataMapping(V2l(-1, 3), V2i(2, 2), 0).WithWindow(Box2l(V2l(-1,3),V2l(0,4))).Value)
//    let b = a.Supersample ()

//    b.Mapping.BufferOrigin = Cell2d(-2,6,-1)    |> Assert.True
//    b.Mapping.BufferSize = V2i(2,2)             |> Assert.True
//    b.Data.Length = 4                           |> Assert.True

//    let check x y s = b.GetSample(Fail, Cell2d(int x, y, -1)) = s  |> Assert.True
//    check -2 6 1.0 
//    check -1 6 1.0 
//    check -2 7 1.0 
//    check -1 7 1.0 

//[<Fact>]
//let ``Supersample 3`` () =

//    let a = Layer(Defs.Heights1d, [| 1.0; 2.0; |], DataMapping(V2l(-1, -1), V2i(2, 1), 0))
//    let b = a.Supersample ()

//    b.Mapping.BufferOrigin = Cell2d(-2,-2,-1)   |> Assert.True
//    b.Mapping.BufferSize = V2i(4,2)             |> Assert.True
//    b.Data.Length = 8                           |> Assert.True

//    let check x y s = b.GetSample(Fail, Cell2d(int x, y, -1)) = s  |> Assert.True

//    check -2 -2 1.0
//    check -1 -2 1.0
//    check -2 -1 1.0
//    check -1 -1 1.0

//    check  0 -2 2.0
//    check  1 -2 2.0
//    check  0 -1 2.0
//    check  1 -1 2.0

//[<Fact>]
//let ``Supersample 3 windowed`` () =

//    let a = Layer(Defs.Heights1d, [| 1.0; 2.0; 3.0; 4.0 |], DataMapping(V2l(-1, -1), V2i(2, 2), 0).WithWindow(Box2l(V2l(-1,-1),V2l(1,0))).Value)
//    let b = a.Supersample ()

//    b.Mapping.BufferOrigin = Cell2d(-2,-2,-1)   |> Assert.True
//    b.Mapping.BufferSize = V2i(4,2)             |> Assert.True
//    b.Data.Length = 8                           |> Assert.True

//    let check x y s = b.GetSample(Fail, Cell2d(int x, y, -1)) = s  |> Assert.True

//    check -2 -2 1.0
//    check -1 -2 1.0
//    check -2 -1 1.0
//    check -1 -1 1.0

//    check  0 -2 2.0
//    check  1 -2 2.0
//    check  0 -1 2.0
//    check  1 -1 2.0




//[<Fact>]
//let ``Resample 1`` () =

//    let xs = [| 
//        1.0;   2.0;  3.0;  4.0
//        5.0;   6.0;  7.0;  8.0
//        9.0;  10.0; 11.0; 12.0
//        13.0; 14.0; 15.0; 16.0
//        |]

//    let ys = [|
//        (1.0+ 2.0+ 5.0+ 6.0)/4.0; ( 3.0+ 4.0+ 7.0+ 8.0)/4.0
//        (9.0+10.0+13.0+14.0)/4.0; (11.0+12.0+15.0+16.0)/4.0
//        |]

//    let a = Layer(Defs.Heights1f, xs, DataMapping(V2l.OO, V2i(4, 4), 0))

//    let b = a.Resample ClampToEdge (fun (a,b,c,d) -> (a+b+c+d)/4.0) a.CellBounds

//    Assert.True(b.Data.Length = 4)
//    Assert.True(Array.forall2 (=) b.Data ys)

//[<Fact>]
//let ``Resample 2`` () =

//    let xs = [|
//        0.0;  0.0;  0.0;  0.0;  0.0; 0.0
//        0.0;  1.0;  2.0;  3.0;  4.0; 0.0
//        0.0;  5.0;  6.0;  7.0;  8.0; 0.0
//        0.0;  9.0; 10.0; 11.0; 12.0; 0.0
//        0.0; 13.0; 14.0; 15.0; 16.0; 0.0
//        0.0;  0.0;  0.0;  0.0;  0.0; 0.0
//        |]

//    let ys = [|
//        (1.0+ 1.0+ 1.0+ 1.0)/4.0; ( 2.0+ 3.0+ 2.0+ 3.0)/4.0
//        (5.0+ 5.0+ 9.0+ 9.0)/4.0; ( 6.0+ 7.0+10.0+11.0)/4.0
//        |]

//    let origin = Cell2d(0L,0L,0)
//    let size = V2i(6,6)
//    let a = Layer(Defs.Heights1f, xs, DataMapping(origin, size, Box2l(V2l(1,1), V2l(4,4))))

//    let b = a.Resample ClampToEdge (fun (a,b,c,d) -> (a+b+c+d)/4.0) a.CellBounds

//    Assert.True(b.Data.Length = 4)
//    Assert.True(Array.forall2 (=) b.Data ys)

//[<Fact>]
//let ``Resample 3`` () =

//    let xs = [| 
//        10.0; 20.0;
//        30.0; 40.0;
//        |]

//    let a = Layer(Defs.Heights1d, xs, DataMapping(V2l(1, 1), V2i(2, 2), 0))
//    Assert.True(a.Data.Length      = 4)
//    Assert.True(a.SampleExponent   = 0)
//    Assert.True(a.SampleWindow.Min = V2l(1, 1))
//    Assert.True(a.SampleWindow.Max = V2l(3, 3))
//    Assert.True(a.GetSample(Fail, Cell2d(1, 1, 0)) = 10.0)
//    Assert.True(a.GetSample(Fail, Cell2d(2, 1, 0)) = 20.0)
//    Assert.True(a.GetSample(Fail, Cell2d(1, 2, 0)) = 30.0)
//    Assert.True(a.GetSample(Fail, Cell2d(2, 2, 0)) = 40.0)

//    let b = a.Resample ClampToEdge (fun (a,b,c,d) -> (a+b+c+d)/4.0) a.CellBounds
//    Assert.True(b.Data.Length      = 4)
//    Assert.True(b.SampleExponent   = 1)
//    Assert.True(b.SampleWindow.Min = V2l(0, 0))
//    Assert.True(b.SampleWindow.Max = V2l(2, 2))
//    Assert.True(b.GetSample(Fail, Cell2d(0, 0, 1)) = 10.0)
//    Assert.True(b.GetSample(Fail, Cell2d(1, 0, 1)) = 20.0)
//    Assert.True(b.GetSample(Fail, Cell2d(0, 1, 1)) = 30.0)
//    Assert.True(b.GetSample(Fail, Cell2d(1, 1, 1)) = 40.0)

//    let c = b.Resample ClampToEdge (fun (a,b,c,d) -> (a+b+c+d)/4.0) b.CellBounds
//    Assert.True(c.Data.Length      = 1)
//    Assert.True(c.SampleExponent   = 2)
//    Assert.True(c.SampleWindow.Min = V2l(0, 0))
//    Assert.True(c.SampleWindow.Max = V2l(1, 1))
//    Assert.True(c.GetSample(Fail, Cell2d(0, 0, 2)) = 25.0)

//    ()

//[<Fact>]
//let ``Resample 4`` () =

//    let xs = [| 
//        10.0; 20.0;
//        30.0; 40.0;
//        |]

//    let a = Layer(Defs.Heights1d, xs, DataMapping(V2l(-1, -1), V2i(2, 2), 0))
//    Assert.True(a.Data.Length      = 4)
//    Assert.True(a.SampleExponent   = 0)
//    Assert.True(a.SampleWindow.Min = V2l(-1, -1))
//    Assert.True(a.SampleWindow.Max = V2l(+1, +1))
//    Assert.True(a.GetSample(Fail, Cell2d(-1, -1, 0)) = 10.0)
//    Assert.True(a.GetSample(Fail, Cell2d( 0, -1, 0)) = 20.0)
//    Assert.True(a.GetSample(Fail, Cell2d(-1,  0, 0)) = 30.0)
//    Assert.True(a.GetSample(Fail, Cell2d( 0,  0, 0)) = 40.0)

//    let b = a.Resample ClampToEdge (fun (a,b,c,d) -> (a+b+c+d)/4.0) a.CellBounds
//    Assert.True(b.Data.Length      = 1)
//    Assert.True(b.Mapping.BufferOrigin.IsCenteredAtOrigin)
//    Assert.True(b.SampleExponent   = 1)
//    Assert.True(b.SampleWindow.IsInvalid)
//    Assert.True(b.GetSample(Fail, Cell2d(1)) = 25.0)

//    ()

//[<Fact>]
//let ``Resample 5`` () =

//    let xs = [| 
//        10.0; 20.0;
//        30.0; 40.0;
//        |]

//    let a = Layer(Defs.Heights1d, xs, DataMapping(V2l(-1, -2), V2i(2, 2), 0))
//    Assert.True(a.Data.Length      = 4)
//    Assert.True(a.SampleExponent   = 0)
//    Assert.True(a.SampleWindow.Min = V2l(-1, -2))
//    Assert.True(a.SampleWindow.Max = V2l(+1,  0))
//    Assert.True(a.GetSample(Fail, Cell2d(-1, -2, 0)) = 10.0)
//    Assert.True(a.GetSample(Fail, Cell2d( 0, -2, 0)) = 20.0)
//    Assert.True(a.GetSample(Fail, Cell2d(-1, -1, 0)) = 30.0)
//    Assert.True(a.GetSample(Fail, Cell2d( 0, -1, 0)) = 40.0)

//    let b = a.Resample ClampToEdge (fun (a,b,c,d) -> (a+b+c+d)/4.0) a.CellBounds
//    Assert.True(b.Data.Length      = 2)
//    Assert.True(b.SampleExponent   = 1)
//    Assert.True(b.SampleWindow.Min = V2l(-1, -1))
//    Assert.True(b.SampleWindow.Max = V2l(+1,  0))
//    Assert.True(b.GetSample(Fail, Cell2d(-1, -1, 1)) = 20.0)
//    Assert.True(b.GetSample(Fail, Cell2d( 0, -1, 1)) = 30.0)

//    let c = b.Resample ClampToEdge (fun (a,b,c,d) -> (a+b+c+d)/4.0) b.CellBounds
//    Assert.True(c.Data.Length      = 1)
//    Assert.True(c.Mapping.BufferOrigin.IsCenteredAtOrigin)
//    Assert.True(c.SampleExponent   = 2)
//    Assert.True(c.SampleWindow.IsInvalid)
//    Assert.True(c.GetSample(Fail, Cell2d(2)) = 25.0)

//    ()