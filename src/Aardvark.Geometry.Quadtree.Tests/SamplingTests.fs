module SamplingTests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base

[<Fact>]
let ``Resample1`` () =

    let xs = [| 
        1.0;   2.0;  3.0;  4.0
        5.0;   6.0;  7.0;  8.0
        9.0;  10.0; 11.0; 12.0
        13.0; 14.0; 15.0; 16.0
        |]

    let ys = [|
        (1.0+ 2.0+ 5.0+ 6.0)/4.0; ( 3.0+ 4.0+ 7.0+ 8.0)/4.0
        (9.0+10.0+13.0+14.0)/4.0; (11.0+12.0+15.0+16.0)/4.0
        |]

    let a = Layer(Defs.Heights1f, xs, DataMapping(V2l.OO, V2i(4, 4), 0))

    let b = a.Resample ClampToEdge (fun (a,b,c,d) -> (a+b+c+d)/4.0)

    Assert.True(b.Data.Length = 4)
    Assert.True(Array.forall2 (=) b.Data ys)

[<Fact>]
let ``Resample2`` () =

    let xs = [|
        0.0;  0.0;  0.0;  0.0;  0.0; 0.0
        0.0;  1.0;  2.0;  3.0;  4.0; 0.0
        0.0;  5.0;  6.0;  7.0;  8.0; 0.0
        0.0;  9.0; 10.0; 11.0; 12.0; 0.0
        0.0; 13.0; 14.0; 15.0; 16.0; 0.0
        0.0;  0.0;  0.0;  0.0;  0.0; 0.0
        |]

    let ys = [|
        (1.0+ 1.0+ 1.0+ 1.0)/4.0; ( 2.0+ 3.0+ 2.0+ 3.0)/4.0
        (5.0+ 5.0+ 9.0+ 9.0)/4.0; ( 6.0+ 7.0+10.0+11.0)/4.0
        |]

    let origin = Cell2d(0L,0L,0)
    let size = V2i(6,6)
    let a = Layer(Defs.Heights1f, xs, DataMapping(origin, size, Box2l(V2l(1,1), V2l(4,4))))

    let b = a.Resample ClampToEdge (fun (a,b,c,d) -> (a+b+c+d)/4.0)

    Assert.True(b.Data.Length = 4)
    Assert.True(Array.forall2 (=) b.Data ys)
