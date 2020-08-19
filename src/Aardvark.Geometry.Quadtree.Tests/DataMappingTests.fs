module DataMappingTests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base

[<Fact>]
let ``DataMapping_Create`` () =
    let m = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
    Assert.True(m.Window = Box2l(V2l(1000,500), V2l(1020, 510)))

[<Fact>]
let ``DataMapping_Create_Centered`` () =
    let m = DataMapping(Cell2d(2), V2i(1,1))
    Assert.True(m.BufferOrigin.IsCenteredAtOrigin)
    Assert.True(m.BufferOrigin.Exponent = 2)