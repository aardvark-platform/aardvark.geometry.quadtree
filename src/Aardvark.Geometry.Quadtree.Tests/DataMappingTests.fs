module DataMappingTests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base
open System

[<Fact>]
let ``create non-centered succeeds`` () =
    let m = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
    Assert.True(m.Window = Box2l(V2l(1000,500), V2l(1020, 510)))

[<Fact>]
let ``create centered succeeds`` () =
    let m = DataMapping(Cell2d(2), V2i(1,1))
    Assert.True(m.BufferOrigin.IsCenteredAtOrigin)
    Assert.True(m.BufferOrigin.Exponent = 2)

[<Fact>]
let ``eq 1`` () =
    let a = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
    Assert.True((a = a))

[<Fact>]
let ``eq 2`` () =
    let a = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
    let b = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
    Assert.True((a = b))

[<Fact>]
let ``neq 1`` () =
    let a = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
    let b = DataMapping(V2l(1000, 500), V2i(20, 10), 1)
    Assert.False((a = b))

[<Fact>]
let ``neq 2`` () =
    let a = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
    let b = DataMapping(V2l(1000, 500), V2i(20, 11), 0)
    Assert.False((a = b))

[<Fact>]
let ``neq3`` () =
    let a = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
    let b = DataMapping(V2l(1000, 501), V2i(20, 10), 0)
    Assert.False((a = b))