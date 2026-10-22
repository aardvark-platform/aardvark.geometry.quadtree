namespace Aardvark.Geometry.Quadtree.Tests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base
open System

module DataMappingTests =

    [<Fact>]
    let ``DataMapping.create non-centered succeeds`` () =
        let m = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
        Assert.True(m.Window = Box2l(V2l(1000,500), V2l(1020, 510)))

    [<Fact>]
    let ``DataMapping.create centered succeeds`` () =
        let m = DataMapping(Cell2d(2), V2i(1,1))
        Assert.True(m.BufferOrigin.IsCenteredAtOrigin)
        Assert.True(m.BufferOrigin.Exponent = 2)

    [<Fact>]
    let ``DataMapping.BoundingBox centered`` () =
        let m = DataMapping(Cell2d(2))
        Assert.True(m.BoundingBox = Cell2d(2).BoundingBox)

    [<Fact>]
    let ``DataMapping.BoundingBox non-centered`` () =
        let m = DataMapping(V2l(-3, 2), V2i(4, 2), -1)
        Assert.True(m.BoundingBox = Box2d(-1.5, 1.0, 0.5, 2.0))

    [<Fact>]
    let ``DataMapping.eq 1`` () =
        let a = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
        Assert.True((a = a))

    [<Fact>]
    let ``DataMapping.eq 2`` () =
        let a = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
        let b = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
        Assert.True((a = b))

    [<Fact>]
    let ``DataMapping.neq 1`` () =
        let a = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
        let b = DataMapping(V2l(1000, 500), V2i(20, 10), 1)
        Assert.False((a = b))

    [<Fact>]
    let ``DataMapping.neq 2`` () =
        let a = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
        let b = DataMapping(V2l(1000, 500), V2i(20, 11), 0)
        Assert.False((a = b))

    [<Fact>]
    let ``DataMapping.neq3`` () =
        let a = DataMapping(V2l(1000, 500), V2i(20, 10), 0)
        let b = DataMapping(V2l(1000, 501), V2i(20, 10), 0)
        Assert.False((a = b))