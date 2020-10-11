module PreludeTests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base

[<Fact>]
let ``Prelude getBoundsForExponent`` () =
    Assert.True(Cell2d(0, 0, 0).GetBoundsForExponent(-2) = Box2l(0L, 0L, 4L, 4L))
    Assert.True(Cell2d(-1, -1, 0).GetBoundsForExponent(-2) = Box2l(-4L, -4L, 0L, 0L))

[<Fact>]
let ``Prelude minInt64`` () =
    Assert.True(minInt64 -5L 5L = -5L)
    Assert.True(minInt64 5L -5L = -5L)
    Assert.True(maxInt64 -5L 5L = 5L)
    Assert.True(maxInt64 5L -5L = 5L)

[<Fact>]
let ``Prelude tryIntersect identical`` () =
    let a = Box2l(0L, 0L, 8L, 6L)
    let b = Box2l(0L, 0L, 8L, 6L)
    Assert.True(a.TryIntersect(b) = Some (Box2l(0L, 0L, 8L, 6L)))
    Assert.True(b.TryIntersect(a) = Some (Box2l(0L, 0L, 8L, 6L)))

[<Fact>]
let ``Prelude tryIntersect inside`` () =
    let a = Box2l(0L, 0L, 8L, 6L)
    let b = Box2l(2L, 3L, 4L, 4L)
    Assert.True(a.TryIntersect(b) = Some (Box2l(2L, 3L, 4L, 4L)))
    Assert.True(b.TryIntersect(a) = Some (Box2l(2L, 3L, 4L, 4L)))

[<Fact>]
let ``Prelude tryIntersect partial`` () =
    let a = Box2l(0L, 0L, 8L, 6L)
    let b = Box2l(2L, 3L, 16L, 10L)
    Assert.True(a.TryIntersect(b) = Some (Box2l(2L, 3L, 8L, 6L)))
    Assert.True(b.TryIntersect(a) = Some (Box2l(2L, 3L, 8L, 6L)))

[<Fact>]
let ``Prelude tryIntersect outside 1`` () =
    let a = Box2l(0L, 0L, 8L, 6L)
    let b = Box2l(8L, 0L, 9L, 6L)
    Assert.True(a.TryIntersect(b) = None)
    Assert.True(b.TryIntersect(a) = None)

[<Fact>]
let ``Prelude tryIntersect outside 2`` () =
    let a = Box2l(0L, 0L, 8L, 6L)
    let b = Box2l(-2L, 0L, 0L, 6L)
    Assert.True(a.TryIntersect(b) = None)
    Assert.True(b.TryIntersect(a) = None)

[<Fact>]
let ``Prelude tryIntersect outside 3`` () =
    let a = Box2l(0L, 0L, 8L, 6L)
    let b = Box2l(0L, 6L, 8L, 8L)
    Assert.True(a.TryIntersect(b) = None)
    Assert.True(b.TryIntersect(a) = None)

[<Fact>]
let ``Prelude tryIntersect outside 4`` () =
    let a = Box2l(0L, 0L, 8L, 6L)
    let b = Box2l(0L, -2L, 8L, 0L)
    Assert.True(a.TryIntersect(b) = None)
    Assert.True(b.TryIntersect(a) = None)

[<Fact>]
let ``Prelude IntersectsMaxExclusive`` () =

    // identical
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 0L,  0L,  4L,  5L)))

    // outside
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L, -9L, -8L, -8L))) // left/bottom
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 0L, -9L,  1L, -8L))) // bottom
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 8L, -9L,  9L, -8L))) // right/bottom
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 8L,  1L,  9L,  3L))) // right
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 8L,  8L,  9L,  9L))) // right/top
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 1L,  8L,  3L,  9L))) // top
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L,  8L, -8L,  9L))) // left/top
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L,  1L, -8L,  3L))) // left

    // outside (touching)
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L, -9L,  0L,  0L))) // left/bottom
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 0L, -9L,  1L,  0L))) // bottom
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 3L, -9L,  9L,  0L))) // right/bottom
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 3L,  1L,  9L,  3L))) // right
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 3L,  4L,  9L,  9L))) // right/top
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 1L,  4L,  3L,  9L))) // top
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L,  4L,  0L,  9L))) // left/top
    Assert.False(Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L,  1L,  0L,  3L))) // left

    // overlapping (side)
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-3L,  1L,  1L,  3L))) // left
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 0L, -2L,  1L,  1L))) // bottom
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 2L,  1L,  6L,  3L))) // right
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 1L,  3L,  3L,  8L))) // top

    // overlapping (horizontal)
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L, -9L,  9L,  2L))) // bottom
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L,  1L,  9L,  3L))) // middle
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L,  3L,  9L,  9L))) // top

    // overlapping (vertical)
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L, -9L,  1L,  9L))) // left
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 1L, -9L,  2L,  9L))) // center
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 2L, -9L,  9L,  9L))) // right

    // overlapping (corner)
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L, -9L,  1L,  1L))) // left/bottom
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 2L, -9L,  9L,  1L))) // right/bottom
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 2L,  3L,  9L,  9L))) // right/top
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l(-9L,  3L,  1L,  9L))) // left/top

    // overlapping (inside)
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 0L,  0L,  1L,  1L)))
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 2L,  0L,  3L,  1L)))
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 2L,  3L,  3L,  4L)))
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 0L,  3L,  1L,  4L)))
    Assert.True (Box2l(0L, 0L, 3L, 4L).IntersectsMaxExclusive(Box2l( 1L,  1L,  2L,  3L)))
