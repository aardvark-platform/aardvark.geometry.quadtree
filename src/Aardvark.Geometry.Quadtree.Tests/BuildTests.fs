module BuildTests

open System
open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base

[<Fact>]
let ``Build1`` () =

    let size = V2i(10, 7)
    let xs = Array.zeroCreate<float32> (size.X * size.Y)
    for y = 0 to size.Y - 1 do
        for x = 0 to size.X - 1 do
            let i = y * size.X + x
            xs.[i] <- float32 x + float32 y / 100.0f

    
    let a = Layer(Defs.Heights1f, xs, DataMapping(V2l.OO, size, exponent = 0))

    let config = { BuildConfig.Default with SplitLimit = 4 }
    let q = Quadtree.Build config [| a |]

    Assert.True(Quadtree.CountLeafs q = 6)
    Assert.True(Quadtree.CountNodes q = Quadtree.CountInner q + Quadtree.CountLeafs q)
