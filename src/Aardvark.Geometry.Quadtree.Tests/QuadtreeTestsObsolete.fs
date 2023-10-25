namespace Aardvark.Geometry.Quadtree.Tests

open Xunit
open Aardvark.Geometry.Quadtree
open Aardvark.Base
open Aardvark.Data

#nowarn "44"
#nowarn "3560"

module QuadtreeTestsObsolete =

    let private createQuadtree' (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int) (layerDef : Durable.Def) (createSample : int -> int -> 'a) =
        let size = V2i(w, h)
        let xs = Array.zeroCreate<'a> (w * h)
        for y = 0 to size.Y - 1 do
            for x = 0 to size.X - 1 do
                let i = y * size.X + x
                xs.[i] <- createSample x y

        let a = Layer(layerDef, xs, DataMapping(V2l(ox, oy), size, exponent = e))

        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = splitLimit }
        Quadtree.Build config [| a |]

    [<Fact>]
    let ``Build_BilinearParams4f`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.BilinearParams4f (fun x y -> V4f.Zero)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)

    [<Fact>]
    let ``Build_BilinearParams4d`` () =

        let q = createQuadtree' 0 0 10 7 0 2 Defs.BilinearParams4d (fun x y -> V4d.Zero)
        Assert.True(Quadtree.CountLeafs true q = 6)
        Assert.True(Quadtree.CountNodes true q = Quadtree.CountInner true q + Quadtree.CountLeafs true q)
