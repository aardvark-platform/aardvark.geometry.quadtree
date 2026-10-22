namespace Aardvark.Geometry.Quadtree.Tests

open Aardvark.Base
open Aardvark.Geometry.Quadtree
open Xunit

module BuilderTests =

    let private value (x : int64) (y : int64) = float y * 1000.0 + float x

    let private createPatch (ox : int) (oy : int) (w : int) (h : int) (e : int) =
        let xs = Array.init (w * h) (fun k -> value (int64 (ox + k % w)) (int64 (oy + k / w)))
        let layer = Layer(Defs.Heights1d, xs, DataMapping(V2l(ox, oy), V2i(w, h), exponent = e))
        LayerSet([| layer :> ILayer |])

    /// All patch samples (non-overlapping patches) must be returned exactly once with their value.
    let private assertSamples (patches : LayerSet list) (q : QNodeRef) =
        let expected =
            patches
            |> List.collect (fun p -> [ for c in p.SampleWindow.GetAllSamples(p.SampleExponent) -> (c, value c.X c.Y) ])
            |> List.sortBy (fun (c, _) -> (c.Exponent, c.X, c.Y))
        let actual =
            Query.All Query.Config.Default q
            |> Seq.collect (fun r -> r.GetSamples<float> Defs.Heights1d)
            |> List.ofSeq
            |> List.sortBy (fun (c, _) -> (c.Exponent, c.X, c.Y))
        Assert.True((actual = expected), sprintf "samples %A, expected %A" actual expected)

    let private build (splitLimit : int) (patches : LayerSet list) =
        let builder = Builder()
        builder.AddRange(patches)
        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = splitLimit }
        (builder.Build config).Value

    /// Every child node must have the cell of the corresponding parent quadrant.
    let rec private assertChildCellsMatchQuadrants (n : QNodeRef) =
        match n with
        | InMemoryInner x ->
            let quadrants = x.Cell.Children
            for i = 0 to 3 do
                match x.SubNodes.[i] with
                | NoNode -> ()
                | sub ->
                    Assert.True((sub.Cell = quadrants.[i]), sprintf "child %d of %A has cell %A, expected %A" i x.Cell sub.Cell quadrants.[i])
                    assertChildCellsMatchQuadrants sub
        | _ -> ()

    [<Fact>]
    let ``Build_Centered_MaxOnCellBoundary`` () =
        let patches = [ createPatch -2 -2 2 2 0; createPatch 0 -2 2 2 0; createPatch -2 0 2 2 0; createPatch 0 0 2 2 0 ]
        let q = build 2 patches
        assertChildCellsMatchQuadrants q
        assertSamples patches q
        Assert.True(q.Cell = Cell2d(2))

    [<Fact>]
    let ``Build_MaxOnCellBoundary`` () =
        let patches = [ createPatch 0 0 2 2 0; createPatch 2 0 2 2 0; createPatch 0 2 2 2 0 ]
        let q = build 2 patches
        assertChildCellsMatchQuadrants q
        assertSamples patches q
        Assert.True(q.Cell = Cell2d(0L, 0L, 2))

    [<Fact>]
    let ``Build_Unaligned`` () =
        let patches = [ createPatch 1 1 1 1 0; createPatch 2 1 1 1 0; createPatch 1 2 1 1 0 ]
        let q = build 2 patches
        assertChildCellsMatchQuadrants q
        assertSamples patches q
        Assert.True(q.Cell = Cell2d(0L, 0L, 2))

    [<Fact>]
    let ``Build_AboveSplitLimit`` () =
        let patches = [ createPatch 0 0 4 4 0; createPatch 4 0 4 4 0; createPatch 0 4 4 4 0; createPatch 4 4 2 2 0 ]
        let q = build 2 patches
        assertChildCellsMatchQuadrants q
        assertSamples patches q
        Assert.True(q.Cell = Cell2d(0L, 0L, 3))
