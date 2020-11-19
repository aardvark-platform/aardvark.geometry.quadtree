module StructureTests

open Aardvark.Geometry.Quadtree
open Aardvark.Base
open System
open Xunit

let createQuadtree (origin : Cell2d) (size : V2i) (data : float[]) =
    let mapping = DataMapping(origin, size)
    let layer = Layer(Defs.Heights1d, data, mapping)
    Quadtree.Build BuildConfig.Default [| layer |]

let getAllSamples n =
    n |> Query.All Query.Config.Default |> Seq.map (fun x -> x.GetSamples<float>(Defs.Heights1d)) |> Seq.concat |> Seq.toArray

let getAllSamplesAtLevel level n =
    let config = { Query.Config.Default with MinExponent=level }
    n |> Query.All config |> Seq.map (fun x -> x.GetSamples<float>(Defs.Heights1d)) |> Seq.concat |> Seq.toArray


(************************************************************************************
    SplitCenteredNodeIntoQuadrantNodesAtSameLevel
 ************************************************************************************)

[<Fact>]
let SplitCenteredNodeIntoQuadrantNodesAtSameLevel_FailsForNonCentered () =
    
    let aRef = createQuadtree (Cell2d(0,0,0)) (V2i(2,2)) [|
        1.0;  2.0; 
        3.0;  4.0;
        |]
    
    let a = aRef.TryGetInMemory().Value
    Assert.ThrowsAny<Exception>(fun () ->
        a.SplitCenteredNodeIntoQuadrantNodesAtSameLevel() |> ignore
        )

[<Fact>]
let SplitCenteredNodeIntoQuadrantNodesAtSameLevel () =

    let aRef = createQuadtree (Cell2d(-1,-1,0)) (V2i(2,2)) [|
        1.0;  2.0; 
        3.0;  4.0;
        |]

    let getSample (node : QNode) (cell : Cell2d) =
        let xs = getAllSamples (InMemoryNode node)
        if xs.Length <> 1 then failwith "Expected exactly 1 sample."
        let (c, x) = xs.[0]
        if c <> cell then failwith "Wrong sample cell."
        x

    let a = aRef.TryGetInMemory().Value
    let ls = a.SplitCenteredNodeIntoQuadrantNodesAtSameLevel()
    Assert.True(ls.Length = 4)

    let n = ls.[0]
    Assert.True(n.Cell                          = Cell2d(-1,-1,8))
    Assert.True(n.SampleExponent                = 0)
    Assert.True(n.SampleWindow                  = Box2l(V2l(-1,-1), V2l(0,0)))
    Assert.True(getSample n (Cell2d(-1,-1,0))   = 1.0)

    let n = ls.[1]
    Assert.True(n.Cell                          = Cell2d(0,-1,8))
    Assert.True(n.SampleExponent                = 0)
    Assert.True(n.SampleWindow                  = Box2l(V2l(0,-1), V2l(1,0)))
    Assert.True(getSample n (Cell2d(0,-1,0))   = 2.0)

    let n = ls.[2]
    Assert.True(n.Cell                          = Cell2d(-1,0,8))
    Assert.True(n.SampleExponent                = 0)
    Assert.True(n.SampleWindow                  = Box2l(V2l(-1,0), V2l(0,1)))
    Assert.True(getSample n (Cell2d(-1,0,0))   = 3.0)

    let n = ls.[3]
    Assert.True(n.Cell                          = Cell2d(0,0,8))
    Assert.True(n.SampleExponent                = 0)
    Assert.True(n.SampleWindow                  = Box2l(V2l(0,0), V2l(1,1)))
    Assert.True(getSample n (Cell2d(0,0,0))   = 4.0)


(************************************************************************************
    create leafs
 ************************************************************************************)

[<Fact>]
let ``CreateLeaf_NonCentered`` () =

    let aRef = createQuadtree (Cell2d(0,0,0)) (V2i(2,2)) [|
        1.0;  2.0; 
        3.0;  4.0;
        |]

    let a = aRef.TryGetInMemory().Value
    Assert.True(a.IsLeafNode             = true)
    Assert.True(a.OriginalSampleExponent = 0)
    Assert.True(a.SampleExponent         = 0)
    Assert.True(a.SplitLimitExponent     = 8)

    let xs = getAllSamples aRef
    Assert.True(xs.[0] = ((Cell2d(0,0,0), 1.0)))
    Assert.True(xs.[1] = ((Cell2d(1,0,0), 2.0)))
    Assert.True(xs.[2] = ((Cell2d(0,1,0), 3.0)))
    Assert.True(xs.[3] = ((Cell2d(1,1,0), 4.0)))

    ()

[<Fact>]
let ``CreateLeaf_Centered`` () =

    let aRef = createQuadtree (Cell2d(-1,-1,0)) (V2i(2,2)) [|
        1.0;  2.0; 
        3.0;  4.0;
        |]

    let a = aRef.TryGetInMemory().Value
    Assert.True(a.IsLeafNode             = true)
    Assert.True(a.OriginalSampleExponent = 0)
    Assert.True(a.SampleExponent         = 0)
    Assert.True(a.SplitLimitExponent     = 8)

    let xs = getAllSamples aRef
    Assert.True(xs.[0] = ((Cell2d(-1,-1,0), 1.0)))
    Assert.True(xs.[1] = ((Cell2d( 0,-1,0), 2.0)))
    Assert.True(xs.[2] = ((Cell2d(-1, 0,0), 3.0)))
    Assert.True(xs.[3] = ((Cell2d( 0, 0,0), 4.0)))

    ()


(************************************************************************************
    extendUpTo
 ************************************************************************************)

[<Fact>]
let ``ExtendUpTo_NonCentered_To_NonCentered`` () =

    let aRef = createQuadtree (Cell2d(0,0,0)) (V2i(2,2)) [|
        1.0;  2.0; 
        3.0;  4.0;
        |]

    let a = aRef.TryGetInMemory().Value
    Assert.True(a.Cell                   = Cell2d(0,0,8))
    Assert.True(a.IsLeafNode             = true)
    Assert.True(a.OriginalSampleExponent = 0)
    Assert.True(a.SampleExponent         = 0)
    Assert.True(a.SplitLimitExponent     = 8)

    let eRef = QNode.extendUpTo (Cell2d(0,0,9)) aRef
    let e = eRef.TryGetInMemory().Value
    Assert.True(e.Cell                   = Cell2d(0,0,9))
    Assert.True(e.IsLeafNode             = false)
    Assert.True(e.OriginalSampleExponent = 0)
    Assert.True(e.SampleExponent         = 1)
    Assert.True(e.SplitLimitExponent     = 8)

    let xs = getAllSamplesAtLevel 0 eRef
    Assert.True(xs.Length = 4)
    Assert.True(xs.[0] = ((Cell2d(0,0,0), 1.0)))
    Assert.True(xs.[1] = ((Cell2d(1,0,0), 2.0)))
    Assert.True(xs.[2] = ((Cell2d(0,1,0), 3.0)))
    Assert.True(xs.[3] = ((Cell2d(1,1,0), 4.0)))

    let ys = getAllSamplesAtLevel 1 eRef
    Assert.True(ys.[0] = ((Cell2d(0,0,1), 2.5)))

    ()

[<Fact>]
let ``ExtendUpTo_NonCentered_To_NonCentered_2_Steps`` () =

    let aRef = createQuadtree (Cell2d(0,0,0)) (V2i(4,4)) [|
         1.0;  2.0;  3.0;  4.0;
         5.0;  6.0;  7.0;  8.0;
         9.0; 10.0; 11.0; 12.0;
        13.0; 14.0; 15.0; 16.0;
        |]

    let a = aRef.TryGetInMemory().Value
    Assert.True(a.Cell                   = Cell2d(0,0,8))
    Assert.True(a.IsLeafNode             = true)
    Assert.True(a.OriginalSampleExponent = 0)
    Assert.True(a.SampleExponent         = 0)
    Assert.True(a.SplitLimitExponent     = 8)

    let eRef = QNode.extendUpTo (Cell2d(0,0,10)) aRef
    let e = eRef.TryGetInMemory().Value
    Assert.True(e.Cell                   = Cell2d(0,0,10))
    Assert.True(e.IsLeafNode             = false)
    Assert.True(e.OriginalSampleExponent = 0)
    Assert.True(e.SampleExponent         = 2)
    Assert.True(e.SplitLimitExponent     = 8)

    let xs = getAllSamplesAtLevel 0 eRef
    Assert.True(xs.Length = 16)
    let mutable k = 0
    for j = 0 to 3 do
        for i = 0 to 3 do
            Assert.True(xs.[k] = ((Cell2d(i,j,0), float (k+1))))
            k <- k + 1

    let ys = getAllSamplesAtLevel 1 eRef
    Assert.True(ys.Length = 4)
    Assert.True(ys.[0] = ((Cell2d(0,0,1),  3.5)))
    Assert.True(ys.[1] = ((Cell2d(1,0,1),  5.5)))
    Assert.True(ys.[2] = ((Cell2d(0,1,1), 11.5)))
    Assert.True(ys.[3] = ((Cell2d(1,1,1), 13.5)))

    let zs = getAllSamplesAtLevel 2 eRef
    Assert.True(zs.Length = 1)
    Assert.True(zs.[0] = ((Cell2d(0,0,2),  8.5)))

    ()

[<Fact>]
let ``ExtendUpTo_NonCentered_To_Centered`` () =

    let aRef = createQuadtree (Cell2d(0,0,0)) (V2i(2,2)) [|
        1.0;  2.0; 
        3.0;  4.0;
        |]

    let a = aRef.TryGetInMemory().Value
    Assert.True(a.Cell                   = Cell2d(0,0,8))
    Assert.True(a.IsLeafNode             = true)
    Assert.True(a.OriginalSampleExponent = 0)
    Assert.True(a.SampleExponent         = 0)
    Assert.True(a.SplitLimitExponent     = 8)

    let eRef = QNode.extendUpTo (Cell2d(9)) aRef
    let e = eRef.TryGetInMemory().Value
    Assert.True(e.Cell                   = Cell2d(9))
    Assert.True(e.IsLeafNode             = false)
    Assert.True(e.OriginalSampleExponent = 0)
    Assert.True(e.SampleExponent         = 1)
    Assert.True(e.SplitLimitExponent     = 8)

    let xs = getAllSamplesAtLevel 0 eRef
    Assert.True(xs.Length = 4)
    Assert.True(xs.[0] = ((Cell2d(0,0,0), 1.0)))
    Assert.True(xs.[1] = ((Cell2d(1,0,0), 2.0)))
    Assert.True(xs.[2] = ((Cell2d(0,1,0), 3.0)))
    Assert.True(xs.[3] = ((Cell2d(1,1,0), 4.0)))

    let ys = getAllSamplesAtLevel 1 eRef
    Assert.True(ys.Length = 1)
    Assert.True(ys.[0] = ((Cell2d(0,0,1), 2.5)))

    ()

[<Fact>]
let ``ExtendUpTo_Centered_To_Centered`` () =

    let aRef = createQuadtree (Cell2d(-1,-1,0)) (V2i(2,2)) [|
        1.0;  2.0; 
        3.0;  4.0;
        |]

    let a = aRef.TryGetInMemory().Value
    Assert.True(a.Cell                   = Cell2d(8))
    Assert.True(a.IsLeafNode             = true)
    Assert.True(a.OriginalSampleExponent = 0)
    Assert.True(a.SampleExponent         = 0)
    Assert.True(a.SplitLimitExponent     = 8)

    let eRef = QNode.extendUpTo (Cell2d(9)) aRef
    let e = eRef.TryGetInMemory().Value
    Assert.True(e.Cell                   = Cell2d(9))
    Assert.True(e.IsLeafNode             = false)
    Assert.True(e.OriginalSampleExponent = 0)
    Assert.True(e.SampleExponent         = 1)
    Assert.True(e.SplitLimitExponent     = 8)

    let xs = getAllSamplesAtLevel 0 eRef
    Assert.True(xs.Length = 4)
    Assert.True(xs.[0] = ((Cell2d(-1,-1,0), 1.0)))
    Assert.True(xs.[1] = ((Cell2d( 0,-1,0), 2.0)))
    Assert.True(xs.[2] = ((Cell2d(-1, 0,0), 3.0)))
    Assert.True(xs.[3] = ((Cell2d( 0, 0,0), 4.0)))

    let ys = getAllSamplesAtLevel 1 eRef
    Assert.True(ys.Length = 4)
    // centered parents again have all 4 samples, but with sample exponent + 1
    Assert.True(ys.[0] = ((Cell2d(-1,-1,1), 1.0)))
    Assert.True(ys.[1] = ((Cell2d( 0,-1,1), 2.0)))
    Assert.True(ys.[2] = ((Cell2d(-1, 0,1), 3.0)))
    Assert.True(ys.[3] = ((Cell2d( 0, 0,1), 4.0)))

    ()


(************************************************************************************
    merges
 ************************************************************************************)

[<Fact>]
let ``Merge_NoNodes`` () =

    let a = createQuadtree (Cell2d(0,0,0)) (V2i(2,2)) [|
        1.0;  2.0; 
        3.0;  4.0;
        |]

    let b = createQuadtree (Cell2d(0,0,0)) (V2i(2,2)) [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
        |]

    match Quadtree.Merge NoNode NoNode MoreDetailedDominates with
    | NoNode -> Assert.True(true)
    | _      -> Assert.True(false)

    match (Quadtree.Merge a NoNode MoreDetailedDominates).TryGetInMemory() with
    | None   -> Assert.True(false)
    | Some n -> Assert.True(n.Id = a.Id)

    match (Quadtree.Merge NoNode b MoreDetailedDominates).TryGetInMemory() with
    | None   -> Assert.True(false)
    | Some n -> Assert.True(n.Id = b.Id)

    ()

[<Fact>]
let ``MergePerfectlyAlignedLeafs_Centered`` () =

    let a = createQuadtree (Cell2d(-1,-1,0)) (V2i(2,2)) [|
        1.0;  2.0; 
        3.0;  4.0;
        |]

    let b = createQuadtree (Cell2d(-1,-1,0)) (V2i(2,2)) [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
        |]

    // first dominates
    let nRef = Quadtree.Merge a b FirstDominates
    let n = nRef.TryGetInMemory().Value
    Assert.True(n.IsLeafNode             = true)
    Assert.True(n.OriginalSampleExponent = 0)
    Assert.True(n.SampleExponent         = 0)
    Assert.True(n.SplitLimitExponent     = 8)
    let xs = getAllSamples nRef
    Assert.True(xs.Length = 4)
    Assert.True(xs.[0] = ((Cell2d(-1,-1,0), 1.0)))
    Assert.True(xs.[1] = ((Cell2d( 0,-1,0), 2.0)))
    Assert.True(xs.[2] = ((Cell2d(-1, 0,0), 3.0)))
    Assert.True(xs.[3] = ((Cell2d( 0, 0,0), 4.0))) 
        
    // second dominates
    let nRef = Quadtree.Merge a b SecondDominates
    let n = nRef.TryGetInMemory().Value
    Assert.True(n.IsLeafNode             = true)
    Assert.True(n.OriginalSampleExponent = 0)
    Assert.True(n.SampleExponent         = 0)
    Assert.True(n.SplitLimitExponent     = 8)
    let xs = getAllSamples nRef
    Assert.True(xs.Length = 4)
    Assert.True(xs.[0] = ((Cell2d(-1,-1,0), -1.0)))
    Assert.True(xs.[1] = ((Cell2d( 0,-1,0), -2.0)))
    Assert.True(xs.[2] = ((Cell2d(-1, 0,0), -3.0)))
    Assert.True(xs.[3] = ((Cell2d( 0, 0,0), -4.0)))

    ()

[<Fact>]
let ``MergePerfectlyAlignedLeafs_NonCentered`` () =

    let a = createQuadtree (Cell2d(0,0,0)) (V2i(2,2)) [|
        1.0;  2.0; 
        3.0;  4.0;
        |]

    let b = createQuadtree (Cell2d(0,0,0)) (V2i(2,2)) [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
        |]

    // first dominates
    let nRef = Quadtree.Merge a b FirstDominates
    let n = nRef.TryGetInMemory().Value
    Assert.True(n.IsLeafNode             = true)
    Assert.True(n.OriginalSampleExponent = 0)
    Assert.True(n.SampleExponent         = 0)
    Assert.True(n.SplitLimitExponent     = 8)
    let xs = getAllSamples nRef
    Assert.True(xs.Length = 4)
    Assert.True(xs.[0] = ((Cell2d(0,0,0), 1.0)))
    Assert.True(xs.[1] = ((Cell2d(1,0,0), 2.0)))
    Assert.True(xs.[2] = ((Cell2d(0,1,0), 3.0)))
    Assert.True(xs.[3] = ((Cell2d(1,1,0), 4.0)))
        
    // second dominates
    let nRef = Quadtree.Merge a b SecondDominates
    let n = nRef.TryGetInMemory().Value
    Assert.True(n.IsLeafNode             = true)
    Assert.True(n.OriginalSampleExponent = 0)
    Assert.True(n.SampleExponent         = 0)
    Assert.True(n.SplitLimitExponent     = 8)
    let xs = getAllSamples nRef
    Assert.True(xs.Length = 4)
    Assert.True(xs.[0] = ((Cell2d(0,0,0), -1.0)))
    Assert.True(xs.[1] = ((Cell2d(1,0,0), -2.0)))
    Assert.True(xs.[2] = ((Cell2d(0,1,0), -3.0)))
    Assert.True(xs.[3] = ((Cell2d(1,1,0), -4.0)))

    ()
