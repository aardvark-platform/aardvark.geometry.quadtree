module StructureTests

open Aardvark.Geometry.Quadtree
open Aardvark.Base
open System
open Xunit

type QuadtreeSpec = { Origin : (int*int*int); Size : (int*int); Data : float[]}

let createQuadtree spec =
    let (ox , oy, oe) = spec.Origin
    let (w, h) = spec.Size
    let mapping = DataMapping(Cell2d(int64 ox, int64 oy, oe), V2i(w,h))
    let layer = Layer(Defs.Heights1d, spec.Data, mapping)
    Quadtree.Build BuildConfig.Default [| layer |]

let getAllSamples n =
    n |> Query.All Query.Config.Default |> Seq.map (fun x -> x.GetSamples<float>(Defs.Heights1d)) |> Seq.concat |> Seq.toArray

let getAllSamplesAtLevel level n =
    let config = { Query.Config.Default with MinExponent=level }
    n |> Query.All config |> Seq.map (fun x -> x.GetSamples<float>(Defs.Heights1d)) |> Seq.concat |> Seq.toArray

type QuadtreeCheckSamples = { Level : int; Data : ((int*int*int)*float) seq }
type QuadtreeCheck = {
    Cell : Cell2d
    IsLeafNode : bool
    OriginalSampleExponent : int
    SampleExponent : int
    SplitLimitExponent : int
    Samples : QuadtreeCheckSamples seq
}

let checkQuadtree spec (rootRef : QNodeRef) =
    let root = rootRef.TryGetInMemory().Value
    Assert.True(spec.Cell                   = rootRef.Cell)
    Assert.True(spec.IsLeafNode             = root.IsLeafNode)
    Assert.True(spec.OriginalSampleExponent = root.OriginalSampleExponent)
    Assert.True(spec.SampleExponent         = root.SampleExponent)
    Assert.True(spec.SplitLimitExponent     = root.SplitLimitExponent)

    for s in spec.Samples do
        let map = s.Data |> Map.ofSeq
        let samples = getAllSamplesAtLevel s.Level rootRef
        Assert.True(map.Count = samples.Length)
        for (cell, value) in samples do
            let k = (int cell.X, int cell.Y, cell.Exponent)
            Assert.True(map |> Map.find k = value)

    rootRef
    

(************************************************************************************
    SplitCenteredNodeIntoQuadrantNodesAtSameLevel
 ************************************************************************************)

[<Fact>]
let SplitCenteredNodeIntoQuadrantNodesAtSameLevel_FailsForNonCentered () =
    
    let aRef = createQuadtree { Origin = (0,0,0); Size = (2,2); Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}
    
    let a = aRef.TryGetInMemory().Value
    Assert.ThrowsAny<Exception>(fun () ->
        a.SplitCenteredNodeIntoQuadrantNodesAtSameLevel() |> ignore
        )

[<Fact>]
let SplitCenteredNodeIntoQuadrantNodesAtSameLevel () =

    let aRef = createQuadtree { Origin = (-1,-1,0); Size = (2,2); Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}

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

    createQuadtree { Origin = (0,0,0); Size = (2,2); Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}

    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0); ((1,0,0), 2.0)
                    ((0,1,0), 3.0); ((1,1,0), 4.0)
            ]}
        ]}

[<Fact>]
let ``CreateLeaf_Centered`` () =

    createQuadtree { Origin = (-1,-1,0); Size = (2,2); Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}

    |> checkQuadtree {
        Cell = Cell2d(8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((-1,-1,0), 1.0); (( 0,-1,0), 2.0)
                    ((-1, 0,0), 3.0); (( 0, 0,0), 4.0)
            ]}
        ]}

(************************************************************************************
    extendUpTo
 ************************************************************************************)

[<Fact>]
let ``ExtendUpTo_NonCentered_To_NonCentered`` () =

    createQuadtree { Origin = (0,0,0); Size = (2,2); Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}

    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0); ((1,0,0), 2.0)
                    ((0,1,0), 3.0); ((1,1,0), 4.0)
            ]}
        ]}

     
    |> QNode.extendUpTo (Cell2d(0,0,9)) 
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0)
                    ((1,0,0), 2.0)
                    ((0,1,0), 3.0)
                    ((1,1,0), 4.0)
            ]}
            {
                Level = 1; Data = [
                    ((0,0,1), 2.5)
            ]}
        ]}

[<Fact>]
let ``ExtendUpTo_NonCentered_To_NonCentered_2_Steps`` () =

    createQuadtree { Origin = (0,0,0); Size = (4,4); Data = [|
        1.0;  2.0;  3.0;  4.0;
        5.0;  6.0;  7.0;  8.0;
        9.0; 10.0; 11.0; 12.0;
       13.0; 14.0; 15.0; 16.0;
    |]}

    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0),  1.0); ((1,0,0),  2.0); ((2,0,0),  3.0); ((3,0,0),  4.0)
                    ((0,1,0),  5.0); ((1,1,0),  6.0); ((2,1,0),  7.0); ((3,1,0),  8.0)
                    ((0,2,0),  9.0); ((1,2,0), 10.0); ((2,2,0), 11.0); ((3,2,0), 12.0)
                    ((0,3,0), 13.0); ((1,3,0), 14.0); ((2,3,0), 15.0); ((3,3,0), 16.0)
            ]}
        ]}

    |> QNode.extendUpTo (Cell2d(0,0,10))
    |> checkQuadtree {
        Cell = Cell2d(0,0,10); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 2; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0
                Data = [
                    let mutable k = 0
                    for j = 0 to 3 do
                        for i = 0 to 3 do
                            yield (((i,j,0), float (k+1)))
                            k <- k + 1
            ]}
            {
                Level = 1; Data = [
                    ((0,0,1),  3.5); ((1,0,1),  5.5)
                    ((0,1,1), 11.5); ((1,1,1), 13.5)
            ]}
            {
                Level = 2; Data = [
                    ((0,0,2),  8.5)
            ]}
        ]}

[<Fact>]
let ``ExtendUpTo_NonCentered_To_Centered`` () =

    createQuadtree { Origin = (0,0,0); Size = (2,2); Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}

    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
        ]}

    |> QNode.extendUpTo (Cell2d(9))

    |> checkQuadtree {
        Cell = Cell2d(9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0
                Data = [
                    ((0,0,0), 1.0); ((1,0,0), 2.0)
                    ((0,1,0), 3.0); ((1,1,0), 4.0)
            ]}
            {
                Level = 1
                Data = [
                    ((0,0,1), 2.5)
            ]}
        ]}


[<Fact>]
let ``ExtendUpTo_Centered_To_Centered`` () =

    createQuadtree { Origin = (-1,-1,0); Size = (2,2); Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}

    |> checkQuadtree {
        Cell = Cell2d(8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
        ]}

    |> QNode.extendUpTo (Cell2d(9))

    |> checkQuadtree {
        Cell = Cell2d(9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((-1,-1,0), 1.0); (( 0,-1,0), 2.0)
                    ((-1, 0,0), 3.0); (( 0, 0,0), 4.0)
            ]}
            {
                Level = 1; Data = [
                    // centered parents again have all 4 samples, but with sample exponent + 1
                    ((-1,-1,1), 1.0); (( 0,-1,1), 2.0)
                    ((-1, 0,1), 3.0); (( 0, 0,1), 4.0)
            ]}
        ]}


(************************************************************************************
    leaf/leaf merges
 ************************************************************************************)

[<Fact>]
let ``Merge_NoNodes`` () =

    let aRef = createQuadtree { Origin = (0,0,0); Size = (2,2); Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}
    let bRef = createQuadtree { Origin = (0,0,0); Size = (2,2); Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
    |]}

    match Quadtree.Merge NoNode NoNode MoreDetailedDominates with
    | NoNode -> Assert.True(true)
    | _      -> Assert.True(false)

    match (Quadtree.Merge aRef NoNode MoreDetailedDominates).TryGetInMemory() with
    | None   -> Assert.True(false)
    | Some n -> Assert.True(n.Id = aRef.Id)

    match (Quadtree.Merge NoNode bRef MoreDetailedDominates).TryGetInMemory() with
    | None   -> Assert.True(false)
    | Some n -> Assert.True(n.Id = bRef.Id)

    ()

[<Fact>]
let ``MergeSingleSamples_SameLeaf_SamePosition`` () =

    let aRef = createQuadtree { Origin = (0,0,0); Size = (1,1); Data = [|
        1.0;
    |]}
    let bRef = createQuadtree { Origin = (0,0,0); Size = (1,1); Data = [|
        -1.0;
    |]}

    // first dominates
    Quadtree.Merge aRef bRef FirstDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0)
            ]}
        ]}
    |> ignore

    // second dominates
    Quadtree.Merge aRef bRef SecondDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), -1.0)
            ]}
        ]}
    |> ignore

[<Fact>]
let ``MergeSingleSamples_SameLeaf_AdjacentPosition`` () =

    let aRef = createQuadtree { Origin = (0,0,0); Size = (1,1); Data = [|
        1.0;
    |]}
    let bRef = createQuadtree { Origin = (1,0,0); Size = (1,1); Data = [|
        -1.0;
    |]}

    // first dominates
    Quadtree.Merge aRef bRef FirstDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0); ((1,0,0), -1.0)
            ]}
        ]}
    |> ignore
        
    // second dominates
    Quadtree.Merge aRef bRef SecondDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0); ((1,0,0), -1.0)
            ]}
        ]}
    |> ignore

[<Fact>]
let ``MergeSingleSamples_SameLeaf_OneSampleUndefinedBetween`` () =

    let aRef = createQuadtree { Origin = (0,0,0); Size = (1,1); Data = [|
        1.0;
    |]}
    let bRef = createQuadtree { Origin = (2,0,0); Size = (1,1); Data = [|
        -1.0;
    |]}

    // first dominates
    Quadtree.Merge aRef bRef FirstDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0); ((1,0,0), 0.0); ((2,0,0), -1.0)
            ]}
        ]}
    |> ignore
        
    // second dominates
    Quadtree.Merge aRef bRef SecondDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0); ((1,0,0), 0.0); ((2,0,0), -1.0)
            ]}
        ]}
    |> ignore

[<Fact>]
let ``MergePerfectlyAlignedLeafs_Centered`` () =
    
    let aRef = createQuadtree { Origin = (-1,-1,0); Size = (2,2); Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}
    let bRef = createQuadtree { Origin = (-1,-1,0); Size = (2,2); Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
    |]}

    // first dominates
    Quadtree.Merge aRef bRef FirstDominates
    |> checkQuadtree {
        Cell = Cell2d(8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((-1,-1,0), 1.0); (( 0,-1,0), 2.0)
                    ((-1, 0,0), 3.0); (( 0, 0,0), 4.0)
            ]}
        ]}
    |> ignore

    // second dominates
    Quadtree.Merge aRef bRef SecondDominates
    |> checkQuadtree {
        Cell = Cell2d(8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((-1,-1,0), -1.0); (( 0,-1,0), -2.0)
                    ((-1, 0,0), -3.0); (( 0, 0,0), -4.0)
            ]}
        ]}
    |> ignore

[<Fact>]
let ``MergePerfectlyAlignedLeafs_NonCentered`` () =

    let aRef = createQuadtree { Origin = (0,0,0); Size = (2,2); Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}
    let bRef = createQuadtree { Origin = (0,0,0); Size = (2,2); Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
    |]}

    // first dominates
    Quadtree.Merge aRef bRef FirstDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0); ((1,0,0), 2.0)
                    ((0,1,0), 3.0); ((1,1,0), 4.0)
            ]}
        ]}
    |> ignore

    // second dominates
    Quadtree.Merge aRef bRef SecondDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), -1.0); ((1,0,0), -2.0)
                    ((0,1,0), -3.0); ((1,1,0), -4.0)
            ]}
        ]}
    |> ignore

[<Fact>]
let ``MergeLeafs_Adjacent_DifferentDepth`` () =
    
    let aRef = createQuadtree { Origin = (0,0,1); Size = (1,1); Data = [|
        1.0;
    |]}
    let bRef = createQuadtree { Origin = (2,0,0); Size = (2,2); Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
    |]}


    let samples = [
        {
        Level = 0; Data = [
            ((0,0,1), 1.0);
            ((2,0,0), -1.0); ((3,0,0), -2.0)
            ((2,1,0), -3.0); ((3,1,0), -4.0)
        ]}
        {
            Level = 1; Data = [
                ((0,0,1), 1.0);
                ((1,0,1), -2.5);
            ]}
        ]

    // first dominates
    Quadtree.Merge aRef bRef FirstDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
        Samples = samples
        }
    |> ignore

    // second dominates
    Quadtree.Merge aRef bRef SecondDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
        Samples = samples
        }
    |> ignore


(************************************************************************************
    leaf/tree merges
 ************************************************************************************)

[<Fact>]
let ``Merge_Leaf_Tree_SamplesPerfectlyOverlap`` () =

    let aRef = createQuadtree { Origin = (0,0,1); Size = (1,1); Data = [|
        1.0;
    |]}
    let bRef = QNode.extendUpTo (Cell2d(0,0,1)) <| createQuadtree { Origin = (0,0,0); Size = (2,2); Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
        |]}
        

    // first dominates
    Quadtree.Merge aRef bRef FirstDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = true; OriginalSampleExponent = 1; SampleExponent = 1; SplitLimitExponent = 9
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0)
            ]}
        ]}
    |> ignore

    // second dominates
    Quadtree.Merge aRef bRef SecondDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), -1.0); ((1,0,0), -2.0)
                    ((0,1,0), -3.0); ((1,1,0), -4.0)
            ]}
            {
                Level = 1; Data = [
                    ((0,0,1), -2.5)
            ]}
        ]}
    |> ignore

[<Fact>]
let ``Merge_Leaf_Tree_SamplesReplaceOneQuadrant`` () =

    let aRef = createQuadtree { Origin = (0,0,1); Size = (2,2); Data = [|
        1.0; 2.0
        3.0; 4.0
    |]}
    let bRef = QNode.extendUpTo (Cell2d(0,0,2)) <| createQuadtree { Origin = (2,0,0); Size = (2,2); Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
        |]}
        

    // first dominates
    Quadtree.Merge aRef bRef FirstDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = true; OriginalSampleExponent = 1; SampleExponent = 1; SplitLimitExponent = 9
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,1), 1.0); ((1,0,1), 2.0);
                    ((0,1,1), 3.0); ((1,1,1), 4.0);
            ]}
            {
                Level = 1; Data = [
                    ((0,0,1), 1.0); ((1,0,1), 2.0);
                    ((0,1,1), 3.0); ((1,1,1), 4.0);
            ]}
            {
                Level = 2; Data = [
                    ((0,0,2), 2.5);
            ]}
        ]}
    |> ignore

    // second dominates
    Quadtree.Merge aRef bRef SecondDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,1), 1.0); ((2,0,0), -1.0); ((3,0,0), -2.0);
                                    ((2,1,0), -3.0); ((3,1,0), -4.0);
                    ((0,1,1), 3.0); ((1,1,1), 4.0);
            ]}
            {
                Level = 1; Data = [
                    ((0,0,1), 1.0); ((0,0,1), 2.5);
                    ((0,1,1), 3.0); ((1,1,1), 4.0);
            ]}
            {
                Level = 2; Data = [
                    ((0,0,2), 2.625);
            ]}
        ]}
    |> ignore
