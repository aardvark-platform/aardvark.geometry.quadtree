module StructureTests

open Aardvark.Geometry.Quadtree
open Aardvark.Base
open System
open Xunit

type QuadtreeSpec = {
    Origin : Cell2d
    Size : (int*int)
    Data : float[]
    Split : int
    }

let createQuadtree spec =
    let (w, h) = spec.Size
    let mapping = DataMapping(spec.Origin, V2i(w,h))
    let layer = Layer(Defs.Heights1d, spec.Data, mapping)
    let config = { BuildConfig.Default with SplitLimitPowerOfTwo = spec.Split }
    Quadtree.Build config [| layer |]

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
let ``SplitCenteredNodeIntoQuadrantNodesAtSameLevel fails for non-centered nodes`` () =
    
    let aRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}
    
    let a = aRef.TryGetInMemory().Value
    Assert.ThrowsAny<Exception>(fun () ->
        a.SplitCenteredNodeIntoQuadrantNodesAtSameLevel() |> ignore
        )

[<Fact>]
let ``SplitCenteredNodeIntoQuadrantNodesAtSameLevel`` () =

    let aRef = createQuadtree { Origin = Cell2d(-1,-1,0); Size = (2,2); Split = 8; Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}

    let a = aRef.TryGetInMemory().Value
    let ls = a.SplitCenteredNodeIntoQuadrantNodesAtSameLevel()
    Assert.True(ls.Length = 4)

    ls.[0] |> InMemoryNode |> checkQuadtree {
        Cell = Cell2d(-1,-1,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [ { Level = 0; Data = [ ((-1,-1,0), 1.0) ]} ]
        } |> ignore
    Assert.True(ls.[0].SampleWindow = Box2l(V2l(-1,-1), V2l(0,0)))

    ls.[1] |> InMemoryNode |> checkQuadtree {
        Cell = Cell2d(0,-1,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [ { Level = 0; Data = [ (( 0,-1,0), 2.0) ]} ]
        } |> ignore
    Assert.True(ls.[1].SampleWindow = Box2l(V2l( 0,-1), V2l(1,0)))

    ls.[2] |> InMemoryNode |> checkQuadtree {
        Cell = Cell2d(-1,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [ { Level = 0; Data = [ ((-1, 0,0), 3.0) ]} ]
        } |> ignore
    Assert.True(ls.[2].SampleWindow = Box2l(V2l(-1, 0), V2l(0,1)))

    ls.[3] |> InMemoryNode |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = [ { Level = 0; Data = [ (( 0, 0,0), 4.0) ]} ]
        } |> ignore
    Assert.True(ls.[3].SampleWindow = Box2l(V2l( 0, 0), V2l(1,1)))


(************************************************************************************
    create leafs
 ************************************************************************************)

[<Fact>]
let ``leaf non-centered 1`` () =
 
    Assert.ThrowsAny<Exception>(fun () ->
        createQuadtree { Origin = Cell2d(0); Size = (1,1); Split = 8; Data = [|
            1.0
        |]} |> ignore
        )

[<Fact>]
let ``leaf non-centered 2`` () =

    createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
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
let ``leaf centered`` () =

    createQuadtree { Origin = Cell2d(-1,-1,0); Size = (2,2); Split = 8; Data = [|
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
let ``extendUpTo: non-centered -> non-centered`` () =

    createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
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
let ``extendUpTo: non-centered -> non-centered, 2 levels difference`` () =

    createQuadtree { Origin = Cell2d(0,0,0); Size = (4,4); Split = 8; Data = [|
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
let ``extendUpTo: non-centered -> centered`` () =

    createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
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

    |> ignore

[<Fact>]
let ``extendUpTo: centered -> centered`` () =

    Assert.ThrowsAny<Exception>(fun () ->
        createQuadtree { Origin = Cell2d(-1,-1,0); Size = (2,2); Split = 8; Data = [|
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

        |> ignore
    )

(************************************************************************************
    leaf/leaf merges
 ************************************************************************************)

[<Fact>]
let ``merge: NoNode`` () =

    let aRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}
    let bRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
    |]}

    match Quadtree.Merge MoreDetailedDominates NoNode NoNode with
    | NoNode -> Assert.True(true)
    | _      -> Assert.True(false)

    match (Quadtree.Merge MoreDetailedDominates aRef NoNode).TryGetInMemory() with
    | None   -> Assert.True(false)
    | Some n -> Assert.True(n.Id = aRef.Id)

    match (Quadtree.Merge MoreDetailedDominates NoNode bRef).TryGetInMemory() with
    | None   -> Assert.True(false)
    | Some n -> Assert.True(n.Id = bRef.Id)

    ()

[<Fact>]
let ``merge: leaf 1x1 / leaf 1x1, same leaf, same sample`` () =

    let aRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (1,1); Split = 8; Data = [|
        1.0;
    |]}
    let bRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (1,1); Split = 8; Data = [|
        -1.0;
    |]}

    // first dominates
    Quadtree.Merge FirstDominates aRef bRef
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
    Quadtree.Merge SecondDominates aRef bRef
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
let ``merge: leaf 1x1 / leaf 1x1, same leaf, adjacent sample`` () =

    let aRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (1,1); Split = 8; Data = [|
        1.0;
    |]}
    let bRef = createQuadtree { Origin = Cell2d(1,0,0); Size = (1,1); Split = 8; Data = [|
        -1.0;
    |]}

    // first dominates
    Quadtree.Merge FirstDominates aRef bRef 
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
    Quadtree.Merge SecondDominates aRef bRef
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
let ``merge: leaf 1x1 / leaf 1x1, same leaf, 1 undefined sample between`` () =

    let aRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (1,1); Split = 8; Data = [|
        1.0;
    |]}
    let bRef = createQuadtree { Origin = Cell2d(2,0,0); Size = (1,1); Split = 8; Data = [|
        -1.0;
    |]}

    // first dominates
    Quadtree.Merge FirstDominates aRef bRef
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
    Quadtree.Merge SecondDominates aRef bRef
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
let ``merge: leaf 2x2 / leaf 2x2, same leaf, same samples`` () =

    let aRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}
    let bRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
    |]}

    // first dominates
    Quadtree.Merge FirstDominates aRef bRef
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
    Quadtree.Merge SecondDominates aRef bRef
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
let ``merge: leaf 2x2 / leaf 2x2, same leaf, same samples, cross-origin`` () =
    
    let aRef = createQuadtree { Origin = Cell2d(-1,-1,0); Size = (2,2); Split = 8; Data = [|
        1.0;  2.0; 
        3.0;  4.0;
    |]}
    let bRef = createQuadtree { Origin = Cell2d(-1,-1,0); Size = (2,2); Split = 8; Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
    |]}

    // first dominates
    Quadtree.Merge FirstDominates aRef bRef
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
    Quadtree.Merge SecondDominates aRef bRef
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
let ``merge: leaf 2x2 / leaf 2x2, adjacent leafs`` () =
    
    let aRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
        1.0; 2.0;
        3.0; 4.0;
    |]}
    let bRef = createQuadtree { Origin = Cell2d(0,2,0); Size = (2,2); Split = 8; Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
    |]}

    let samples = [
        {
            Level = 0; Data = [
                ((0,0,0),  1.0); ((1,0,0),  2.0)
                ((0,1,0),  3.0); ((1,1,0),  4.0)
                ((0,2,0), -1.0); ((1,2,0), -2.0)
                ((0,3,0), -3.0); ((1,3,0), -4.0)
        ]}
    ]

    // first dominates
    Quadtree.Merge FirstDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = samples
        }
    |> ignore

    // second dominates
    Quadtree.Merge SecondDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 0; SplitLimitExponent = 8
        Samples = samples
        }
    |> ignore

[<Fact>]
let ``merge: leaf 2x2 / leaf 2x2, adjacent leafs, cross-origin`` () =
    
    let aRef = createQuadtree { Origin = Cell2d(-2,0,0); Size = (2,2); Split = 8; Data = [|
        1.0; 2.0;
        3.0; 4.0;
    |]}
    let bRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
    |]}

    let samples = [
        {
            Level = 1; Data = [
                ((-1,0,1), 2.5); ((0,0,1), -2.5)
        ]}
        {
            Level = 0; Data = [
                ((-2,0,0), 1.0); ((-1,0,0), 2.0);   (( 0,0,0), -1.0); (( 1,0,0), -2.0)
                ((-2,1,0), 3.0); ((-1,1,0), 4.0);   (( 0,1,0), -3.0); (( 1,1,0), -4.0)
        ]}
    ]

    // first dominates
    Quadtree.Merge FirstDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
        Samples = samples
        }
    |> ignore

    // second dominates
    Quadtree.Merge SecondDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
        Samples = samples
        }
    |> ignore

[<Fact>]
let ``merge: leaf 1x1 / leaf 2x2, adjacent leafs, different depth`` () =
    
    let aRef = createQuadtree { Origin = Cell2d(0,0,1); Size = (1,1); Split = 8; Data = [|
        1.0;
    |]}
    let bRef = createQuadtree { Origin = Cell2d(2,0,0); Size = (2,2); Split = 8; Data = [|
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
    Quadtree.Merge FirstDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
        Samples = samples
        }
    |> ignore

    // second dominates
    Quadtree.Merge SecondDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
        Samples = samples
        }
    |> ignore

(************************************************************************************
    leaf/tree merges
 ************************************************************************************)

[<Fact>]
let ``merge: leaf 1x1 / tree 2x2, samples perfectly overlap`` () =

    let aRef = createQuadtree { Origin = Cell2d(0,0,1); Size = (1,1); Split = 8; Data = [|
        1.0;
    |]}
    let bRef = 
        createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
            -1.0;  -2.0; 
            -3.0;  -4.0;
            |]}
        |> QNode.extendUpTo (Cell2d(0,0,9)) 
        

    // first dominates
    Quadtree.Merge FirstDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = true; OriginalSampleExponent = 1; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,1), 1.0)
            ]}
        ]}
    |> ignore

    // second dominates
    Quadtree.Merge SecondDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
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
let ``merge: leaf 1x1 / tree 2x2, adjacent`` () =

    let aRef = createQuadtree { Origin = Cell2d(0,0,1); Size = (1,1); Split = 8; Data = [|
        1.0;
    |]}
    let bRef =
        createQuadtree { Origin = Cell2d(2,0,0); Size = (2,2); Split = 8; Data = [|
            -1.0;  -2.0; 
            -3.0;  -4.0;
            |]}
        |> QNode.extendUpTo (Cell2d(0,0,9))
        

    // first dominates
    Quadtree.Merge FirstDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,1),  1.0)
                    ((2,0,0), -1.0); ((3,0,0), -2.0)
                    ((2,1,0), -3.0); ((3,1,0), -4.0)
            ]}
            {
                Level = 1; Data = [
                    ((0,0,1),  1.0)
                    ((1,0,1), -2.5)
            ]}
        ]}
    |> ignore

    // second dominates
    Quadtree.Merge SecondDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = false; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,1),  1.0)
                    ((2,0,0), -1.0); ((3,0,0), -2.0)
                    ((2,1,0), -3.0); ((3,1,0), -4.0)
            ]}
            {
                Level = 1; Data = [
                    ((0,0,1),  1.0)
                    ((1,0,1), -2.5)
            ]}
        ]}
    |> ignore

[<Fact>]
let ``merge: leaf 2x2 L1 / leaf 2x2 L-1, replace, fully overlapping, 2 levels`` () =

    let a = createQuadtree { Origin = Cell2d(0,0, 1); Size = (2,2); Split = 8; Data = [|
        1.0; 2.0
        3.0; 4.0
        |]}
    let c = createQuadtree { Origin = Cell2d(2,0,-1); Size = (2,2); Split = 8; Data = [|
        91.0; 92.0; 
        33.0; 94.0;
        |]}

    Quadtree.Merge FirstDominates a c
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = true; OriginalSampleExponent = 1; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0)
            ]}
        ]}
    |> ignore

[<Fact>]
let ``merge: leaf 2x2 L1 / leaf 2x2 L0 / leaf 2x2 L-1, replace quadrant, 2 levels`` () =

    let a = createQuadtree { Origin = Cell2d(0,0, 1); Size = (2,2); Split = 8; Data = [|
        1.0; 2.0
        3.0; 4.0
        |]}
    let b = createQuadtree { Origin = Cell2d(2,0, 0); Size = (2,2); Split = 8; Data = [|
        -1.0;  -2.0; 
        -3.0;  -4.0;
        |]}
    let c = createQuadtree { Origin = Cell2d(4,0,-1); Size = (2,2); Split = 8; Data = [|
        91.0; 92.0; 
        93.0; 94.0;
        |]}

    /// (x d1 y) d2 z
    let merge x y z d1 d2 = Quadtree.Merge d2 (Quadtree.Merge d1 x y) z

    merge a b c FirstDominates FirstDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = true; OriginalSampleExponent = 1; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,1), 1.0); ((1,0,1), 2.0)
                    ((0,1,1), 3.0); ((1,1,1), 4.0)
            ]}
        ]}
    |> ignore

    merge a b c FirstDominates SecondDominates
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = false; OriginalSampleExponent = -1; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,0), 1.0)
            ]}
        ]}
    |> ignore

    merge a b c SecondDominates FirstDominates
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

    merge a b c SecondDominates SecondDominates
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

    // merge a c b FirstDominates FirstDominates
    // merge a c b FirstDominates SecondDominates
    // merge a c b SecondDominates FirstDominates
    // merge a c b SecondDominates SecondDominates

    // merge b a c FirstDominates FirstDominates
    // merge b a c FirstDominates SecondDominates
    // merge b a c SecondDominates FirstDominates
    // merge b a c SecondDominates SecondDominates

    // merge b c a FirstDominates FirstDominates
    // merge b c a FirstDominates SecondDominates
    // merge b c a SecondDominates FirstDominates
    // merge b c a SecondDominates SecondDominates
    
    // merge c a b FirstDominates FirstDominates
    // merge c a b FirstDominates SecondDominates
    // merge c a b SecondDominates FirstDominates
    // merge c a b SecondDominates SecondDominates

    // merge c b a FirstDominates FirstDominates
    // merge c b a FirstDominates SecondDominates
    // merge c b a SecondDominates FirstDominates
    // merge c b a SecondDominates SecondDominates

[<Fact>]
let ``merge: leaf 2x2 L1 / tree 2x2 L0, samples replace 1 quadrant`` () =

    let aRef = createQuadtree { Origin = Cell2d(0,0,1); Size = (2,2); Split = 8; Data = [|
        1.0; 2.0
        3.0; 4.0
    |]}
    let bRef =
        createQuadtree { Origin = Cell2d(2,0,0); Size = (2,2); Split = 8; Data = [|
            -1.0;  -2.0; 
            -3.0;  -4.0;
            |]}
        |> QNode.extendUpTo (Cell2d(0,0,9))
        
    Quadtree.Merge FirstDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = true; OriginalSampleExponent = 1; SampleExponent = 1; SplitLimitExponent = 8
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
        ]}
    |> ignore

    Quadtree.Merge SecondDominates bRef aRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = true; OriginalSampleExponent = 1; SampleExponent = 1; SplitLimitExponent = 8
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
        ]}
    |> ignore

    Quadtree.Merge SecondDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = false; OriginalSampleExponent = -1; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = 0; Data = [
                    ((0,0,1), 1.0); ((2,0,0), -1.0); ((3,0,0), -2.0);
                                    ((2,1,0), -3.0); ((3,1,0), -4.0);
                    ((0,1,1), 3.0); ((1,1,1), 4.0);
            ]}
            {
                Level = 1; Data = [
                    ((0,0,1), 1.0); ((1,0,1),-2.5);
                    ((0,1,1), 3.0); ((1,1,1), 4.0);
            ]}
        ]}
    |> ignore

[<Fact>]
let ``merge: leaf 8x8 L-1 / tree 2x2 L0, samples replace 1 quadrant`` () =

    let aRef = createQuadtree { Origin = Cell2d(0,0,-1); Size = (8,8); Split = 8; Data = [|
        for x = 1 to 64 do yield 1.0
    |]}
    let bRef =
        createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
            -1.0;  -2.0; 
            -3.0;  -4.0;
            |]}
        |> QNode.extendUpTo (Cell2d(0,0,9))
        
    Quadtree.Merge FirstDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = true; OriginalSampleExponent = 1; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = -1; Data = [
                    for j = 0 to 7 do for i = 0 to 7 do yield ((i,j,-1), 1.0)
                    yield ((0,0,0), -1.0); yield ((1,0,0), -2.0);
                    yield ((0,1,0), -3.0); yield ((1,1,0), -4.0);
            ]}
        ]}
    |> ignore

    Quadtree.Merge SecondDominates bRef aRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,9); IsLeafNode = true; OriginalSampleExponent = 1; SampleExponent = 1; SplitLimitExponent = 8
        Samples = [
            {
                Level = -1; Data = [
                    for j = 0 to 7 do for i = 0 to 7 do yield ((i,j,-1), 1.0)
                    yield ((0,0,0), -1.0); yield ((1,0,0), -2.0);
                    yield ((0,1,0), -3.0); yield ((1,1,0), -4.0);
            ]}
        ]}
    |> ignore

    Quadtree.Merge SecondDominates aRef bRef
    |> checkQuadtree {
        Cell = Cell2d(0,0,8); IsLeafNode = true; OriginalSampleExponent = 0; SampleExponent = 1; SplitLimitExponent = 8
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