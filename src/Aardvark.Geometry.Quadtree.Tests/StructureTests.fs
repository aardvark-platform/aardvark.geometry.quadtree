namespace Aardvark.Geometry.Quadtree.Tests

open Aardvark.Geometry.Quadtree
open Aardvark.Geometry.Quadtree.PrettyPrint
open Aardvark.Base
open System
open Xunit

#nowarn "3560"

module StructureTests =

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

    type QuadtreeSpecTyped<'a> = {
        Origin : Cell2d
        Size : (int*int)
        Data : 'a[]
        Split : int
        }

    let createQuadtreeTyped<'a when 'a : equality> def spec =
        let (w, h) = spec.Size
        let mapping = DataMapping(spec.Origin, V2i(w,h))
        let layer = Layer<'a>(def, spec.Data, mapping)
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
        SampleExponent : int
        SplitLimitExponent : int
        Samples : QuadtreeCheckSamples seq
    }

    let checkQuadtree spec (rootRef : QNodeRef) =

        Assert.True(spec.Cell                   = rootRef.Cell)
        Assert.True(spec.IsLeafNode             = rootRef.IsLeafNode)
        Assert.True(spec.SampleExponent         = rootRef.Cell.Exponent - rootRef.SplitLimitExponent)
        Assert.True(spec.SplitLimitExponent     = rootRef.SplitLimitExponent)

        for s in spec.Samples do
            let map = s.Data |> Map.ofSeq
            let samples = getAllSamplesAtLevel s.Level rootRef
            Assert.True(map.Count = samples.Length)
            for (cell, value) in samples do
                let k = (int cell.X, int cell.Y, cell.Exponent)
                let s = map |> Map.find k
                if s.IsNaN() then 
                    value.IsNaN() |> Assert.True
                else
                    s = value     |> Assert.True

        rootRef
    
    (************************************************************************************
        ExactBoundingBox
     ************************************************************************************)

    [<Fact>]
    let ``boundingbox: single e=0`` () =
 
         let a = createQuadtree { Origin = Cell2d(1,2,0); Size = (7,14); Split = 8; Data = [|
             for i=0 to 7*14 do yield 1.0
         |]}
 
         a.ExactBoundingBox = Box2d(V2d(1,2), V2d(8, 16)) |> Assert.True

    [<Fact>]
    let ``boundingbox: single e=1`` () =
 
         let a = createQuadtree { Origin = Cell2d(1,2,1); Size = (7,14); Split = 8; Data = [|
             for i=0 to 7*14 do yield 1.0
         |]}
 
         a.ExactBoundingBox = Box2d(V2d(2,4), V2d(16, 32)) |> Assert.True

    [<Fact>]
    let ``boundingbox: merged e=0 e=0 contained`` () =
 
         let a = createQuadtree { Origin = Cell2d(1,2,0); Size = (7,14); Split = 8; Data = [|
             for i=0 to 7*14 do yield 1.0
         |]}

         let b = createQuadtree { Origin = Cell2d(3,8,0); Size = (3,2); Split = 8; Data = [|
             for i=0 to 3*2 do yield 1.0
         |]}

         let m1 = Quadtree.Merge FirstDominates a b
         m1.ExactBoundingBox = Box2d(V2d(1,2), V2d(8, 16)) |> Assert.True

         let m2 = Quadtree.Merge SecondDominates a b
         m2.ExactBoundingBox = Box2d(V2d(1,2), V2d(8, 16)) |> Assert.True

    [<Fact>]
    let ``boundingbox: merged e=0 e=-2 contained`` () =
 
         let a = createQuadtree { Origin = Cell2d(0,0,0); Size = (7,14); Split = 8; Data = [|
             for i=0 to 7*14 do yield 1.0
         |]}

         let b = createQuadtree { Origin = Cell2d(3,8,-2); Size = (3,2); Split = 8; Data = [|
             for i=0 to 3*2 do yield 1.0
         |]}

         let m1 = Quadtree.Merge FirstDominates a b
         m1.ExactBoundingBox = Box2d(V2d(0,0), V2d(7, 14)) |> Assert.True

         let m2 = Quadtree.Merge SecondDominates a b
         m2.ExactBoundingBox = Box2d(V2d(0,0), V2d(7, 14)) |> Assert.True

    [<Fact>]
    let ``boundingbox: merged e=0 e=0 partial overlap`` () =
 
         let a = createQuadtree { Origin = Cell2d(1,2,0); Size = (7,14); Split = 8; Data = [|
             for i=0 to 7*14 do yield 1.0
         |]}

         let b = createQuadtree { Origin = Cell2d(5,1,0); Size = (5,3); Split = 8; Data = [|
             for i=0 to 5*3 do yield 1.0
         |]}

         let m1 = Quadtree.Merge FirstDominates a b
         m1.ExactBoundingBox = Box2d(V2d(1,1), V2d(10, 16)) |> Assert.True

         let m2 = Quadtree.Merge SecondDominates a b
         m2.ExactBoundingBox = Box2d(V2d(1,1), V2d(10, 16)) |> Assert.True

    [<Fact>]
    let ``boundingbox: merged e=0 e=-2 partial overlap`` () =
 
         let a = createQuadtree { Origin = Cell2d(1,2,0); Size = (7,14); Split = 8; Data = [|
             for i=0 to 7*14 do yield 1.0
         |]}

         let b = createQuadtree { Origin = Cell2d(5,2,-2); Size = (5,3); Split = 8; Data = [|
             for i=0 to 5*3 do yield 1.0
         |]}

         let m1 = Quadtree.Merge FirstDominates a b
         m1.ExactBoundingBox = Box2d(V2d(1.0,0.5), V2d(8, 16)) |> Assert.True

         let m2 = Quadtree.Merge SecondDominates a b
         m2.ExactBoundingBox = Box2d(V2d(1.0,0.5), V2d(8, 16)) |> Assert.True

    [<Fact>]
    let ``boundingbox: merged e=0 e=0 adjacent`` () =
 
         let a = createQuadtree { Origin = Cell2d(1,2,0); Size = (7,14); Split = 8; Data = [|
             for i=0 to 7*14 do yield 1.0
         |]}

         let b = createQuadtree { Origin = Cell2d(8,4,0); Size = (5,3); Split = 8; Data = [|
             for i=0 to 5*3 do yield 1.0
         |]}

         let m1 = Quadtree.Merge FirstDominates a b
         m1.ExactBoundingBox = Box2d(V2d(1,2), V2d(13, 16)) |> Assert.True

         let m2 = Quadtree.Merge SecondDominates a b
         m2.ExactBoundingBox = Box2d(V2d(1,2), V2d(13, 16)) |> Assert.True

    [<Fact>]
    let ``boundingbox: merged e=0 e=-2 adjacent`` () =
 
         let a = createQuadtree { Origin = Cell2d(0,0,0); Size = (1,1); Split = 8; Data = [|
             for i=0 to 1*1 do yield 10.0
         |]}

         let b = createQuadtree { Origin = Cell2d(4,0,-2); Size = (4,4); Split = 8; Data = [|
             for i=0 to 4*4 do yield 20.0
         |]}

         let m1 = Quadtree.Merge FirstDominates a b
         let m2 = Quadtree.Merge SecondDominates a b

         m1.ExactBoundingBox = Box2d(V2d(0,0), V2d(2,1)) |> Assert.True
         m2.ExactBoundingBox = Box2d(V2d(0,0), V2d(2,1)) |> Assert.True

    [<Fact>]
    let ``boundingbox: merged e=0 e=0 islands`` () =
 
         let a = createQuadtree { Origin = Cell2d(7,11,0); Size = (1,1); Split = 8; Data = [| 1.0|]}

         let b = createQuadtree { Origin = Cell2d(3,5,0); Size = (1,1); Split = 8; Data = [| 1.0 |]}

         let m1 = Quadtree.Merge FirstDominates a b
         m1.ExactBoundingBox = Box2d(V2d(3,5), V2d(8, 12)) |> Assert.True

         let m2 = Quadtree.Merge SecondDominates a b
         m2.ExactBoundingBox = Box2d(V2d(3,5), V2d(8, 12)) |> Assert.True

    [<Fact>]
    let ``boundingbox: merged e=0 e=-2 islands`` () =
 
         let a = createQuadtree { Origin = Cell2d(7,11,0); Size = (1,1); Split = 8; Data = [| 1.0|]}

         let b = createQuadtree { Origin = Cell2d(3,5,-2); Size = (1,1); Split = 8; Data = [| 1.0|]}

         let m1 = Quadtree.Merge FirstDominates a b
         m1.ExactBoundingBox = Box2d(V2d(0.75,1.25), V2d(8, 12)) |> Assert.True

         let m2 = Quadtree.Merge SecondDominates a b
         m2.ExactBoundingBox = Box2d(V2d(0.75,1.25), V2d(8, 12)) |> Assert.True

    (************************************************************************************
        SplitCenteredNodeIntoQuadrantNodesAtSameLevel
     ************************************************************************************)

    //[<Fact>]
    //let ``SplitCenteredNodeIntoQuadrantNodesAtSameLevel fails for non-centered nodes`` () =
    
    //    let aRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
    //        1.0;  2.0; 
    //        3.0;  4.0;
    //    |]}
    
    //    let a = aRef.TryGetInMemory().Value
    //    Assert.ThrowsAny<Exception>(fun () ->
    //        a.SplitCenteredNodeIntoQuadrantNodesAtSameLevel() |> ignore
    //        )

    //[<Fact>]
    //let ``SplitCenteredNodeIntoQuadrantNodesAtSameLevel`` () =

    //    let aRef = createQuadtree { Origin = Cell2d(-1,-1,0); Size = (2,2); Split = 8; Data = [|
    //        1.0;  2.0; 
    //        3.0;  4.0;
    //    |]}

    //    let a = aRef.TryGetInMemory().Value
    //    let ls = a.SplitCenteredNodeIntoQuadrantNodesAtSameLevel()
    //    Assert.True(ls.Length = 4)

    //    ls.[0].Value |> InMemoryNode |> checkQuadtree {
    //        Cell = Cell2d(-1,-1,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
    //        Samples = [ { Level = 0; Data = [ ((-1,-1,0), 1.0) ]} ]
    //        } |> ignore
    //    Assert.True(ls.[0].Value.SampleWindow = Box2l(V2l(-1,-1), V2l(0,0)))

    //    ls.[1].Value |> InMemoryNode |> checkQuadtree {
    //        Cell = Cell2d(0,-1,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
    //        Samples = [ { Level = 0; Data = [ (( 0,-1,0), 2.0) ]} ]
    //        } |> ignore
    //    Assert.True(ls.[1].Value.SampleWindow = Box2l(V2l( 0,-1), V2l(1,0)))

    //    ls.[2].Value |> InMemoryNode |> checkQuadtree {
    //        Cell = Cell2d(-1,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
    //        Samples = [ { Level = 0; Data = [ ((-1, 0,0), 3.0) ]} ]
    //        } |> ignore
    //    Assert.True(ls.[2].Value.SampleWindow = Box2l(V2l(-1, 0), V2l(0,1)))

    //    ls.[3].Value |> InMemoryNode |> checkQuadtree {
    //        Cell = Cell2d(0,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
    //        Samples = [ { Level = 0; Data = [ (( 0, 0,0), 4.0) ]} ]
    //        } |> ignore
    //    Assert.True(ls.[3].Value.SampleWindow = Box2l(V2l( 0, 0), V2l(1,1)))


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
            Cell = Cell2d(0,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
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
            Cell = Cell2d(8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
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

    //[<Fact>]
    //let ``extendUpTo: non-centered -> non-centered`` () =

    //    createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
    //        1.0;  2.0; 
    //        3.0;  4.0;
    //    |]}

    //    |> checkQuadtree {
    //        Cell = Cell2d(0,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
    //        Samples = [
    //            {
    //                Level = 0; Data = [
    //                    ((0,0,0), 1.0); ((1,0,0), 2.0)
    //                    ((0,1,0), 3.0); ((1,1,0), 4.0)
    //            ]}
    //        ]}

     
    //    |> QNode.extendUpTo (Cell2d(0,0,9)) 
    //    |> checkQuadtree {
    //        Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
    //        Samples = [
    //            {
    //                Level = 0; Data = [
    //                    ((0,0,0), 1.0)
    //                    ((1,0,0), 2.0)
    //                    ((0,1,0), 3.0)
    //                    ((1,1,0), 4.0)
    //            ]}
    //            {
    //                Level = 1; Data = [
    //                    ((0,0,1), 2.5)
    //            ]}
    //        ]}

    //[<Fact>]
    //let ``extendUpTo: non-centered -> non-centered, 2 levels difference`` () =

    //    createQuadtree { Origin = Cell2d(0,0,0); Size = (4,4); Split = 8; Data = [|
    //        1.0;  2.0;  3.0;  4.0;
    //        5.0;  6.0;  7.0;  8.0;
    //        9.0; 10.0; 11.0; 12.0;
    //       13.0; 14.0; 15.0; 16.0;
    //    |]}

    //    |> checkQuadtree {
    //        Cell = Cell2d(0,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
    //        Samples = [
    //            {
    //                Level = 0; Data = [
    //                    ((0,0,0),  1.0); ((1,0,0),  2.0); ((2,0,0),  3.0); ((3,0,0),  4.0)
    //                    ((0,1,0),  5.0); ((1,1,0),  6.0); ((2,1,0),  7.0); ((3,1,0),  8.0)
    //                    ((0,2,0),  9.0); ((1,2,0), 10.0); ((2,2,0), 11.0); ((3,2,0), 12.0)
    //                    ((0,3,0), 13.0); ((1,3,0), 14.0); ((2,3,0), 15.0); ((3,3,0), 16.0)
    //            ]}
    //        ]}

    //    |> QNode.extendUpTo (Cell2d(0,0,10))
    //    |> checkQuadtree {
    //        Cell = Cell2d(0,0,10); IsLeafNode = false; SampleExponent = 2; SplitLimitExponent = 8
    //        Samples = [
    //            {
    //                Level = 0
    //                Data = [
    //                    let mutable k = 0
    //                    for j = 0 to 3 do
    //                        for i = 0 to 3 do
    //                            yield (((i,j,0), float (k+1)))
    //                            k <- k + 1
    //            ]}
    //            {
    //                Level = 1; Data = [
    //                    ((0,0,1),  3.5); ((1,0,1),  5.5)
    //                    ((0,1,1), 11.5); ((1,1,1), 13.5)
    //            ]}
    //            {
    //                Level = 2; Data = [
    //                    ((0,0,2),  8.5)
    //            ]}
    //        ]}

    //[<Fact>]
    //let ``extendUpTo: non-centered -> centered`` () =

    //    createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
    //        1.0;  2.0; 
    //        3.0;  4.0;
    //    |]}

    //    |> checkQuadtree {
    //        Cell = Cell2d(0,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
    //        Samples = [
    //        ]}

    //    |> QNode.extendUpTo (Cell2d(9))

    //    |> checkQuadtree {
    //        Cell = Cell2d(9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
    //        Samples = [
    //            {
    //                Level = 0
    //                Data = [
    //                    ((0,0,0), 1.0); ((1,0,0), 2.0)
    //                    ((0,1,0), 3.0); ((1,1,0), 4.0)
    //            ]}
    //            {
    //                Level = 1
    //                Data = [
    //                    ((0,0,1), 2.5)
    //            ]}
    //        ]}

    //    |> ignore

    //[<Fact>]
    //let ``extendUpTo: centered -> centered`` () =

    //    Assert.ThrowsAny<Exception>(fun () ->
    //        createQuadtree { Origin = Cell2d(-1,-1,0); Size = (2,2); Split = 8; Data = [|
    //            1.0;  2.0; 
    //            3.0;  4.0;
    //        |]}

    //        |> checkQuadtree {
    //            Cell = Cell2d(8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
    //            Samples = [
    //            ]}

    //        |> QNode.extendUpTo (Cell2d(9))

    //        |> checkQuadtree {
    //            Cell = Cell2d(9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
    //            Samples = [
    //                {
    //                    Level = 0; Data = [
    //                        ((-1,-1,0), 1.0); (( 0,-1,0), 2.0)
    //                        ((-1, 0,0), 3.0); (( 0, 0,0), 4.0)
    //                ]}
    //                {
    //                    Level = 1; Data = [
    //                        // centered parents again have all 4 samples, but with sample exponent + 1
    //                        ((-1,-1,1), 1.0); (( 0,-1,1), 2.0)
    //                        ((-1, 0,1), 3.0); (( 0, 0,1), 4.0)
    //                ]}
    //            ]}

    //        |> ignore
    //    )

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

        match Quadtree.Merge FirstDominates NoNode NoNode with
        | NoNode -> Assert.True(true)
        | _      -> Assert.True(false)

        match Quadtree.Merge SecondDominates NoNode NoNode with
        | NoNode -> Assert.True(true)
        | _      -> Assert.True(false)

        (Quadtree.Merge FirstDominates  aRef NoNode).Id = aRef.Id   |> Assert.True

        (Quadtree.Merge SecondDominates aRef NoNode).Id = aRef.Id   |> Assert.True

        (Quadtree.Merge FirstDominates  NoNode bRef).Id = bRef.Id   |> Assert.True

        (Quadtree.Merge SecondDominates NoNode bRef).Id = bRef.Id   |> Assert.True

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
            Cell = Cell2d(0,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
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
            Cell = Cell2d(0,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
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
            Cell = Cell2d(0,0,8); IsLeafNode = false; SampleExponent = 0; SplitLimitExponent = 8
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
            Cell = Cell2d(0,0,8); IsLeafNode = false; SampleExponent = 0; SplitLimitExponent = 8
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
            Cell = Cell2d(0,0,8); IsLeafNode = false; SampleExponent = 0; SplitLimitExponent = 8
            Samples = [
                {
                    Level = 0; Data = [
                        ((0,0,0), 1.0);                     ((2,0,0), -1.0)
                ]}
            ]}
        |> ignore
        
        // second dominates
        Quadtree.Merge SecondDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,8); IsLeafNode = false; SampleExponent = 0; SplitLimitExponent = 8
            Samples = [
                {
                    Level = 0; Data = [
                        ((0,0,0), 1.0);                     ((2,0,0), -1.0)
                ]}
            ]}
        |> ignore




    let generateDefaultsTest def s0 s1 checkNan =

        let aRef = createQuadtreeTyped<_> def { Origin = Cell2d(0,0,0); Size = (1,1); Split = 8; Data = [| s0 |]}
        let bRef = createQuadtreeTyped<_> def { Origin = Cell2d(2,0,0); Size = (1,1); Split = 8; Data = [| s1 |]}

        let m = Quadtree.Merge FirstDominates aRef bRef

        let xs = m |> Query.All Query.Config.Default |> Seq.map (fun x -> x.GetSamples<_>(def)) |> Seq.concat |> Seq.toArray
        let map = xs |> Array.map (fun (c, x) -> ((int c.X, int c.Y, c.Exponent), x)) |> Map.ofArray

        let x0 = Map.find (0,0,0) map
        let x1 = Map.tryFind (1,0,0) map
        let x2 = Map.find (2,0,0) map

        x0 = s0     |> Assert.True
        x2 = s1     |> Assert.True
        if x1.IsSome then checkNan x1.Value |> Assert.True

    [<Fact>]
    let ``defaults: C3b Black   Colors3b`` ()          = generateDefaultsTest Defs.Colors3b C3b.Red C3b.Green (fun x -> x = C3b.Black)
    [<Fact>]
    let ``defaults: C4b Black   Colors4b`` ()          = generateDefaultsTest Defs.Colors4b C4b.Red C4b.Green (fun x -> x = C4b.Black)
    [<Fact>]
    let ``defaults: C3f Black   Colors3f`` ()          = generateDefaultsTest Defs.Colors3f C3f.Red C3f.Green (fun x -> x = C3f.Black)
    [<Fact>]
    let ``defaults: C4f Black   Colors4f`` ()          = generateDefaultsTest Defs.Colors4f C4f.Red C4f.Green (fun x -> x = C4f.Black)

    [<Fact>]
    let ``defaults: V3d NaN     Normals3d`` ()         = generateDefaultsTest Defs.Normals3d V3d.IOO V3d.IOO (fun x -> x.IsNaN)
    [<Fact>]
    let ``defaults: V3f NaN     Normals3f`` ()         = generateDefaultsTest Defs.Normals3f V3f.IOO V3f.IOO (fun x -> x.IsNaN)

    [<Fact>]
    let ``defaults: V4d NaN     HeightsBilinear4d`` () = generateDefaultsTest Defs.HeightsBilinear4d V4d.IOOO V4d.OOOI (fun x -> x.IsNaN)
    [<Fact>]
    let ``defaults: V4f NaN     HeightsBilinear4f`` () = generateDefaultsTest Defs.HeightsBilinear4f V4f.IOOO V4f.OOOI (fun x -> x.IsNaN)

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
            Cell = Cell2d(0,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
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
            Cell = Cell2d(0,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
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
            Cell = Cell2d(8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
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
            Cell = Cell2d(8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
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
            Cell = Cell2d(0,0,8); IsLeafNode = false; SampleExponent = 0; SplitLimitExponent = 8
            Samples = samples
            }
        |> ignore

        // second dominates
        Quadtree.Merge SecondDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,8); IsLeafNode = false; SampleExponent = 0; SplitLimitExponent = 8
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
            //{
            //    Level = 1; Data = [
            //        ((-1,0,1), 2.5); ((0,0,1), -2.5)
            //]}
            {
                Level = 0; Data = [
                    ((-2,0,0), 1.0); ((-1,0,0), 2.0);   (( 0,0,0), -1.0); (( 1,0,0), -2.0)
                    ((-2,1,0), 3.0); ((-1,1,0), 4.0);   (( 0,1,0), -3.0); (( 1,1,0), -4.0)
            ]}
        ]

        // first dominates
        Quadtree.Merge FirstDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
            Samples = samples
            }
        |> ignore

        // second dominates
        Quadtree.Merge SecondDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
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
            //{
            //    Level = 1; Data = [
            //        ((0,0,1), 1.0);     ((1,0,1), -2.5);
            //]}
            {
                Level = 0; Data = [
                    ((0,0,1), 1.0);                     ((2,0,0), -1.0); ((3,0,0), -2.0);
                                                        ((2,1,0), -3.0); ((3,1,0), -4.0);
            ]}
        ]

        // first dominates
        let m = Quadtree.Merge FirstDominates aRef bRef
        m
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
            Samples = samples
            }
        |> ignore

        // second dominates
        Quadtree.Merge SecondDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
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
        let bRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
            -1.0;  -2.0; 
            -3.0;  -4.0;
            |]}
        

        // first dominates
        Quadtree.Merge FirstDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = true; SampleExponent = 1; SplitLimitExponent = 8
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
            Cell = Cell2d(0,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
            Samples = [
                {
                    Level = 0; Data = [
                        ((0,0,0), -1.0); ((1,0,0), -2.0)
                        ((0,1,0), -3.0); ((1,1,0), -4.0)
                ]}
                {
                    Level = 1; Data = [
                        ((0,0,0), -1.0); ((1,0,0), -2.0)
                        ((0,1,0), -3.0); ((1,1,0), -4.0)
                ]}
            ]}
        |> ignore

    [<Fact>]
    let ``merge: leaf 1x1 / leaf 2x2, adjacent`` () =

        let aRef = createQuadtree { Origin = Cell2d(0,0,1); Size = (1,1); Split = 8; Data = [|
            1.0;
        |]}
        let bRef = createQuadtree { Origin = Cell2d(2,0,0); Size = (2,2); Split = 8; Data = [|
            -1.0;  -2.0; 
            -3.0;  -4.0;
            |]}
        

        // first dominates
        let m = Quadtree.Merge FirstDominates aRef bRef
        m
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = 0; Data = [
                        ((0,0,1),  1.0);                    ((2,0,0), -1.0); ((3,0,0), -2.0);
                                                            ((2,1,0), -3.0); ((3,1,0), -4.0);
                ]}
                //{
                //    Level = 1; Data = [
                //        ((0,0,1),  1.0)
                //        ((1,0,1), -2.5)
                //]}
            ]}
        |> ignore

        // second dominates
        let m = Quadtree.Merge SecondDominates aRef bRef
        m
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = 0; Data = [
                        ((0,0,1),  1.0);                    ((2,0,0), -1.0); ((3,0,0), -2.0);
                                                            ((2,1,0), -3.0); ((3,1,0), -4.0);
                ]}
                //{
                //    Level = 1; Data = [
                //        ((0,0,1),  1.0)
                //        ((1,0,1), -2.5)
                //]}
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
            93.0; 94.0;
            |]}

        let m = Quadtree.Merge FirstDominates a c
        m
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = true; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = 1; Data = [
                        ((0,0,1), 1.0); ((1,0,1), 2.0);
                        ((0,1,1), 3.0); ((1,1,1), 4.0);
                ]}
            ]}
        |> ignore

        let m = Quadtree.Merge SecondDominates a c
    
        //showHtmlDebugView<float> "merge: leaf 2x2 L1 / leaf 2x2 L-1, replace, fully overlapping, 2 levels" Defs.Heights1d [
        //    ("a", a)
        //    ("c", c)
        //    ("m = Quadtree.Merge SecondDominates a c",  m)
        //    ]

        m
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                //{
                //    Level = 1; Data = [
                //        ((0,0,1), 23.875); ((1,0,1), 2.0);
                //        ((0,1,1),  3.000); ((1,1,1), 4.0);
                //]}
                //{
                //    Level = 0; Data = [
                //        ((0,0,0), 23.875); ((1,0,0), 92.500);   ((1,0,1), 2.0);
                //        ((0,1,0), 23.875); ((1,1,0), 23.875);   

                //        ((0,1,1),  3.000);                      ((1,1,1), 4.0);
                //]}
                {
                    Level = -1; Data = [
                        ((0,0,0), 1.0);  ((2,0,-1),91.0);((3,0,-1),92.0);    ((1,0,1), 2.0);
                                         ((2,1,-1),93.0);((3,1,-1),94.0);    
                        ((0,1,0), 1.0);  ((1,1, 0), 1.0);    
                    
                        ((0,1,1), 3.0);                                      ((1,1,1), 4.0);
                ]}
            ]}
        |> ignore


    /// (x d1 y) d2 z
    let private merge x y z d1 d2 = Quadtree.Merge d2 (Quadtree.Merge d1 x y) z

    [<Fact>]
    let ``sm 2020-12-07`` () =

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

        let m1 = Quadtree.Merge SecondDominates a b
        let m2 = Quadtree.Merge SecondDominates m1 c

        //showHtmlDebugView<float> "merge: leaf 2x2 L1 / leaf 2x2 L0 / leaf 2x2 L-1, replace quadrant, 2 levels" Defs.Heights1d [
        //    ("a", a)
        //    ("b", b)
        //    ("c", c)
        //    ("m1 = Quadtree.Merge SecondDominates a b",  m1)
        //    ("m2 = Quadtree.Merge SecondDominates m1 c", m2)
        //    ]

        m2
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                //{
                //    Level = 1; Data = [
                //        ((0,0,1), 1.0);     ((1,0,1), 20.875)

                //        ((0,1,1), 3.0);     ((1,1,1),  4.0)
                //]}
                //{
                //    Level = 0; Data = [
                //        ((0,0,1), 1.0);                     ((2,0,0), 92.5); ((3,0,0), -2.0);
                //                                            ((2,1,0), -3.0); ((3,1,0), -4.0);
                    
                //        ((0,1,1), 3.0);                     ((1,1,1),  4.000);

                //]}
                {
                    Level = -1; Data = [
                        ((0,0,1), 1.0);                     ((4,0,-1), 91.0);((5,0,-1), 92.0);  ((3,0,0), -2.0);
                                                            ((4,1,-1), 93.0);((5,1,-1), 94.0);
                                                            ((2,1,0), -3.0);                    ((3,1,0), -4.0);
                                                        
                        ((0,1,1), 3.0);                     ((1,1,1),  4.000);
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

        merge a b c FirstDominates FirstDominates
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = true; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = 0; Data = [
                        ((0,0,1), 1.0); ((1,0,1), 2.0)
                        ((0,1,1), 3.0); ((1,1,1), 4.0)
                ]}
            ]}
        |> ignore

        let m = Quadtree.Merge SecondDominates a c
        m
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = -1; Data = [
                        ((0,0,1), 1.0);                     ((4,0,-1), 91.0); ((5,0,-1), 92.0);     ((3,0,0), 2.0);
                                                            ((4,1,-1), 93.0); ((5,1,-1), 94.0);

                                                            ((2,1, 0),  2.0);                       ((3,1,0), 2.0);



                        ((0,1,1), 3.0);                     ((1,1, 1),  4.0);

                ]}
            ]}
        |> ignore

        let m = merge a b c FirstDominates SecondDominates
        m
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = -1; Data = [
                        ((0,0,1), 1.0);                     ((4,0,-1), 91.0); ((5,0,-1), 92.0);     ((3,0,0), 2.0);
                                                            ((4,1,-1), 93.0); ((5,1,-1), 94.0);

                                                            ((2,1, 0),  2.0);                       ((3,1,0), 2.0);



                        ((0,1,1), 3.0);                     ((1,1, 1),  4.0);

                ]}
            ]}
        |> ignore

        merge a b c SecondDominates FirstDominates
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = 0; Data = [
                        ((0,0,1), 1.0);                     ((2,0,0), -1.0); ((3,0,0), -2.0);
                                                            ((2,1,0), -3.0); ((3,1,0), -4.0);
                    
                        ((0,1,1), 3.0);                     ((1,1,1),  4.000);
                ]}
                //{
                //    Level = 1; Data = [
                //        ((0,0,1), 1.0);     ((1,0,1),-2.5);
                //        ((0,1,1), 3.0);     ((1,1,1), 4.0);
                //]}
            ]}
        |> ignore

        let m = merge a b c SecondDominates SecondDominates
    
        //showHtmlDebugView<float> "merge: leaf 2x2 L1 / leaf 2x2 L0 / leaf 2x2 L-1, replace quadrant, 2 levels" Defs.Heights1d [
        //    ("a", a)
        //    ("b", b)
        //    ("c", c)
        //    ("m = merge a b c SecondDominates SecondDominates", m)
        //    ]

        m
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                //{
                //    Level = 1; Data = [
                //        ((0,0,1), 1.0);     ((1,0,1), 20.875)

                //        ((0,1,1), 3.0);     ((1,1,1),  4.0)
                //]}
                //{
                //    Level = 0; Data = [
                //        ((0,0,1), 1.0);                     ((2,0,0), 92.5); ((3,0,0), -2.0);
                //                                            ((2,1,0), -3.0); ((3,1,0), -4.0);
                    
                //        ((0,1,1), 3.0);                     ((1,1,1),  4.000);

                //]}
                {
                    Level = -1; Data = [
                        ((0,0,1), 1.0);                     ((4,0,-1), 91.0);((5,0,-1), 92.0);  ((3,0,0), -2.0);
                                                            ((4,1,-1), 93.0);((5,1,-1), 94.0);
                                                            ((2,1,0), -3.0);                    ((3,1,0), -4.0);
                                                        
                        ((0,1,1), 3.0);                     ((1,1,1),  4.000);
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
        let bRef = createQuadtree { Origin = Cell2d(2,0,0); Size = (2,2); Split = 8; Data = [|
            -1.0;  -2.0; 
            -3.0;  -4.0;
            |]}
        
        Quadtree.Merge FirstDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = true; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = 0; Data = [
                        ((0,0,1), 1.0); ((1,0,1), 2.0);
                        ((0,1,1), 3.0); ((1,1,1), 4.0);
                ]}
            ]}
        |> ignore

        Quadtree.Merge SecondDominates bRef aRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = true; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = 0; Data = [
                        ((0,0,1), 1.0); ((1,0,1), 2.0);
                        ((0,1,1), 3.0); ((1,1,1), 4.0);
                ]}
                //{
                //    Level = 1; Data = [
                //        ((0,0,1), 1.0); ((1,0,1), 2.0);
                //        ((0,1,1), 3.0); ((1,1,1), 4.0);
                //]}
            ]}
        |> ignore

        Quadtree.Merge SecondDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = 0; Data = [
                        ((0,0,1), 1.0);                     ((2,0,0), -1.0); ((3,0,0), -2.0);
                                                            ((2,1,0), -3.0); ((3,1,0), -4.0);

                        ((0,1,1), 3.0);                     ((1,1,1),  4.0);
                ]}
                //{
                //    Level = 1; Data = [
                //        ((0,0,1), 1.0);                     ((1,0,1),-2.5);

                //        ((0,1,1), 3.0);                     ((1,1,1), 4.0);
                //]}
            ]}
        |> ignore


    [<Fact>]
    let ``merge: leaf 2x2 L-1 / leaf 1x1 L0, sample replaces all`` () =

        let aRef = createQuadtree { Origin = Cell2d(0,0,-1); Size = (2,2); Split = 8; Data = [|
             1.0;  2.0;
             3.0;  4.0; 
        |]}
        let bRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (1,1); Split = 8; Data = [|
            -50.0;
        |]}
        
        Quadtree.Merge FirstDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,7); IsLeafNode = true; SampleExponent = -1; SplitLimitExponent = 8
            Samples = [
                //{
                //    Level = 0; Data = [
                //        ((0,0,0), 2.5);
                //]}
                {
                    Level = -1; Data = [
                        ((0,0,-1),  1.0); ((1,0,-1),  2.0);
                        ((0,1,-1),  3.0); ((1,1,-1),  4.0);
                ]}
            ]}
        |> ignore

        Quadtree.Merge SecondDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,8); IsLeafNode = true; SampleExponent = 0; SplitLimitExponent = 8
            Samples = [
                //{
                //    Level = 0; Data = [
                //        ((0,0,0), -50.0); 
                //]}
                {
                    Level = -1; Data = [
                        ((0,0,0), -50.0); 
                ]}
            ]}
        |> ignore

    [<Fact>]
    let ``merge: leaf 4x4 L-1 / leaf 1x1 L0, samples replace 1 quadrant`` () =

        let aRef = createQuadtree { Origin = Cell2d(0,0,-1); Size = (4,4); Split = 8; Data = [|
             1.0;  2.0;  3.0;  4.0;
             5.0;  6.0;  7.0;  8.0;
             9.0; 10.0; 11.0; 12.0;
            13.0; 14.0; 15.0; 16.0;
        |]}
        let bRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (1,1); Split = 8; Data = [|
            50.0;
        |]}
        
        let m = Quadtree.Merge FirstDominates aRef bRef
        m
        |> checkQuadtree {
            Cell = Cell2d(0,0,7); IsLeafNode = true; SampleExponent = -1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = -1; Data = [
                        ((0,0,-1),  1.0); ((1,0,-1),  2.0);     ((2,0,-1),  3.0); ((3,0,-1),  4.0);
                        ((0,1,-1),  5.0); ((1,1,-1),  6.0);     ((2,1,-1),  7.0); ((3,1,-1),  8.0);

                        ((0,2,-1),  9.0); ((1,2,-1), 10.0);     ((2,2,-1), 11.0); ((3,2,-1), 12.0);
                        ((0,3,-1), 13.0); ((1,3,-1), 14.0);     ((2,3,-1), 15.0); ((3,3,-1), 16.0);
                ]}
            ]}
        |> ignore
    
        let m = Quadtree.Merge SecondDominates aRef bRef
    
        //showHtmlDebugView<float> "merge: leaf 4x4 L-1 / leaf 1x1 L0, samples replace 1 quadrant" Defs.Heights1d [
        //    ("aRef", aRef)
        //    ("bRef", bRef)
        //    ("m = Quadtree.Merge SecondDominates aRef bRef",  m)
        //    ]

        m
        |> checkQuadtree {
            Cell = Cell2d(0,0,8); IsLeafNode = false; SampleExponent = 0; SplitLimitExponent = 8
            Samples = [
                //{
                //    Level = 0; Data = [
                //        ((0,0,0), 50.0); ((1,0,0),  5.5);
                //        ((0,1,0), 11.5); ((1,1,0), 13.5);
                //]}
                {
                    Level = -1; Data = [
                        ((0,0, 0), 50.0);                       ((2,0,-1),  3.0); ((3,0,-1),  4.0);
                                                                ((2,1,-1),  7.0); ((3,1,-1),  8.0);

                        ((0,2,-1),  9.0); ((1,2,-1), 10.0);     ((2,2,-1), 11.0); ((3,2,-1), 12.0);
                        ((0,3,-1), 13.0); ((1,3,-1), 14.0);     ((2,3,-1), 15.0); ((3,3,-1), 16.0);
                ]}
            ]}
        |> ignore

    //[<Fact>]
    //let ``merge: leaf 4x4 L-1 / tree 1x1 L0, samples replace 1 quadrant`` () =

    //    let aRef = createQuadtree { Origin = Cell2d(0,0,-1); Size = (4,4); Split = 8; Data = [|
    //         1.0;  2.0;  3.0;  4.0;
    //         5.0;  6.0;  7.0;  8.0;
    //         9.0; 10.0; 11.0; 12.0;
    //        13.0; 14.0; 15.0; 16.0;
    //    |]}
    //    let bRef0 = createQuadtree { Origin = Cell2d(0,0,0); Size = (1,1); Split = 8; Data = [|
    //        50.0;
    //    |]}
    //    let bRef = bRef0 |> QNode.extendUpTo (bRef0.Cell.Parent)

    //    Quadtree.Merge FirstDominates aRef bRef
    //    |> checkQuadtree {
    //        Cell = Cell2d(0,0,7); IsLeafNode = true; SampleExponent = -1; SplitLimitExponent = 8
    //        Samples = [
    //            {
    //                Level = -1; Data = [
    //                    ((0,0,-1),  1.0); ((1,0,-1),  2.0); ((2,0,-1),  3.0); ((3,0,-1),  4.0);
    //                    ((0,1,-1),  5.0); ((1,1,-1),  6.0); ((2,1,-1),  7.0); ((3,1,-1),  8.0);
    //                    ((0,2,-1),  9.0); ((1,2,-1), 10.0); ((2,2,-1), 11.0); ((3,2,-1), 12.0);
    //                    ((0,3,-1), 13.0); ((1,3,-1), 14.0); ((2,3,-1), 15.0); ((3,3,-1), 16.0);
    //            ]}
    //        ]}
    //    |> ignore
    
    //    Quadtree.Merge SecondDominates aRef bRef
    //    |> checkQuadtree {
    //        Cell = Cell2d(0,0,9); IsLeafNode = false; SampleExponent = 1; SplitLimitExponent = 8
    //        Samples = [
    //            {
    //                Level = 1; Data = [
    //                    ((0,0,1), 20.125);
    //            ]}
    //            {
    //                Level = 0; Data = [
    //                    ((0,0,0), 50.0); ((1,0,0),  5.5);
    //                    ((0,1,0), 11.5); ((1,1,0), 13.5);
    //            ]}
    //            {
    //                Level = -1; Data = [
    //                    ((0,0,-1), 50.0); ((1,0,-1), 50.0); ((2,0,-1),  3.0); ((3,0,-1),  4.0);
    //                    ((0,1,-1), 50.0); ((1,1,-1), 50.0); ((2,1,-1),  7.0); ((3,1,-1),  8.0);
    //                    ((0,2,-1),  9.0); ((1,2,-1), 10.0); ((2,2,-1), 11.0); ((3,2,-1), 12.0);
    //                    ((0,3,-1), 13.0); ((1,3,-1), 14.0); ((2,3,-1), 15.0); ((3,3,-1), 16.0);
    //            ]}
    //        ]}
    //    |> ignore

    [<Fact>]
    let ``merge: leaf 8x8 L-1 / tree 2x2 L0, samples replace 1 quadrant`` () =

        let aRef = createQuadtree { Origin = Cell2d(0,0,-1); Size = (8,8); Split = 8; Data = [|
            for x = 1 to 64 do yield 1.0
        |]}
        let bRef = createQuadtree { Origin = Cell2d(0,0,0); Size = (2,2); Split = 8; Data = [|
            -1.0;  -2.0; 
            -3.0;  -4.0;
            |]}
        
        Quadtree.Merge FirstDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,7); IsLeafNode = true; SampleExponent = -1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = -1; Data = [
                        for j = 0 to 7 do for i = 0 to 7 do yield ((i,j,-1), 1.0)
                ]}
            ]}
        |> ignore
    
        Quadtree.Merge SecondDominates aRef bRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,8); IsLeafNode = false; SampleExponent = 0; SplitLimitExponent = 8
            Samples = [
                {
                    Level = -1; Data = [
                        for j = 0 to 7 do for i = 0 to 7 do if i >= 4 || j >= 4 then yield ((i,j,-1), 1.0)

                        yield ((0,0,0), -1.0);     yield ((1,0,0), -2.0); 
                        yield ((0,1,0), -3.0);     yield ((1,1,0), -4.0); 
                ]}
            ]}
        |> ignore

        Quadtree.Merge FirstDominates bRef aRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,8); IsLeafNode = false; SampleExponent = 0; SplitLimitExponent = 8
            Samples = [
                {
                    Level = -1; Data = [
                        for j = 0 to 7 do for i = 0 to 7 do if i >= 4 || j >= 4 then yield ((i,j,-1), 1.0)

                        yield ((0,0,0), -1.0);     yield ((1,0,0), -2.0); 
                        yield ((0,1,0), -3.0);     yield ((1,1,0), -4.0); 
                ]}
            ]}
        |> ignore

        Quadtree.Merge SecondDominates bRef aRef
        |> checkQuadtree {
            Cell = Cell2d(0,0,7); IsLeafNode = true; SampleExponent = -1; SplitLimitExponent = 8
            Samples = [
                {
                    Level = -1; Data = [
                        for j = 0 to 7 do for i = 0 to 7 do yield ((i,j,-1), 1.0)
                ]}
            ]}
        |> ignore
